(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** A single-threaded worker queue with request deduplication.

    Requests are identified by a string key. If a request with the same key
    is already pending, the new request is dropped. This prevents piling up
    duplicate work when the worker is busy. *)

type 'a request = {key : string; work : unit -> 'a; on_complete : 'a -> unit}

(** Metrics for a worker queue *)
type metrics = {
  mutable requests_total : int;
  mutable requests_deduped : int;
  mutable execution_times : float list;  (** Recent execution times in ms *)
  deduped_by_key : (string, int) Hashtbl.t;  (** Dedup count per key *)
}

let max_timing_samples = 100

type 'a t = {
  name : string;
  queue : 'a request Queue.t;
  pending : (string, unit) Hashtbl.t;
  lock : Mutex.t;
  cond : Condition.t;
  mutable running : bool;
  metrics : metrics;
}

let create ~name () =
  {
    name;
    queue = Queue.create ();
    pending = Hashtbl.create 17;
    lock = Mutex.create ();
    cond = Condition.create ();
    running = false;
    metrics =
      {
        requests_total = 0;
        requests_deduped = 0;
        execution_times = [];
        deduped_by_key = Hashtbl.create 17;
      };
  }

(** Submit a request. Returns true if queued, false if deduplicated. *)
let submit t ~key ~work ~on_complete =
  Mutex.lock t.lock ;
  t.metrics.requests_total <- t.metrics.requests_total + 1 ;
  let queued =
    if Hashtbl.mem t.pending key then (
      t.metrics.requests_deduped <- t.metrics.requests_deduped + 1 ;
      let prev =
        Hashtbl.find_opt t.metrics.deduped_by_key key |> Option.value ~default:0
      in
      Hashtbl.replace t.metrics.deduped_by_key key (prev + 1) ;
      false)
    else (
      Hashtbl.add t.pending key () ;
      Queue.add {key; work; on_complete} t.queue ;
      Condition.signal t.cond ;
      true)
  in
  Mutex.unlock t.lock ;
  queued

(** Submit without callback *)
let submit_unit t ~key ~work =
  submit t ~key ~work ~on_complete:(fun () -> ()) |> ignore

let worker_loop t =
  while t.running do
    Mutex.lock t.lock ;
    while Queue.is_empty t.queue && t.running do
      Condition.wait t.cond t.lock
    done ;
    let req_opt =
      if t.running && not (Queue.is_empty t.queue) then Some (Queue.pop t.queue)
      else None
    in
    Mutex.unlock t.lock ;
    match req_opt with
    | None -> ()
    | Some req -> (
        let start_time = Unix.gettimeofday () in
        let result = try req.work () with _ -> Obj.magic () in
        let elapsed_ms = (Unix.gettimeofday () -. start_time) *. 1000.0 in
        Mutex.lock t.lock ;
        Hashtbl.remove t.pending req.key ;
        (* Record timing, keep only recent samples *)
        t.metrics.execution_times <-
          (if List.length t.metrics.execution_times >= max_timing_samples then
             elapsed_ms
             :: List.rev (List.tl (List.rev t.metrics.execution_times))
           else elapsed_ms :: t.metrics.execution_times) ;
        Mutex.unlock t.lock ;
        try req.on_complete result with _ -> ())
  done

let start t =
  if not t.running then (
    t.running <- true ;
    ignore (Domain.spawn (fun () -> worker_loop t)))

let stop t =
  Mutex.lock t.lock ;
  t.running <- false ;
  Condition.broadcast t.cond ;
  Mutex.unlock t.lock

(** Compute percentile from sorted list. p is 0.0-1.0 *)
let percentile_of_sorted arr p =
  let n = Array.length arr in
  if n = 0 then 0.0
  else
    let idx = int_of_float (Float.floor (float_of_int (n - 1) *. p)) in
    arr.(idx)

type key_dedup = {key : string; count : int}

type stats = {
  name : string;
  requests_total : int;
  requests_deduped : int;
  deduped_by_key : key_dedup list;
      (** Per-key dedup counts, sorted by count desc *)
  p50_ms : float;
  p90_ms : float;
  p95_ms : float;
  p99_ms : float;
}

let get_stats t =
  Mutex.lock t.lock ;
  let total = t.metrics.requests_total in
  let deduped = t.metrics.requests_deduped in
  let times = t.metrics.execution_times in
  let by_key =
    Hashtbl.fold
      (fun key count acc -> {key; count} :: acc)
      t.metrics.deduped_by_key
      []
  in
  Mutex.unlock t.lock ;
  let sorted = Array.of_list times in
  Array.sort Float.compare sorted ;
  (* Sort by count descending *)
  let by_key_sorted =
    List.sort (fun a b -> Int.compare b.count a.count) by_key
  in
  {
    name = t.name;
    requests_total = total;
    requests_deduped = deduped;
    deduped_by_key = by_key_sorted;
    p50_ms = percentile_of_sorted sorted 0.5;
    p90_ms = percentile_of_sorted sorted 0.9;
    p95_ms = percentile_of_sorted sorted 0.95;
    p99_ms = percentile_of_sorted sorted 0.99;
  }
