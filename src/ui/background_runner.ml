(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

(* Background runner backed by an Eio stream. Producers enqueue tasks from any
   thread; a dedicated Eio runtime hosts worker fibers that execute tasks in
   separate domains to keep the UI responsive. *)

type task = {fn : unit -> unit; enqueued_at : float}

let stream : task Eio.Stream.t Lazy.t = lazy (Eio.Stream.create 1024)

let started = Atomic.make false

let num_workers = 4

let queue_depth = Atomic.make 0

let rec worker domain_mgr stream =
  let task = Eio.Stream.take stream in
  let depth_after_take = Atomic.fetch_and_add queue_depth (-1) - 1 in
  let wait_ms = max 0. (Unix.gettimeofday () -. task.enqueued_at) *. 1000. in
  Metrics.record_bg_dequeue ~queued_depth:depth_after_take ~wait_ms ;
  (try Eio.Domain_manager.run domain_mgr task.fn with _ -> ()) ;
  worker domain_mgr stream

let start () =
  if Atomic.compare_and_set started false true then
    ignore
      (Domain.spawn (fun () ->
           (* Use POSIX backend to avoid io_uring resource exhaustion *)
           Eio_posix.run (fun env ->
               let stream = Lazy.force stream in
               Eio.Switch.run (fun sw ->
                   for _ = 1 to num_workers do
                     Eio.Fiber.fork ~sw (fun () -> worker env#domain_mgr stream)
                   done ;
                   (* Keep the switch alive indefinitely. *)
                   Eio.Fiber.await_cancel ()))))

let default_enqueue fn =
  start () ;
  let stream = Lazy.force stream in
  let task = {fn; enqueued_at = Unix.gettimeofday ()} in
  let depth_after_enqueue = Atomic.fetch_and_add queue_depth 1 + 1 in
  Metrics.record_bg_enqueue ~queued_depth:depth_after_enqueue ;
  try Eio.Stream.add stream task with _ -> ()

let enqueue_ref : ((unit -> unit) -> unit) Atomic.t =
  Atomic.make default_enqueue

let enqueue fn = (Atomic.get enqueue_ref) fn

let submit_blocking ?on_complete f =
  let task () =
    Fun.protect ~finally:(fun () -> Option.iter (fun g -> g ()) on_complete) f
  in
  enqueue task

module For_tests = struct
  let with_synchronous_runner f =
    let original = Atomic.get enqueue_ref in
    Atomic.set enqueue_ref (fun fn -> fn ()) ;
    Fun.protect ~finally:(fun () -> Atomic.set enqueue_ref original) f
end
