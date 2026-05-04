(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Background scheduler for polling octez-index status. *)

open Octez_manager_lib

let refresh_interval = 6.0

let last_refresh : (string, float) Hashtbl.t = Hashtbl.create 17

let last_refresh_lock = Mutex.create ()

let shutdown_requested = Atomic.make false

let worker : unit Worker_queue.t = Worker_queue.create ~name:"index" ()

(** Normalise an RPC bind address to an http:// URL suitable for polling.
    [0.0.0.0] is replaced with [127.0.0.1] since the service is local. *)
let normalize_rpc_addr addr =
  let with_scheme =
    if String.starts_with ~prefix:"http" addr then addr else "http://" ^ addr
  in
  (* Replace wildcard bind address with loopback for outgoing requests *)
  if String.starts_with ~prefix:"http://0.0.0.0" with_scheme then
    "http://127.0.0.1"
    ^ String.sub with_scheme 14 (String.length with_scheme - 14)
  else with_scheme

(** Poll [/explorer/status] on the indexer RPC endpoint.
    Returns [(head_level, synced)] or [None] when unreachable. *)
let poll_status rpc_addr =
  let url = normalize_rpc_addr rpc_addr ^ "/explorer/status" in
  match
    Cmd_runner.run_out
      ["curl"; "-sf"; "--connect-timeout"; "2"; "--max-time"; "4"; url]
  with
  | Error _ -> None
  | Ok json -> (
      try
        let j = Yojson.Safe.from_string json in
        let open Yojson.Safe.Util in
        let synced =
          match member "status" j with
          | `String s -> Some (String.equal (String.lowercase_ascii s) "synced")
          | _ -> None
        in
        let head_level =
          match member "blocks" j with `Int n -> Some n | _ -> None
        in
        Some (head_level, synced)
      with _ -> None)

(** Read [OCTEZ_INDEX_RPC_ADDR] from the instance env file and poll the RPC. *)
let refresh_instance (svc : Service.t) =
  let instance = svc.Service.instance in
  try
    let rpc_addr_opt =
      match Node_env.read ~inst:instance with
      | Error _ -> None
      | Ok pairs -> (
          match List.assoc_opt "OCTEZ_INDEX_RPC_ADDR" pairs with
          | None | Some "" -> None
          | Some v -> Some v)
    in
    (match rpc_addr_opt with
    | None -> () (* RPC not configured — nothing to poll *)
    | Some addr -> (
        match poll_status addr with
        | None -> () (* Unreachable — keep stale cache, no update *)
        | Some (head_level, synced) ->
            Index_metrics.set
              ~instance
              {
                Index_metrics.head_level;
                synced;
                last_check = Unix.gettimeofday ();
              })) ;
    Context.mark_instances_dirty ()
  with _ -> ()

let submit_refresh (svc : Service.t) =
  let key = Printf.sprintf "index-refresh:%s" svc.Service.instance in
  Worker_queue.submit_unit worker ~key ~work:(fun () ->
      try
        refresh_instance svc ;
        Mutex.protect last_refresh_lock (fun () ->
            Hashtbl.replace
              last_refresh
              svc.Service.instance
              (Unix.gettimeofday ()))
      with _ -> ())

let is_due_for_refresh now instance =
  match
    Mutex.protect last_refresh_lock (fun () ->
        Hashtbl.find_opt last_refresh instance)
  with
  | None -> true
  | Some last -> now -. last >= refresh_interval

let start () =
  Worker_queue.start worker ;
  Domain_pool.submit (fun () ->
      Eio_unix.sleep 0.5 ;
      while not (Atomic.get shutdown_requested) do
        (try
           let now = Unix.gettimeofday () in
           let index_services =
             match Service_registry.list () with
             | Ok svcs ->
                 List.filter
                   (fun (svc : Service.t) ->
                     String.equal svc.Service.role "index")
                   svcs
             | Error _ -> []
           in
           List.iter
             (fun svc ->
               if is_due_for_refresh now svc.Service.instance then
                 submit_refresh svc)
             index_services
         with _ -> ()) ;
        Eio_unix.sleep 1.0
      done)

let stop () =
  Atomic.set shutdown_requested true ;
  Worker_queue.stop worker
