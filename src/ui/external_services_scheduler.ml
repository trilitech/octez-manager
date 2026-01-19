(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Background scheduler for detecting external Octez services.

    Polls systemd at configurable intervals to detect Octez services
    not managed by octez-manager. Results are cached in memory for
    fast access during rendering. *)

open Octez_manager_lib

(** Cache storage *)
let cache : External_service.t list ref = ref []

let cache_lock = Mutex.create ()

(** Poll interval: 5 seconds *)
let poll_interval = 5.0

(** Last poll timestamp *)
let last_poll = ref 0.0

(** Get cached external services (fast, no I/O) *)
let get () = Mutex.protect cache_lock (fun () -> !cache)

(** Refresh external services (does I/O, called by background scheduler) *)
let refresh () =
  let now = Unix.gettimeofday () in
  if now -. !last_poll < poll_interval then () (* Skip if polled recently *)
  else (
    last_poll := now ;
    match External_service_detector.detect () with
    | Ok services ->
        Mutex.protect cache_lock (fun () -> cache := services) ;
        Format.eprintf
          "[External Services Scheduler] Detected %d services@."
          (List.length services)
    | Error msg ->
        Format.eprintf
          "[External Services Scheduler] Detection failed: %s@."
          msg ;
        (* Keep previous cache on error *)
        ())

(** Background domain for polling *)
let domain = ref None

let stop_flag = ref false

let scheduler_loop () =
  while not !stop_flag do
    refresh () ;
    Unix.sleepf poll_interval
  done

(** Start the background scheduler *)
let start () =
  match !domain with
  | Some _ -> () (* Already running *)
  | None ->
      stop_flag := false ;
      (* Initial synchronous load for immediate display *)
      refresh () ;
      (* Start background polling *)
      domain := Some (Domain.spawn scheduler_loop)

(** Stop the background scheduler *)
let stop () =
  stop_flag := true ;
  match !domain with
  | None -> ()
  | Some d ->
      Domain.join d ;
      domain := None
