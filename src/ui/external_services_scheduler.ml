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

(** Poll interval: 30 seconds - external services don't change frequently *)
let poll_interval = 30.0

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
    | Ok [] ->
        (* Empty result may be transient (daemon-reload, brief systemd state
           flush).  Preserve the previous cache so the section does not
           flicker away.  The next poll will update if services genuinely
           disappeared. *)
        ()
    | Ok services -> Mutex.protect cache_lock (fun () -> cache := services)
    | Error _ ->
        (* Keep previous cache on error *)
        ())

let stop_flag = Atomic.make false

let started = ref false

let scheduler_loop () =
  while not (Atomic.get stop_flag) do
    refresh () ;
    Eio_unix.sleep poll_interval
  done

(** Start the background scheduler *)
let start () =
  if not !started then (
    started := true ;
    Atomic.set stop_flag false ;
    (* Initial synchronous load for immediate display *)
    refresh () ;
    (* Submit background polling to domain pool *)
    Domain_pool.submit scheduler_loop)

(** Stop the background scheduler *)
let stop () = Atomic.set stop_flag true

let shutdown = stop
