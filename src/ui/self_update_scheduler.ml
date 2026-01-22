(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Background scheduler for self-update checks.

    Polls GitHub releases every 10 minutes to check for new versions
    of octez-manager. Results are cached in memory for fast access
    during rendering. *)

open Octez_manager_lib

(** Poll interval: 10 minutes *)
let poll_interval = 600.0

(** {1 Cache} *)

type cached_update = {
  latest_version : string;
  is_major : bool;
  install_method : Self_update_checker.install_method;
}

let cache : cached_update option ref = ref None

let cache_lock = Mutex.create ()

(** Get cached update info (fast, no I/O) *)
let get () = Mutex.protect cache_lock (fun () -> !cache)

(** Check if an update is available (fast, no I/O) *)
let update_available () = Option.is_some (get ())

(** Get latest version if available (fast, no I/O) *)
let get_latest_version () = Option.map (fun u -> u.latest_version) (get ())

(** Check if update is a major version (fast, no I/O) *)
let is_major_update () =
  match get () with Some u -> u.is_major | None -> false

(** Get installation method (fast, no I/O) *)
let get_install_method () =
  match get () with Some u -> Some u.install_method | None -> None

(** {1 Refresh logic} *)

(** Last poll timestamp *)
let last_poll = ref 0.0

(** Refresh update check (does I/O, called by background scheduler) *)
let refresh ?(force = false) () =
  let now = Unix.gettimeofday () in
  if (not force) && now -. !last_poll < poll_interval then ()
    (* Skip if polled recently *)
  else (
    last_poll := now ;
    match Self_update_checker.check_for_updates ~force () with
    | Self_update_checker.Update_available info ->
        let install_method = Self_update_checker.detect_install_method () in
        (* Only cache if not dismissed *)
        if not (Self_update_checker.is_version_dismissed info.latest_version)
        then
          Mutex.protect cache_lock (fun () ->
              cache :=
                Some
                  {
                    latest_version = info.latest_version;
                    is_major = info.is_major_update;
                    install_method;
                  })
        else Mutex.protect cache_lock (fun () -> cache := None)
    | Self_update_checker.Up_to_date | Self_update_checker.Check_disabled
    | Self_update_checker.Check_failed _ ->
        Mutex.protect cache_lock (fun () -> cache := None))

(** Clear the cache (e.g., after dismissing a version) *)
let clear_cache () = Mutex.protect cache_lock (fun () -> cache := None)

(** {1 Background scheduler} *)

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
      (* Start background polling - don't block startup with initial check *)
      domain := Some (Domain.spawn scheduler_loop)

(** Stop the background scheduler *)
let stop () =
  stop_flag := true ;
  match !domain with
  | None -> ()
  | Some d ->
      Domain.join d ;
      domain := None

(** Run a single check immediately (for startup) *)
let check_now () = refresh ~force:true ()
