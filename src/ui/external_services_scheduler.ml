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

(** Cache for validation results: unit_name -> validation result *)
let validation_cache : (string, (unit, [`Msg of string]) result) Hashtbl.t =
  Hashtbl.create 17

let validation_cache_lock = Mutex.create ()

(** Poll interval: 30 seconds - external services don't change frequently *)
let poll_interval = 30.0

(** Last poll timestamp *)
let last_poll = ref 0.0

(** Get cached external services (fast, no I/O) *)
let get () = Mutex.protect cache_lock (fun () -> !cache)

(** Get cached validation result for a service (fast, no I/O) *)
let get_validation ~unit_name =
  Mutex.protect validation_cache_lock (fun () ->
      Hashtbl.find_opt validation_cache unit_name)

(** Refresh external services (does I/O, called by background scheduler) *)
let refresh () =
  let now = Unix.gettimeofday () in
  if now -. !last_poll < poll_interval then () (* Skip if polled recently *)
  else (
    last_poll := now ;
    match External_service_detector.detect () with
    | Ok services ->
        Mutex.protect cache_lock (fun () -> cache := services) ;
        (* Validate each service and cache the result.
           Fetch service registry once to avoid repeated I/O. *)
        Mutex.protect validation_cache_lock (fun () ->
            Hashtbl.clear validation_cache ;
            match Service_registry.list () with
            | Error _ ->
                (* If we can't read the service registry, skip validation caching *)
                ()
            | Ok all_services ->
                List.iter
                  (fun (svc : External_service.t) ->
                    let validation_result =
                      Import.validate_importable_with_services ~all_services svc
                    in
                    Hashtbl.replace
                      validation_cache
                      svc.config.unit_name
                      validation_result)
                  services)
    | Error _ ->
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

let shutdown = stop
