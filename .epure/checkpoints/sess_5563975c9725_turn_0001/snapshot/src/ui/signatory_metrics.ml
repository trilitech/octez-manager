(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Signatory health and metrics tracking.

    This module tracks health status and metrics for Signatory remote signer
    instances. It caches data with TTL to avoid excessive polling. *)

(** Health status of a Signatory instance *)
type health_status =
  | Up  (** Service is responding and healthy *)
  | Down  (** Service is not responding *)
  | Degraded  (** Service is responding but experiencing issues *)
  | Unknown  (** Health status not yet determined *)

(** Metrics for a Signatory instance *)
type signatory_metrics = {
  health : health_status;  (** Current health status *)
  last_check : float option;  (** Unix timestamp of last health check *)
  authorized_keys : string list;
      (** List of authorized key hashes (tz1/tz2/tz3/tz4) *)
  address : string option;  (** HTTP server address (host:port) *)
  metrics_address : string option;  (** Prometheus metrics address *)
  version : string option;  (** Signatory version *)
  backend : string option;  (** Backend type (File, YubiHSM, etc.) *)
  total_requests : int option;  (** Total signature requests (lifetime) *)
  successful_requests : int option;  (** Successful signature count *)
  failed_requests : int option;  (** Failed signature count *)
  last_error : string option;  (** Last error message if any *)
}

(** Cache table: instance name -> metrics *)
let table : (string, signatory_metrics) Hashtbl.t = Hashtbl.create 17

(** Lock for thread-safe access *)
let lock = Mutex.create ()

(** Get metrics for an instance *)
let get ~instance =
  Mutex.protect lock (fun () -> Hashtbl.find_opt table instance)

(** Update metrics for an instance *)
let set ~instance metrics =
  Mutex.protect lock (fun () -> Hashtbl.replace table instance metrics)

(** Clear all cached metrics *)
let clear () = Mutex.protect lock (fun () -> Hashtbl.clear table)

(** Remove metrics for a specific instance *)
let remove ~instance =
  Mutex.protect lock (fun () -> Hashtbl.remove table instance)

(** Create initial/unknown metrics for an instance *)
let create_unknown () =
  {
    health = Unknown;
    last_check = None;
    authorized_keys = [];
    address = None;
    metrics_address = None;
    version = None;
    backend = None;
    total_requests = None;
    successful_requests = None;
    failed_requests = None;
    last_error = None;
  }

(** Calculate error rate percentage from metrics *)
let error_rate metrics =
  match (metrics.total_requests, metrics.failed_requests) with
  | Some total, Some failed when total > 0 ->
      Some (float_of_int failed /. float_of_int total *. 100.)
  | _ -> None

(** Calculate success rate percentage from metrics *)
let success_rate metrics =
  match error_rate metrics with Some err -> Some (100. -. err) | None -> None

(** Check if metrics are stale (older than 10 seconds) *)
let is_stale metrics =
  match metrics.last_check with
  | None -> true
  | Some ts ->
      let now = Unix.gettimeofday () in
      now -. ts > 10.0
