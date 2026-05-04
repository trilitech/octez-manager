(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Signatory health and metrics tracking *)

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

(** Get cached metrics for an instance *)
val get : instance:string -> signatory_metrics option

(** Update metrics for an instance *)
val set : instance:string -> signatory_metrics -> unit

(** Clear all cached metrics *)
val clear : unit -> unit

(** Remove metrics for a specific instance *)
val remove : instance:string -> unit

(** Create initial/unknown metrics for an instance *)
val create_unknown : unit -> signatory_metrics

(** Calculate error rate percentage from metrics *)
val error_rate : signatory_metrics -> float option

(** Calculate success rate percentage from metrics *)
val success_rate : signatory_metrics -> float option

(** Check if metrics are stale (older than 10 seconds) *)
val is_stale : signatory_metrics -> bool
