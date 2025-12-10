(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Measure render duration for a page. No-op when metrics are disabled. *)
val record_render : page:string -> (unit -> 'a) -> 'a

(** Mark the start of an input event to measure key-to-render latency. *)
val mark_input_event : unit -> unit

(** Record background queue depth after enqueue. *)
val record_bg_enqueue : queued_depth:int -> unit

(** Record wait time for a background task and queue depth after dequeue. *)
val record_bg_dequeue : queued_depth:int -> wait_ms:float -> unit

(** Record service status for monitoring. *)
val record_service_status : service:string -> is_active:bool -> unit

(** Get current background queue depth. *)
val get_bg_queue_depth : unit -> int

(** Get maximum background queue depth seen. *)
val get_bg_queue_max : unit -> int

(** Get all service statuses. Returns list of (service_name, status) pairs. *)
val get_service_statuses : unit -> (string * bool) list

(** Check if metrics collection is enabled. *)
val is_enabled : unit -> bool

(** Enable metrics server if [OCTEZ_MANAGER_METRICS_ADDR] is set. *)
val maybe_start_from_env : unit -> unit
