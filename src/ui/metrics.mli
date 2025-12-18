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

(** Wrap a scheduler tick function to record its duration. *)
val record_scheduler_tick : scheduler:string -> (unit -> unit) -> unit

(** Histogram snapshot type *)
type snapshot = {
  count : int;
  sum : float;
  min_v : float;
  max_v : float;
  p50 : float option;
  p90 : float option;
  p99 : float option;
}

(** Get scheduler histogram snapshots. Returns (scheduler_name, snapshot) pairs. *)
val get_scheduler_snapshots : unit -> (string * snapshot) list

(** Get current background queue depth. *)
val get_bg_queue_depth : unit -> int

(** Get maximum background queue depth seen. *)
val get_bg_queue_max : unit -> int

(** Get all service statuses. Returns list of (service_name, status) pairs. *)
val get_service_statuses : unit -> (string * bool) list

(** Check if metrics collection is enabled. *)
val is_enabled : unit -> bool

(** Get current server info if running. Returns Some (addr, port) or None. *)
val get_server_info : unit -> (string * int) option

(** Start metrics server on specified address and port. *)
val start_server : addr:string -> port:int -> unit

(** Parse address string in format "host:port" or "port". *)
val parse_addr : string -> (string * int, [> `Msg of string]) result

(** Enable metrics server if [OCTEZ_MANAGER_METRICS_ADDR] is set. *)
val maybe_start_from_env : unit -> unit

(** Snapshot of all metrics at a point in time *)
type metrics_snapshot = {
  timestamp : float;
  bg_queue_depth : int;
  bg_queue_max : int;
  services_active : int;
  services_total : int;
  render_p50 : float option;
  render_p90 : float option;
  render_p99 : float option;
  key_to_render_p50 : float option;
  key_to_render_p90 : float option;
  bg_wait_p50 : float option;
  bg_wait_p90 : float option;
}

(** Take a snapshot of current metrics state *)
val take_snapshot : unit -> metrics_snapshot

(** Get all recorded snapshots in chronological order *)
val get_snapshots : unit -> metrics_snapshot list

(** Start background recording (samples every 5 seconds) *)
val start_recording : unit -> unit

(** Stop background recording *)
val stop_recording : unit -> unit

(** Check if recording is active *)
val is_recording : unit -> bool

(** Set recording duration (number of samples to keep) *)
val set_recording_duration : int -> unit

(** Get current recording duration *)
val get_recording_duration : unit -> int

(** Clear all recorded snapshots *)
val clear_snapshots : unit -> unit
