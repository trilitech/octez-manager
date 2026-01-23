(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Background scheduler for collecting system metrics.

    Polls CPU, memory, disk usage, and version information at
    configurable intervals. Uses sparklines to display history. *)

(** {2 Metrics Polling} *)

(** Poll metrics for a specific service instance.
    Call this to update CPU, memory, disk, and version data.
    For external services, pass [~unit_name] to use the actual systemd unit name
    instead of constructing it from role and instance. *)
val poll :
  role:string ->
  instance:string ->
  binary:string ->
  data_dir:string ->
  ?unit_name:string ->
  unit ->
  unit

(** Submit a poll request to the worker queue (non-blocking).
    For external services, pass [~unit_name] to use the actual systemd unit name. *)
val submit_poll :
  role:string ->
  instance:string ->
  binary:string ->
  data_dir:string ->
  ?unit_name:string ->
  unit ->
  unit

(** {2 Metrics Access} *)

(** Get version string for an instance. *)
val get_version : role:string -> instance:string -> string option

(** Invalidate cached version for an instance.
    Forces version refresh on next poll. Call after upgrading a service. *)
val invalidate_version : role:string -> instance:string -> unit

(** Format version with color based on comparison with latest stable.
    - Green: running latest stable
    - Yellow: same major, older minor
    - Red: older major version
    - Blue: dev or RC version *)
val format_version_colored : string -> string

(** Get disk size for an instance. *)
val get_disk_size : role:string -> instance:string -> int64 option

(** {2 Rendering} *)

(** Render CPU line chart for an instance (multi-row braille).
    Returns [Some (chart_lines, avg_percent)] or [None] if no data.
    The chart_lines string contains newlines for multi-row display. *)
val render_cpu_chart :
  role:string -> instance:string -> focus:bool -> (string * float) option

(** Render memory sparkline for an instance.
    Returns empty string if no data. *)
val render_mem_sparkline :
  role:string -> instance:string -> focus:bool -> string

(** {2 Visibility Tracking} *)

(** Mark an instance as visible (unfolded and on-screen).
    Visible instances are polled at normal intervals. *)
val mark_visible : role:string -> instance:string -> unit

(** Mark an instance as hidden.
    Hidden instances are polled at reduced intervals (4x slower). *)
val mark_hidden : role:string -> instance:string -> unit

(** Clear all visibility markers (call at start of render pass). *)
val clear_visibility : unit -> unit

(** {2 Scheduler Control} *)

(** Start the background polling loop.
    Only starts once; subsequent calls are no-ops. *)
val start : unit -> unit

(** Perform one tick of polling. *)
val tick : unit -> unit

(** Clear all stored metrics. *)
val clear : unit -> unit

(** Shutdown the background scheduler. *)
val shutdown : unit -> unit

(** Get worker queue statistics. *)
val get_worker_stats : unit -> Worker_queue.stats
