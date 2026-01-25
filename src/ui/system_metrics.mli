(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

(** System metrics collection for service instances.

    Collects CPU, memory, disk usage, and version information for
    services managed by systemd. Designed to be reusable across
    different service types (node, baker, accuser, dal, signer). *)

(** Per-process resource statistics *)
type process_stats = {
  pid : int;
  cpu_percent : float;  (** 0-100 per core *)
  memory_rss : int64;  (** bytes *)
  memory_percent : float;  (** 0-100 *)
}

(** Previous CPU sample for delta calculation *)
type cpu_sample = {
  utime : int64;  (** user time in clock ticks *)
  stime : int64;  (** system time in clock ticks *)
  timestamp : float;  (** Unix timestamp *)
}

(** Aggregate metrics for a service instance *)
type t = {
  version : string option;  (** e.g. "21.0" *)
  cpu_percent : float;  (** sum across all processes *)
  memory_rss : int64;  (** total RSS bytes *)
  memory_percent : float;  (** sum across all processes *)
  data_dir_size : int64 option;  (** bytes *)
  pids : int list;  (** tracked PIDs *)
}

(** Empty metrics record *)
val empty : t

(** {2 PID Discovery} *)

(** Get the main PID for a systemd unit by unit name.
    Works for any systemd unit name (managed or external).
    Returns [None] if the service is not running or PID cannot be determined. *)
val get_main_pid_by_unit : unit_name:string -> int option

(** Get the main PID for a managed systemd service.
    Constructs unit name as [octez-{role}@{instance}].
    Returns [None] if the service is not running or PID cannot be determined. *)
val get_service_main_pid : role:string -> instance:string -> int option

(** Get child PIDs of a process (direct children only). *)
val get_child_pids : parent_pid:int -> int list

(** Get all PIDs for a systemd unit by unit name (main + all descendants).
    Works for any systemd unit name (managed or external).
    Returns empty list if service is not running. *)
val get_pids_by_unit : unit_name:string -> int list

(** Get all PIDs for a managed service (main + all descendants).
    Constructs unit name as [octez-{role}@{instance}].
    Returns empty list if service is not running. *)
val get_service_pids : role:string -> instance:string -> int list

(** {2 Process Metrics} *)

(** Read raw CPU times and RSS from /proc/<pid>/stat.
    Returns [(utime, stime, rss_bytes)] or [None] if process not found. *)
val read_proc_stat : pid:int -> (int64 * int64 * int64) option

(** Calculate CPU percentage from two samples. *)
val calc_cpu_percent : prev:cpu_sample -> curr:cpu_sample -> float

(** Get process stats with CPU calculated from previous sample.
    Returns the stats and the new sample for the next calculation.
    Pass [None] for [prev_sample] on first call (CPU will be 0.0). *)
val get_process_stats :
  pid:int ->
  prev_sample:cpu_sample option ->
  (process_stats * cpu_sample) option

(** {2 Version Detection} *)

(** Parse version string from binary --version output.
    Looks for "Octez XX.Y" pattern and returns just the version number. *)
val parse_version_output : string -> string option

(** Get version string from a binary.
    Runs [binary --version] and extracts version number. *)
val get_version : binary:string -> string option

(** {2 Disk Usage} *)

(** Get directory size in bytes using du.
    Returns [None] if path doesn't exist or du fails. *)
val get_dir_size : path:string -> int64 option

(** {2 Formatting} *)

(** Format bytes as human-readable string (e.g., "1.2G", "450M"). *)
val format_bytes : int64 -> string
