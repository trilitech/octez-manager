(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Render-safe data accessors.

    This module provides the single source of truth for data that can be safely
    accessed during view/render functions. All functions here read from
    in-memory caches populated by background schedulers and NEVER perform I/O.

    {b IMPORTANT}: All data accessed during view/render functions MUST come from
    this module. If you need data that's not available here, add it to the
    appropriate scheduler first, then expose a cached accessor here.

    See AGENTS.md for detailed documentation on the TUI architecture. *)

(** {1 Baker Configuration}

    Cached by {!Delegate_scheduler}, refreshed every 60s. *)

(** [baker_has_dal ~instance] returns whether the baker has DAL enabled.
    Reads from cache, never performs file I/O. *)
val baker_has_dal : instance:string -> bool

(** [baker_delegates ~instance] returns the list of delegate public key hashes.
    Reads from cache, never performs file I/O. *)
val baker_delegates : instance:string -> string list

(** [baker_node_endpoint ~instance] returns the configured node endpoint.
    Reads from cache, never performs file I/O. *)
val baker_node_endpoint : instance:string -> string option

(** {1 Baker Highwatermarks}

    Cached by {!Delegate_scheduler}, refreshed every 60s. *)

(** [baker_highwatermarks ~instance] returns signing activity for delegates.
    Reads from cache, never performs file I/O. *)
val baker_highwatermarks :
  instance:string -> Baker_highwatermarks.delegate_activity list

(** {1 Delegate Participation Data}

    Cached by {!Delegate_scheduler}, refreshed every 60s via RPC. *)

(** [delegate_data ~pkh] returns participation data for a delegate.
    Reads from cache, never performs network I/O. *)
val delegate_data : pkh:string -> Delegate_data.t option

(** [baker_delegate_data ~instance] returns participation data for all
    delegates of a baker. Reads from cache, never performs I/O. *)
val baker_delegate_data : instance:string -> Delegate_data.t list

(** {1 DAL Node Health}

    Cached by {!System_metrics_scheduler}, refreshed every 5s. *)

(** [dal_health ~instance] returns health status for a DAL node.
    Reads from cache, never performs network I/O. *)
val dal_health : instance:string -> Dal_health.t option

(** {1 Node RPC Metrics}

    Cached by {!Rpc_scheduler}, refreshed every 6-10s. *)

(** [node_rpc_metrics ~instance] returns RPC metrics for a node.
    Reads from cache, never performs network I/O. *)
val node_rpc_metrics : instance:string -> Rpc_metrics.rpc_metrics option

(** {1 System Metrics}

    Cached by {!System_metrics_scheduler}, refreshed every 0.5-5s. *)

(** [cpu_chart ~role ~instance ~focus] renders a CPU usage chart.
    Returns [(rendered_chart, average_cpu)] or [None] if no data.
    Reads from cache, never performs I/O. *)
val cpu_chart :
  role:string -> instance:string -> focus:bool -> (string * float) option

(** [mem_sparkline ~role ~instance ~focus] renders a memory usage sparkline.
    Reads from cache, never performs I/O. *)
val mem_sparkline : role:string -> instance:string -> focus:bool -> string

(** [service_version ~role ~instance] returns the binary version string.
    Reads from cache, never performs I/O. *)
val service_version : role:string -> instance:string -> string option

(** [service_disk_size ~role ~instance] returns the data directory size in bytes.
    Reads from cache, never performs I/O. *)
val service_disk_size : role:string -> instance:string -> int64 option

(** {1 Service States}

    Cached by {!Data} module with 5s TTL. *)

(** [service_states ()] returns all service states.
    Uses cache with background refresh, minimal blocking. *)
val service_states : unit -> Data.Service_state.t list
