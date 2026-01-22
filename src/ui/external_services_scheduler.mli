(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Background scheduler for detecting external Octez services.

    This module runs detection in a background domain and caches results
    for fast access during rendering. Never call External_service_detector
    directly from render paths - always use this scheduler.

    Usage:
    - Call {!start} once at application startup
    - Call {!get} from render functions (fast, no I/O)
    - Call {!stop} at application shutdown *)

(** Get cached external services.
    Fast, no I/O, safe to call from render functions. *)
val get : unit -> Octez_manager_lib.External_service.t list

(** Force a refresh of external services (does I/O).
    Called automatically by background scheduler.
    Can be called manually for immediate refresh. *)
val refresh : unit -> unit

(** Start the background scheduler.
    Performs initial synchronous detection, then polls every 5 seconds. *)
val start : unit -> unit

(** Stop the background scheduler. *)
val stop : unit -> unit

(** Shutdown the background scheduler (alias for stop). *)
val shutdown : unit -> unit
