(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Detection of external (unmanaged) Octez systemd services.

    This module scans the system for systemd services that run octez binaries
    but are not managed by octez-manager. Detection is performed on-demand
    (not in the render loop) and results are cached.

    {b Important:} Detection performs I/O and must NOT be called from view
    functions. Use {!get_cached} to read results during rendering. *)

(** {1 Detection} *)

(** Scan for external octez services.

    This function performs I/O (systemctl calls, file reads) and should
    only be called from startup or user-triggered actions, never from
    view functions.

    @return List of detected external services, or error message *)
val detect : unit -> (External_service.t list, string) result

(** {1 Cache Access} *)

(** Get cached detection results. Safe to call from view functions.
    Returns empty list if detection hasn't run yet. *)
val get_cached : unit -> External_service.t list

(** Clear the cache. *)
val clear_cache : unit -> unit

(** {1 Filtering} *)

(** Check if a unit name matches octez-manager's naming convention.
    Pattern: [octez-<role>@<instance>.service] *)
val is_managed_unit_name : string -> bool

(** Check if a unit name corresponds to a service in the registry. *)
val is_in_registry : unit_name:string -> bool

(** {1 Systemd Queries} *)

(** List all systemd service units (active, inactive, and failed).
    @return List of unit names *)
val list_all_service_units : unit -> (string list, string) result

(** Get ExecStart property for a unit.
    @return The ExecStart command line, or None if not set *)
val get_exec_start : unit_name:string -> string option

(** Get multiple properties for a unit.
    @return Association list of property name to value *)
val get_unit_properties :
  unit_name:string -> props:string list -> (string * string) list

(** Get full unit file content (including drop-ins).
    @return Unit file content, or error if permission denied *)
val get_unit_content :
  unit_name:string -> (string, [`Permission_denied | `Error of string]) result
