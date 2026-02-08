(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

(** Root directory of the octez-manager registry. *)
val registry_root : unit -> string

(** Subdirectory containing per-service JSON files. *)
val services_dir : unit -> string

(** Persist a service configuration to the registry as a JSON file. *)
val write : Service.t -> (unit, Rresult.R.msg) result

(** List all registered service configurations. *)
val list : unit -> (Service.t list, Rresult.R.msg) result

(** Remove the registry entry for [instance]. *)
val remove : instance:string -> (unit, Rresult.R.msg) result

(** Look up a service by [instance] name. Returns [Ok None] if not found. *)
val find : instance:string -> (Service.t option, Rresult.R.msg) result

(** Count instances using a specific binary source.

    Does not propagate registry read/parse errors: returns [0] if
    {!list} fails. Use {!list} directly when the distinction between
    "no instances" and "an error occurred" matters. *)
val count_instances_using : Binary_registry.bin_source -> int

(** Get the instance names using a specific binary source.

    Does not propagate registry read/parse errors: returns [[]] if
    {!list} fails. Use {!list} directly when the distinction between
    "no instances" and "an error occurred" matters. *)
val get_instances_using : Binary_registry.bin_source -> string list
