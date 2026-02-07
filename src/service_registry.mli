(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

val registry_root : unit -> string

val services_dir : unit -> string

val write : Service.t -> (unit, Rresult.R.msg) result

val list : unit -> (Service.t list, Rresult.R.msg) result

val remove : instance:string -> (unit, Rresult.R.msg) result

val find : instance:string -> (Service.t option, Rresult.R.msg) result

(** Count instances using a specific binary source. *)
val count_instances_using : Binary_registry.bin_source -> int

(** Get the instance names using a specific binary source. *)
val get_instances_using : Binary_registry.bin_source -> string list
