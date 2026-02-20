(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Persistent storage for instance groups. *)

(** Subdirectory containing per-group JSON files. *)
val groups_dir : unit -> string

(** Persist a group configuration to the registry as a JSON file. *)
val write : Group.t -> (unit, Rresult.R.msg) result

(** List all registered group configurations. *)
val list : unit -> (Group.t list, Rresult.R.msg) result

(** Look up a group by [name]. Returns [Ok None] if not found. *)
val find : name:string -> (Group.t option, Rresult.R.msg) result

(** Remove the registry entry for [name]. *)
val remove : name:string -> (unit, Rresult.R.msg) result
