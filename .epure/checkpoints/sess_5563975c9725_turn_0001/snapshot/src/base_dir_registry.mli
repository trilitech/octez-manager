(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

(* DEPRECATED: Use Directory_registry directly with dir_type:Client_base_dir *)

type base_dir_entry = Directory_registry.directory_entry

(** Register a client base directory with a list of associated service names. *)
val add :
  path:string ->
  registered_services:string list ->
  (unit, [`Msg of string]) result

(** Look up a base directory entry by its path. Returns [Ok None] if not found. *)
val find_by_path : string -> (base_dir_entry option, [`Msg of string]) result

(** List all registered base directory entries. *)
val list : unit -> (base_dir_entry list, [`Msg of string]) result

(** Remove a base directory entry by its path. *)
val remove : string -> (unit, [`Msg of string]) result
