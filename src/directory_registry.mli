(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Unified directory registry for tracking both node data directories and
    client base directories used by services. *)

(** Directory type discriminator *)
type dir_type =
  | Node_data_dir  (** Node data directory (used by node services) *)
  | Client_base_dir
      (** Client base directory / wallet (used by baker/signer) *)
  | App_bin_dir  (** Application binary directory (contains octez binaries) *)

(** Directory registry entry *)
type directory_entry = {
  path : string;  (** Absolute directory path *)
  dir_type : dir_type;  (** Type of directory *)
  created_at : string;  (** ISO timestamp: YYYY-MM-DD HH:MM:SS *)
  linked_services : string list;  (** Instance names using this directory *)
}

(** Register or update a directory in the registry.
    If the path already exists, updates its linked_services. *)
val add :
  path:string ->
  dir_type:dir_type ->
  linked_services:string list ->
  (unit, [`Msg of string]) result

(** Find a directory entry by path. *)
val find_by_path : string -> (directory_entry option, [`Msg of string]) result

(** List all registered directories.
    @param dir_type Optional filter by directory type *)
val list :
  ?dir_type:dir_type -> unit -> (directory_entry list, [`Msg of string]) result

(** Remove a directory from the registry. *)
val remove : string -> (unit, [`Msg of string]) result

(** Update linked services for an existing directory entry.
    If the directory doesn't exist, does nothing. *)
val update_linked_services :
  path:string -> linked_services:string list -> (unit, [`Msg of string]) result

(** Remove all directories from the registry. *)
val clear_all : unit -> (unit, [`Msg of string]) result

(** Migrate from old base_dirs.json format.
    Called automatically on first read. *)
val migrate_from_base_dir_registry : unit -> (unit, [`Msg of string]) result
