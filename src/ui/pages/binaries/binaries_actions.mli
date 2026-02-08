(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Side-effecting action handlers for the binaries page. *)

open Octez_manager_lib
open Binaries_types

(** Show confirm modal and remove a managed version in background. *)
val remove_version : string -> unit

(** Show confirm modal and unregister a directory in background. *)
val unregister_directory : Binary_registry.registered_dir -> unit

(** Download a version in background with multi-progress UI. *)
val download_version : Binary_downloader.version_info -> unit

(** Open file browser modal to register a new binary directory. *)
val register_directory : unit -> unit

(** Calculate unused versions, show confirm modal, bulk-remove them. *)
val prune_unused : state -> state

(** Dispatch Enter key based on selected item type. *)
val handle_action :
  toggle_managed_expansion:(state -> string -> state) ->
  toggle_registered_expansion:(state -> string -> state) ->
  toggle_major_expansion:(state -> int -> state) ->
  state ->
  state

(** Dispatch Tab key to toggle expansion of current item. *)
val toggle_current_group :
  toggle_managed_expansion:(state -> string -> state) ->
  toggle_registered_expansion:(state -> string -> state) ->
  toggle_major_expansion:(state -> int -> state) ->
  state ->
  state
