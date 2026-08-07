(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Data loading and item building for the binaries page. *)

open Octez_manager_lib

(** Load managed Octez versions with sizes and instance counts. *)
val load_managed_octez_versions : unit -> (string * int64 option * int) list

(** Load managed Signatory versions with sizes and instance counts. *)
val load_managed_signatory_versions : unit -> (string * int64 option * int) list

(** Load registered directories with instance counts. *)
val load_registered_dirs : unit -> (Binary_registry.registered_dir * int) list

(** [filter_latest_n_major_versions n versions] keeps only versions from
    the [n] most recent major version families. *)
val filter_latest_n_major_versions :
  int ->
  Binary_downloader.version_info list ->
  Binary_downloader.version_info list

(** Load available Octez versions for download, filtering installed and old ones. *)
val load_available_octez_versions : unit -> Binary_downloader.version_info list

(** Load available Signatory versions for download, filtering installed and old ones. *)
val load_available_signatory_versions :
  unit -> Signatory_downloader.version_info list

(** [build_items managed_octez managed_signatory registered available_octez
    available_signatory ~expanded_managed_octez ~expanded_managed_signatory
    ~expanded_available_octez ~expanded_available_signatory
    ~expanded_octez_majors]
    constructs the flat item list for rendering with nested groups. *)
val build_items :
  (string * int64 option * int) list ->
  (string * int64 option * int) list ->
  (Binary_registry.registered_dir * int) list ->
  Binary_downloader.version_info list ->
  Signatory_downloader.version_info list ->
  expanded_managed_octez:bool ->
  expanded_managed_signatory:bool ->
  expanded_available_octez:bool ->
  expanded_available_signatory:bool ->
  expanded_octez_majors:int list ->
  Binaries_types.item_type list
