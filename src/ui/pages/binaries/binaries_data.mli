(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Data loading and item building for the binaries page. *)

open Octez_manager_lib

(** Load managed binary versions with sizes and instance counts. *)
val load_managed_versions : unit -> (string * int64 option * int) list

(** Load registered directories with instance counts. *)
val load_registered_dirs : unit -> (Binary_registry.registered_dir * int) list

(** [filter_latest_n_major_versions n versions] keeps only versions from
    the [n] most recent major version families. *)
val filter_latest_n_major_versions :
  int ->
  Binary_downloader.version_info list ->
  Binary_downloader.version_info list

(** Load available-for-download versions, filtering installed and old ones. *)
val load_available_versions : unit -> Binary_downloader.version_info list

(** [build_items managed registered available expanded_majors] constructs
    the flat item list for rendering. *)
val build_items :
  (string * int64 option * int) list ->
  (Binary_registry.registered_dir * int) list ->
  Binary_downloader.version_info list ->
  int list ->
  Binaries_types.item_type list
