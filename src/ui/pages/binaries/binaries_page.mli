(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Binaries management page for downloading, registering, and pruning binaries. *)

open Octez_manager_lib

(** Binary kind - distinguishes between Octez and Signatory binaries *)
type binary_kind = Binaries_types.binary_kind = Octez | Signatory

(** The type of item shown in the binaries list. *)
type item_type = Binaries_types.item_type =
  | ManagedGroup of binary_kind * bool  (** kind, is_expanded *)
  | ManagedVersion of binary_kind * string * int64 option * int
      (** kind, version, size, instance_count *)
  | RegisteredDir of Binary_registry.registered_dir * int
      (** registered_dir, instance_count *)
  | RegisterAction  (** Button to register a new directory *)
  | AvailableGroup of binary_kind * bool  (** kind, is_expanded *)
  | AvailableVersion of binary_kind * Binary_downloader.version_info
      (** Octez version *)
  | AvailableSignatoryVersion of Signatory_downloader.version_info
      (** Signatory version (different type) *)
  | AvailableMajorGroup of int * Binary_downloader.version_info list
      (** Octez major version group *)

(** Page state holding managed, registered, and available versions. *)
type state = {
  managed_octez_versions : (string * int64 option * int) list;
  managed_signatory_versions : (string * int64 option * int) list;
  registered_dirs : (Binary_registry.registered_dir * int) list;
  available_octez_versions : Binary_downloader.version_info list;
  available_signatory_versions : Signatory_downloader.version_info list;
  items : item_type list;
  selected : int;
  loading_remote : bool;
  expanded_managed_octez : bool;  (** Octez managed group expanded *)
  expanded_managed_signatory : bool;  (** Signatory managed group expanded *)
  expanded_available_octez : bool;  (** Octez available group expanded *)
  expanded_available_signatory : bool;
      (** Signatory available group expanded *)
  expanded_octez_majors : int list;  (** list of expanded major versions *)
  expanded_managed_octez_items : string list;
      (** list of expanded managed octez versions *)
  expanded_registered : string list;
      (** list of expanded registered directory aliases *)
}

(** Message type (unused, placeholder for Miaou page signature). *)
type msg = unit

(** Page name for the page registry. *)
val name : string

(** Pre-built page value for registration. *)
val page : Miaou.Core.Registry.page

(** Register this page with the global page registry. *)
val register : unit -> unit

(** Page implementation exposing [state] and [msg] type equalities. *)
module Page_Impl :
  Miaou.Core.Tui_page.PAGE_SIG with type state = state and type msg = msg

(** Testing interface exposing internal helpers. *)
module For_tests : sig
  (** Filter a version list to keep only the [n] latest major version families. *)
  val filter_latest_n_major_versions :
    int ->
    Binary_downloader.version_info list ->
    Binary_downloader.version_info list

  (** Format a byte count as a human-readable size string (e.g. ["1.2 GB"]). *)
  val format_size : int64 -> string

  (** Build the flat item list from managed versions, registered directories,
      available versions, and which major groups are expanded. *)
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
    item_type list

  (** Move the cursor up by one position. *)
  val move_up : state -> state

  (** Move the cursor down by one position. *)
  val move_down : state -> state

  (** Toggle expansion of a major version group. *)
  val toggle_major_expansion : state -> int -> state

  (** Toggle expansion of a managed version entry. *)
  val toggle_managed_expansion : state -> string -> state

  (** Toggle expansion of a registered directory entry. *)
  val toggle_registered_expansion : state -> string -> state
end
