(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Binaries management page for downloading, registering, and pruning binaries. *)

open Octez_manager_lib

(** The type of item shown in the binaries list. *)
type item_type =
  | ManagedVersion of string * int64 option * int
      (** A managed version: [(version, total_size, binary_count)]. *)
  | RegisteredDir of Binary_registry.registered_dir * int
      (** A user-registered directory with its binary count. *)
  | RegisterAction  (** The "Register directory..." action row. *)
  | AvailableVersion of Binary_downloader.version_info
      (** A single downloadable version. *)
  | AvailableMajorGroup of int * Binary_downloader.version_info list
      (** A collapsible group of versions sharing the same major version. *)

(** Page state holding managed, registered, and available versions. *)
type state = {
  managed_versions : (string * int64 option * int) list;
      (** Installed managed versions. *)
  registered_dirs : (Binary_registry.registered_dir * int) list;
      (** User-registered directories. *)
  available_versions : Binary_downloader.version_info list;
      (** Remote versions available for download. *)
  items : item_type list;  (** Flat list of displayable items. *)
  selected : int;  (** Currently selected item index. *)
  loading_remote : bool;  (** [true] while fetching remote version list. *)
  expanded_majors : int list;  (** Major version groups currently expanded. *)
  expanded_managed : string list;  (** Managed versions currently expanded. *)
  expanded_registered : string list;
      (** Registered directories currently expanded. *)
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
    (Binary_registry.registered_dir * int) list ->
    Binary_downloader.version_info list ->
    int list ->
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
