(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Shared types for the binaries management page. *)

open Octez_manager_lib

(** Binary kind - distinguishes between Octez and Signatory binaries *)
type binary_kind = Octez | Signatory

(** Items displayed in the binaries list. *)
type item_type =
  | ManagedGroup of binary_kind * bool
  | ManagedVersion of binary_kind * string * int64 option * int
  | RegisteredDir of Binary_registry.registered_dir * int
  | RegisterAction
  | AvailableGroup of binary_kind * bool
  | AvailableVersion of binary_kind * Binary_downloader.version_info
  | AvailableSignatoryVersion of Signatory_downloader.version_info
  | AvailableMajorGroup of int * Binary_downloader.version_info list

(** Page state for the binaries management page. *)
type state = {
  managed_octez_versions : (string * int64 option * int) list;
  managed_signatory_versions : (string * int64 option * int) list;
  registered_dirs : (Binary_registry.registered_dir * int) list;
  available_octez_versions : Binary_downloader.version_info list;
  available_signatory_versions : Signatory_downloader.version_info list;
  items : item_type list;
  selected : int;
  loading_remote : bool;
  expanded_managed_octez : bool;
  expanded_managed_signatory : bool;
  expanded_available_octez : bool;
  expanded_available_signatory : bool;
  expanded_octez_majors : int list;
  expanded_managed_octez_items : string list;
  expanded_registered : string list;
  download_tick : int;
      (** incremented on each download progress update to force re-render *)
}

type msg = unit

type pstate = state Miaou.Core.Navigation.t
