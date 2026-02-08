(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Shared types for the binaries management page. *)

open Octez_manager_lib

(** Items displayed in the binaries list. *)
type item_type =
  | ManagedVersion of string * int64 option * int
  | RegisteredDir of Binary_registry.registered_dir * int
  | RegisterAction
  | AvailableVersion of Binary_downloader.version_info
  | AvailableMajorGroup of int * Binary_downloader.version_info list

(** Page state for the binaries management page. *)
type state = {
  managed_versions : (string * int64 option * int) list;
  registered_dirs : (Binary_registry.registered_dir * int) list;
  available_versions : Binary_downloader.version_info list;
  items : item_type list;
  selected : int;
  loading_remote : bool;
  expanded_majors : int list;
  expanded_managed : string list;
  expanded_registered : string list;
}

type msg = unit

type pstate = state Miaou.Core.Navigation.t
