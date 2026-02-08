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
      (** version, size, instance_count *)
  | RegisteredDir of Binary_registry.registered_dir * int
      (** registered_dir, instance_count *)
  | RegisterAction  (** Button to register a new directory *)
  | AvailableVersion of Binary_downloader.version_info
  | AvailableMajorGroup of int * Binary_downloader.version_info list
      (** major version, list of minor versions *)

(** Page state for the binaries management page. *)
type state = {
  managed_versions : (string * int64 option * int) list;
  registered_dirs : (Binary_registry.registered_dir * int) list;
  available_versions : Binary_downloader.version_info list;
  items : item_type list;
  selected : int;
  loading_remote : bool;
  expanded_majors : int list;  (** list of expanded major versions *)
  expanded_managed : string list;  (** list of expanded managed versions *)
  expanded_registered : string list;
      (** list of expanded registered directory aliases *)
}

type msg = unit

type pstate = state Miaou.Core.Navigation.t
