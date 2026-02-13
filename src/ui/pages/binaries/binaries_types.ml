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

type msg = unit

type pstate = state Miaou.Core.Navigation.t
