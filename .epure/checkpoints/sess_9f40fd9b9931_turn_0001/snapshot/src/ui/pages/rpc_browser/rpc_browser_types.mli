(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Shared type definitions for the RPC Browser.

    This module defines the core types used across all RPC browser submodules.
    It exists to break circular dependencies between {!Rpc_browser_state} and
    its helper modules. *)

open Octez_manager_lib
module Pager = Miaou_widgets_display.Pager_widget

(** Entry kind for navigation list. *)
type entry_kind =
  | Get
  | Sub
  | Dyn of string  (** Dynamic segment to prompt for *)
  | DynValue of (string * string)  (** Recent dynamic value: (typ, value) *)
  | ChangeTarget  (** Button to change target instance *)

(** A navigation entry. *)
type entry = {name : string; kind : entry_kind}

(** A single pager slot for multi-pager view. *)
type pager_slot = {
  id : int;  (** 0-9 identifier *)
  request : string;  (** URL or empty if new *)
  body : string;  (** Rendered content *)
  raw_body : string;  (** Raw JSON *)
  pager : Pager.t option;  (** Pager widget *)
  foldable : Foldable_json.t option;  (** Fold state *)
  response_time_ms : float option;
  response_size : int option;
  target_instance : Service.t option;  (** Target node for this pager *)
  streaming_handle : Rpc_client.monitor_handle option;
      (** Active streaming connection, if any *)
}

(** Focus for side-by-side mode. *)
type result_focus = FocusBrowser | FocusPager of int

(** @deprecated Use result_focus instead *)
type focus = FocusBrowser | FocusPager

(** Display mode. *)
type mode =
  | List of {
      entries : entry list;  (** Available entries at current path *)
      cursor : int;  (** Selected entry index *)
      loading : bool;  (** Whether entries are being fetched *)
    }
  | Result of {
      pagers : pager_slot list;  (** All pagers, max 10 *)
      focus : result_focus;  (** Browser or which pager *)
      last_pager_id : int;
          (** Last focused pager, used when browser is focused *)
    }

(** OpenAPI loading status. *)
type openapi_status = Loading | Ready | Error of string | NotAvailable

(** Dynamic segment value history entry. *)
type dynamic_value = {
  segment_type : string;  (** e.g., "chain_id", "block_id" *)
  value : string;  (** User-provided value *)
  timestamp : float;  (** When it was used *)
}

(** Recent path entry for LRU shortcuts. *)
type recent_path = {
  rp_path : string;  (** RPC path e.g., "/chains/main/blocks/head" *)
  rp_desc : string;  (** Description for display *)
  rp_timestamp : float;  (** When it was used *)
}

(** RPC Browser page state. *)
type state = {
  instances : Service.t list;  (** Available instances with RPC *)
  selected_idx : int;  (** Currently selected instance index *)
  path : string list;  (** Current navigation path segments *)
  mode : mode;  (** Current display mode *)
  openapi_status : openapi_status;  (** OpenAPI spec status *)
  error : string option;  (** Last error message *)
  dynamic_history : dynamic_value list;
      (** Recent user-provided dynamic values *)
  recent_paths : recent_path list;  (** LRU list of recently used RPC paths *)
  cached_entries : entry list;  (** Cached entries for side-by-side display *)
  cached_cursor : int;  (** Cached cursor position for side-by-side display *)
  target_override : Service.t option;
      (** Global target override for RPC calls *)
}
