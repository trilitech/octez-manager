(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Page state types and navigation helpers for the Rewards page. *)

(** Tab selector for the rewards page. *)
type active_tab = Overview | Delegators | History | Configuration

(** Sort column in the delegators list. *)
type sort_column = SortAddress | SortBalance | SortReward | SortStatus

(** Filter mode in the delegators list. *)
type filter_mode =
  | FilterAll
  | FilterEligible
  | FilterExcluded
  | FilterBelowMin

(** Full page state. *)
type state = {
  baker_instances : (string * string) list;
  selected_baker : int;
  active_tab : active_tab;
  selected_cycle : int option;
  current_cycle : int option;
  delegator_cursor : int;
  delegator_sort : sort_column;
  delegator_filter : filter_mode;
  search_query : string;
  search_active : bool;
  blueprint : Octez_manager_rewards.Rewards.payout_blueprint option;
  overview_preview : bool;
  config : Octez_manager_rewards.Payout_config.t option;
  config_cursor : int;
  config_dirty : bool;
  history_cursor : int;
  loading : bool;
  error : string option;
}

val tab_index : active_tab -> int

val tab_of_index : int -> active_tab

val tab_label : active_tab -> string

val all_tabs : active_tab list

val selected_baker_instance : state -> (string * string) option

val selected_baker_pkh : state -> string option

val selected_instance_name : state -> string option

val next_sort_column : sort_column -> sort_column

val next_filter_mode : filter_mode -> filter_mode

val sort_label : sort_column -> string

val filter_label : filter_mode -> string
