(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Page state types for the Rewards page. *)

open Octez_manager_rewards

type active_tab = Overview | Delegators | Configuration

type sort_column = SortAddress | SortBalance | SortReward | SortStatus

type filter_mode =
  | FilterAll
  | FilterEligible
  | FilterExcluded
  | FilterBelowMin

type state = {
  baker_instances : (string * string) list;
      (** [(instance_name, baker_pkh)] pairs *)
  custom_baker_instances : string list;
      (** Subset of [baker_instances] sourced from the custom-baker registry.
          Cached at init/refresh time so view functions can identify custom
          bakers without performing I/O. *)
  selected_baker : int;  (** Index into [baker_instances] *)
  active_tab : active_tab;
  selected_cycle : int option;  (** Currently viewed cycle (None = latest) *)
  current_cycle : int option;  (** Current protocol cycle *)
  delegator_cursor : int;
  delegator_sort : sort_column;
  delegator_filter : filter_mode;
  search_query : string;
  search_active : bool;
  blueprint : Rewards.payout_blueprint option;
      (** Loaded blueprint for delegators/overview tabs *)
  overview_preview : bool;
      (** True when the overview tab should display the payout preview *)
  config : Payout_config.t option;  (** Loaded payout config for editing *)
  config_cursor : int;  (** Selected field in config tab *)
  config_dirty : bool;  (** True if config has unsaved changes *)
  config_exists : bool;
      (** True if a saved config file exists on disk for the selected baker *)
  cycle_cursor : int;
      (** Cursor index into the Overview's Recent Cycles table. *)
  loading : bool;
  error : string option;
}

let tab_index = function Overview -> 0 | Delegators -> 1 | Configuration -> 2

let tab_of_index = function
  | 0 -> Overview
  | 1 -> Delegators
  | 2 -> Configuration
  | _ -> Overview

let tab_label = function
  | Overview -> "Overview"
  | Delegators -> "Delegators"
  | Configuration -> "Configuration"

let all_tabs = [Overview; Delegators; Configuration]

let selected_baker_instance st =
  List.nth_opt st.baker_instances st.selected_baker

let selected_baker_pkh st =
  match selected_baker_instance st with
  | Some (_, pkh) -> Some pkh
  | None -> None

let selected_instance_name st =
  match selected_baker_instance st with
  | Some (name, _) -> Some name
  | None -> None

let selected_baker_is_custom st =
  match selected_instance_name st with
  | None -> false
  | Some inst -> List.exists (String.equal inst) st.custom_baker_instances

let next_sort_column = function
  | SortBalance -> SortReward
  | SortReward -> SortAddress
  | SortAddress -> SortStatus
  | SortStatus -> SortBalance

let next_filter_mode = function
  | FilterAll -> FilterEligible
  | FilterEligible -> FilterExcluded
  | FilterExcluded -> FilterBelowMin
  | FilterBelowMin -> FilterAll

let sort_label = function
  | SortAddress -> "Address"
  | SortBalance -> "Balance"
  | SortReward -> "Reward"
  | SortStatus -> "Status"

let filter_label = function
  | FilterAll -> "All"
  | FilterEligible -> "Eligible"
  | FilterExcluded -> "Excluded"
  | FilterBelowMin -> "Below Min"
