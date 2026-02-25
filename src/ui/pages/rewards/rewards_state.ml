(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Page state types for the Rewards page. *)

open Octez_manager_rewards

type active_tab = Overview | Delegators | History | Configuration

type sort_column = SortAddress | SortBalance | SortReward | SortStatus

type filter_mode =
  | FilterAll
  | FilterEligible
  | FilterExcluded
  | FilterBelowMin

type state = {
  baker_instances : (string * string) list;
      (** [(instance_name, baker_pkh)] pairs *)
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
      (** Loaded blueprint for delegators tab *)
  history_cursor : int;
  loading : bool;
  error : string option;
}

let tab_index = function
  | Overview -> 0
  | Delegators -> 1
  | History -> 2
  | Configuration -> 3

let tab_of_index = function
  | 0 -> Overview
  | 1 -> Delegators
  | 2 -> History
  | 3 -> Configuration
  | _ -> Overview

let tab_label = function
  | Overview -> "Overview"
  | Delegators -> "Delegators"
  | History -> "History"
  | Configuration -> "Configuration"

let all_tabs = [Overview; Delegators; History; Configuration]

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
