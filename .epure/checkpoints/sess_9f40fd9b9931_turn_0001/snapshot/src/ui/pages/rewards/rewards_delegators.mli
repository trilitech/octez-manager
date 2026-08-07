(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Delegators tab rendering for the Rewards page. *)

(** Render the delegators tab content. No I/O — reads from page state. *)
val render : state:Rewards_state.state -> cols:int -> rows:int -> string

(** Filter a delegator list by status. *)
val apply_filter :
  Rewards_state.filter_mode ->
  Octez_manager_rewards.Rewards.delegator_reward list ->
  Octez_manager_rewards.Rewards.delegator_reward list

(** Filter a delegator list by search query (address substring match). *)
val apply_search :
  string ->
  Octez_manager_rewards.Rewards.delegator_reward list ->
  Octez_manager_rewards.Rewards.delegator_reward list
