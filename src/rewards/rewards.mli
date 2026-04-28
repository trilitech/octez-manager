(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Core types for the rewards & payouts engine.

    This module defines the shared data types used across the rewards
    calculation pipeline: configuration, cycle data, reward results,
    payout blueprints, execution results, and reports. *)

(** {1 Configuration types} *)

(** Payout mode determines which rewards base is used for calculations. *)
type payout_mode =
  | Actual  (** Based on real earned rewards *)
  | Ideal  (** Based on theoretical expected rewards *)

(** How payout transactions are signed. *)
type signing_mode = Octez_client of {key_alias : string}

(** Where excluded delegator rewards go. *)
type below_min_destination =
  | Baker_keeps  (** Baker retains the excess *)
  | Redistribute  (** Proportionally shared among eligible delegators *)

(** Per-delegator override of global payout settings. *)
type delegator_override = {
  redirect_to : string option;
  custom_fee : float option;
  custom_min_balance : Int64.t option;
  max_balance_cap : Int64.t option;
  baker_pays_tx_fee : bool option;
  baker_pays_alloc_fee : bool option;
}

(** Notification channel for post-payout alerts. *)
type notification_channel =
  | Discord of {webhook_url : string; message_template : string; admin : bool}
  | Telegram of {
      api_token : string;
      receivers : int list;
      message_template : string;
    }
  | Webhook of {url : string; auth : webhook_auth}
  | External of {path : string; args : string list}

and webhook_auth = No_auth | Bearer of string

(** {1 Cycle data types} *)

(** Per-delegator balance at the cycle snapshot point. *)
type delegator_snapshot = {
  address : string;
  delegated_balance : Int64.t;
  staked_balance : Int64.t;
}

(** Cycle-level reward data fetched from TzKT. *)
type cycle_rewards = {
  cycle : int;
  baker : string;
  staking_balance : Int64.t;
  delegated_balance : Int64.t;
  own_staked_balance : Int64.t;
  own_delegated_balance : Int64.t;
  external_staked_balance : Int64.t;
  external_delegated_balance : Int64.t;
  block_rewards : Int64.t;
  attestation_rewards : Int64.t;
  dal_rewards : Int64.t;  (** DAL attestation rewards. *)
  other_rewards : Int64.t;
      (** VDF revelation + nonce revelation rewards. DAL attestation
          rewards are tracked separately in {!field-dal_rewards}. *)
  block_fees : Int64.t;
  num_delegators : int;
  delegators : delegator_snapshot list;
}

(** {1 Reward calculation types} *)

(** Eligibility status for a delegator within a given cycle. *)
type delegator_status =
  | Eligible
  | Below_minimum_payout
  | Below_minimum_balance
  | Ignored
  | Emptied
  | Override_excluded

(** Calculated reward for a single delegator. *)
type delegator_reward = {
  delegator : string;
  delegated_balance : Int64.t;
  staked_balance : Int64.t;
  gross_reward : Int64.t;
  fee_rate : float;
  fee_amount : Int64.t;
  net_reward : Int64.t;
  recipient : string;
  status : delegator_status;
}

(** {1 Payout blueprint types} *)

(** Pre-execution preview of a cycle's reward distribution. *)
type payout_blueprint = {
  cycle : int;
  baker : string;
  network : string;
  earned_rewards : Int64.t;
  earned_block_fees : Int64.t;
  total_delegators : int;
  eligible_delegators : int;
  delegator_rewards : delegator_reward list;
  baker_bond_income : Int64.t;
  baker_fee_income : Int64.t;
  estimated_tx_fees : Int64.t;
  bond_payouts : (string * Int64.t) list;
  fee_payouts : (string * Int64.t) list;
}

(** {1 Execution types} *)

(** Per-delegator payout execution outcome. *)
type payout_result = {
  delegator : string;
  recipient : string;
  amount : Int64.t;
  op_hash : string option;
  success : bool;
  note : string;
}

(** {1 Report types} *)

(** Post-execution summary persisted as summary.json. *)
type cycle_summary = {
  cycle : int;
  delegators : int;
  paid_delegators : int;
  own_staked_balance : Int64.t;
  own_delegated_balance : Int64.t;
  external_staked_balance : Int64.t;
  external_delegated_balance : Int64.t;
  earned_rewards : Int64.t;
  earned_block_fees : Int64.t;
  distributed_rewards : Int64.t;
  bond_income : Int64.t;
  fee_income : Int64.t;
  tx_fees_paid : Int64.t;
  timestamp : string;
}

(** {1 Payout status} *)

(** Status of a cycle's payout. *)
type payout_status = Unpaid | Paid | Partial | Failed | In_progress

(** Total earned rewards for a cycle (block + attestation + other + fees). *)
val total_earned : cycle_rewards -> Int64.t

(** Convert a mutez amount to a plain tez decimal string (e.g., "1234.567890").
    Suitable for octez-client and machine-readable output. *)
val tez_of_mutez : Int64.t -> string

(** {1 Formatting helpers} *)

(** Format a mutez amount as a human-readable tez string (e.g., "1,234.567890"). *)
val format_tez : Int64.t -> string

(** Format a delegator status as a short label. *)
val string_of_delegator_status : delegator_status -> string

(** Format a payout mode as a string. *)
val string_of_payout_mode : payout_mode -> string

(** Parse a payout mode from a string. *)
val payout_mode_of_string : string -> payout_mode option

(** Format a payout status as a short label. *)
val string_of_payout_status : payout_status -> string
