(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

type payout_mode = Actual | Ideal

type signing_mode = Octez_client of {key_alias : string}

type below_min_destination = Baker_keeps | Redistribute

type delegator_override = {
  redirect_to : string option;
  custom_fee : float option;
  custom_min_balance : Int64.t option;
  max_balance_cap : Int64.t option;
  baker_pays_tx_fee : bool option;
  baker_pays_alloc_fee : bool option;
}

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

type delegator_snapshot = {
  address : string;
  delegated_balance : Int64.t;
  staked_balance : Int64.t;
}

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
  dal_rewards : Int64.t;
  other_rewards : Int64.t;
      (** VDF revelation + nonce revelation rewards. DAL attestation
          rewards are tracked separately in [dal_rewards]. *)
  block_fees : Int64.t;
  num_delegators : int;
  delegators : delegator_snapshot list;
}

type delegator_status =
  | Eligible
  | Below_minimum_payout
  | Below_minimum_balance
  | Ignored
  | Emptied
  | Override_excluded

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

type payout_result = {
  delegator : string;
  recipient : string;
  amount : Int64.t;
  op_hash : string option;
  success : bool;
  note : string;
}

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

type payout_status = Unpaid | Paid | Partial | Failed | In_progress

let total_earned (cr : cycle_rewards) =
  List.fold_left
    Int64.add
    0L
    [
      cr.block_rewards;
      cr.attestation_rewards;
      cr.dal_rewards;
      cr.other_rewards;
      cr.block_fees;
    ]

let tez_of_mutez amount_mutez =
  let s = Printf.sprintf "%Ld" amount_mutez in
  let len = String.length s in
  if len <= 6 then "0." ^ String.make (6 - len) '0' ^ s
  else String.sub s 0 (len - 6) ^ "." ^ String.sub s (len - 6) 6

let format_tez mutez =
  let tez = Int64.to_float mutez /. 1_000_000.0 in
  let s = Printf.sprintf "%.6f" tez in
  (* Add thousands separators to the integer part *)
  let parts = String.split_on_char '.' s in
  match parts with
  | [int_part; dec_part] ->
      let negative = String.length int_part > 0 && int_part.[0] = '-' in
      let digits =
        if negative then String.sub int_part 1 (String.length int_part - 1)
        else int_part
      in
      let len = String.length digits in
      let buf = Buffer.create (len + (len / 3) + 7) in
      if negative then Buffer.add_char buf '-' ;
      for i = 0 to len - 1 do
        if i > 0 && (len - i) mod 3 = 0 then Buffer.add_char buf ',' ;
        Buffer.add_char buf digits.[i]
      done ;
      Buffer.add_char buf '.' ;
      Buffer.add_string buf dec_part ;
      Buffer.contents buf
  | _ -> s

let string_of_delegator_status = function
  | Eligible -> "eligible"
  | Below_minimum_payout -> "below min payout"
  | Below_minimum_balance -> "below min balance"
  | Ignored -> "ignored"
  | Emptied -> "emptied"
  | Override_excluded -> "excluded"

let string_of_payout_mode = function Actual -> "actual" | Ideal -> "ideal"

let payout_mode_of_string = function
  | "actual" -> Some Actual
  | "ideal" -> Some Ideal
  | _ -> None

let string_of_payout_status = function
  | Unpaid -> "unpaid"
  | Paid -> "paid"
  | Partial -> "partial"
  | Failed -> "failed"
  | In_progress -> "in progress"
