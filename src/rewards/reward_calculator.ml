(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Estimate transaction fee in mutez for a single transfer. *)
let estimated_tx_fee = 400L

let get_override config delegator =
  List.assoc_opt delegator config.Payout_config.delegator_overrides

let is_in_list addr = List.exists (String.equal addr)

let delegator_status config delegator ~balance ~net_reward =
  let has_whitelist = config.Payout_config.whitelist <> [] in
  let has_blacklist = config.Payout_config.blacklist <> [] in
  if has_blacklist && is_in_list delegator config.blacklist then Rewards.Ignored
  else if has_whitelist && not (is_in_list delegator config.whitelist) then
    Rewards.Ignored
  else if config.ignore_contracts && String.starts_with ~prefix:"KT1" delegator
  then Rewards.Ignored
  else if balance < config.min_balance then Rewards.Below_minimum_balance
  else if net_reward < config.min_payout then Rewards.Below_minimum_payout
  else Rewards.Eligible

let generate_blueprint ~config ~network ~cycle_rewards =
  let cr = cycle_rewards in
  let total_rewards =
    List.fold_left
      Int64.add
      0L
      [
        Rewards.total_of_split cr.Rewards.block_rewards;
        Rewards.total_of_split cr.Rewards.attestation_rewards;
        Rewards.total_of_split cr.Rewards.dal_rewards;
        Rewards.total_of_split cr.Rewards.vdf_rewards;
        Rewards.total_of_split cr.Rewards.nonce_rewards;
        cr.Rewards.block_fees;
      ]
  in
  (* Compute the effective staking balance for overdelegation protection.
     The limit is 9x the baker's own staked balance. *)
  let own_stake = cr.own_staked_balance in
  let max_external =
    if config.Payout_config.overdelegation_protect then
      Int64.sub (Int64.mul own_stake 9L) own_stake
    else Int64.max_int
  in
  (* Sum of all delegator effective balances *)
  let delegator_total_balance =
    List.fold_left
      (fun acc (d : Rewards.delegator_snapshot) ->
        Int64.add acc (Int64.add d.delegated_balance d.staked_balance))
      0L
      cr.delegators
  in
  (* Total balance includes baker's own *)
  let baker_own_balance =
    Int64.add cr.own_staked_balance cr.own_delegated_balance
  in
  let total_staking = Int64.add baker_own_balance delegator_total_balance in
  (* Compute per-delegator rewards *)
  let raw_rewards =
    List.map
      (fun (d : Rewards.delegator_snapshot) ->
        let balance = Int64.add d.delegated_balance d.staked_balance in
        (* Cap for overdelegation *)
        let effective_balance =
          if
            config.overdelegation_protect
            && delegator_total_balance > max_external
          then
            (* Scale each delegator proportionally *)
            let ratio =
              Int64.to_float max_external
              /. Int64.to_float delegator_total_balance
            in
            Int64.of_float (Int64.to_float balance *. ratio)
          else balance
        in
        (* Proportional share *)
        let gross_reward =
          if total_staking = 0L then 0L
          else
            Int64.of_float
              (Int64.to_float total_rewards
              *. (Int64.to_float effective_balance
                 /. Int64.to_float total_staking))
        in
        (* Fee rate: per-delegator override takes precedence *)
        let override = get_override config d.address in
        let fee_rate =
          match override with
          | Some ov -> (
              match ov.Rewards.custom_fee with
              | Some f -> f
              | None -> config.baker_fee)
          | None -> config.baker_fee
        in
        let fee_amount =
          Int64.of_float (Int64.to_float gross_reward *. fee_rate)
        in
        let net_reward = Int64.sub gross_reward fee_amount in
        (* Recipient: override redirect takes precedence *)
        let recipient =
          match override with
          | Some ov -> (
              match ov.redirect_to with Some addr -> addr | None -> d.address)
          | None -> d.address
        in
        (* Status check *)
        let status = delegator_status config d.address ~balance ~net_reward in
        {
          Rewards.delegator = d.address;
          delegated_balance = d.delegated_balance;
          staked_balance = d.staked_balance;
          gross_reward;
          fee_rate;
          fee_amount;
          net_reward;
          recipient;
          status;
        })
      cr.delegators
  in
  (* Redistribute below-minimum rewards if configured *)
  let eligible =
    List.filter
      (fun (r : Rewards.delegator_reward) ->
        match r.status with Rewards.Eligible -> true | _ -> false)
      raw_rewards
  in
  let excluded =
    List.filter
      (fun (r : Rewards.delegator_reward) ->
        match r.status with Rewards.Eligible -> false | _ -> true)
      raw_rewards
  in
  let excluded_total =
    List.fold_left
      (fun acc (r : Rewards.delegator_reward) -> Int64.add acc r.net_reward)
      0L
      excluded
  in
  let delegator_rewards =
    match config.below_min_dest with
    | Rewards.Redistribute when excluded_total > 0L && eligible <> [] ->
        (* Redistribute excluded amounts proportionally to eligible *)
        let eligible_total_net =
          List.fold_left
            (fun acc (r : Rewards.delegator_reward) ->
              Int64.add acc r.net_reward)
            0L
            eligible
        in
        let boosted =
          List.map
            (fun (r : Rewards.delegator_reward) ->
              if eligible_total_net > 0L then
                let share =
                  Int64.to_float r.net_reward
                  /. Int64.to_float eligible_total_net
                in
                let bonus =
                  Int64.of_float (Int64.to_float excluded_total *. share)
                in
                {r with net_reward = Int64.add r.net_reward bonus}
              else r)
            eligible
        in
        let zeroed =
          List.map
            (fun (r : Rewards.delegator_reward) -> {r with net_reward = 0L})
            excluded
        in
        boosted @ zeroed
    | _ -> raw_rewards
  in
  let eligible_delegators =
    List.length
      (List.filter
         (fun (r : Rewards.delegator_reward) ->
           match r.status with Rewards.Eligible -> true | _ -> false)
         delegator_rewards)
  in
  (* Baker income: own share + fees *)
  let baker_share =
    if total_staking = 0L then total_rewards
    else
      Int64.of_float
        (Int64.to_float total_rewards
        *. (Int64.to_float baker_own_balance /. Int64.to_float total_staking))
  in
  let total_fees =
    List.fold_left
      (fun acc (r : Rewards.delegator_reward) -> Int64.add acc r.fee_amount)
      0L
      delegator_rewards
  in
  (* Bond/fee recipient payouts *)
  let bond_payouts =
    List.map
      (fun (addr, share) ->
        (addr, Int64.of_float (Int64.to_float baker_share *. share)))
      config.bond_recipients
  in
  let fee_payouts =
    List.map
      (fun (addr, share) ->
        (addr, Int64.of_float (Int64.to_float total_fees *. share)))
      config.fee_recipients
  in
  let est_tx_fees =
    Int64.mul estimated_tx_fee (Int64.of_int eligible_delegators)
  in
  {
    Rewards.cycle = cr.cycle;
    baker = cr.baker;
    network;
    earned_rewards = total_rewards;
    earned_block_fees = cr.block_fees;
    total_delegators = cr.num_delegators;
    eligible_delegators;
    delegator_rewards;
    baker_bond_income = baker_share;
    baker_fee_income = total_fees;
    estimated_tx_fees = est_tx_fees;
    bond_payouts;
    fee_payouts;
  }
