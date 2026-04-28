(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Octez_manager_rewards

let baker_pkh = "tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU"

let addr_a = "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb"

let addr_b = "tz1aSkwEot3L2kmUvcoxzjMomb9LTQjTBKt2"

let addr_c = "tz1burnburnburnburnburnburnburjAYjjX"

let default_config () =
  {(Payout_config.default ~baker_pkh) with overdelegation_protect = false}

let zero_split : Rewards.reward_split =
  {delegated = 0L; staked_own = 0L; staked_edge = 0L; staked_shared = 0L}

let delegated_only amount : Rewards.reward_split =
  {delegated = amount; staked_own = 0L; staked_edge = 0L; staked_shared = 0L}

let make_cycle_rewards ?(cycle = 100) ?(own_staked = 1_000_000_000L)
    ?(own_delegated = 0L) ?(block_rewards = 10_000_000L)
    ?(block_fees = 500_000L) delegators =
  {
    Rewards.cycle;
    baker = baker_pkh;
    staking_balance = 0L;
    delegated_balance = 0L;
    own_staked_balance = own_staked;
    own_delegated_balance = own_delegated;
    external_staked_balance = 0L;
    external_delegated_balance = 0L;
    block_rewards = delegated_only block_rewards;
    attestation_rewards = zero_split;
    dal_rewards = zero_split;
    vdf_rewards = zero_split;
    nonce_rewards = zero_split;
    block_fees;
    num_delegators = List.length delegators;
    delegators;
  }

let make_delegator ?(staked = 0L) addr balance =
  {Rewards.address = addr; delegated_balance = balance; staked_balance = staked}

(* {1 Proportional share tests} *)

let test_single_delegator_proportional_share () =
  let config = default_config () in
  (* Baker has no own_delegated, so the lone delegator captures the whole
     delegated pool (block_rewards.delegated + block_fees). *)
  let cr =
    make_cycle_rewards
      ~own_staked:1_000_000_000L
      [make_delegator addr_a 1_000_000_000L]
  in
  let bp =
    Reward_calculator.generate_blueprint
      ~config
      ~network:"ghostnet"
      ~cycle_rewards:cr
  in
  Alcotest.(check int) "total delegators" 1 bp.total_delegators ;
  Alcotest.(check int) "eligible delegators" 1 bp.eligible_delegators ;
  let expected_gross = Int64.add cr.block_rewards.delegated cr.block_fees in
  let dr = List.hd bp.delegator_rewards in
  let diff = Int64.abs (Int64.sub dr.gross_reward expected_gross) in
  Alcotest.(check bool) "lone delegator captures pool" true (diff <= 1L)

let test_two_delegators_proportional () =
  let config = {(default_config ()) with baker_fee = 0.0} in
  let cr =
    make_cycle_rewards
      ~own_staked:0L
      ~block_rewards:10_000_000L
      ~block_fees:0L
      [make_delegator addr_a 3_000_000L; make_delegator addr_b 7_000_000L]
  in
  let bp =
    Reward_calculator.generate_blueprint
      ~config
      ~network:"ghostnet"
      ~cycle_rewards:cr
  in
  let find addr =
    List.find
      (fun (r : Rewards.delegator_reward) -> String.equal r.delegator addr)
      bp.delegator_rewards
  in
  let ra = find addr_a in
  let rb = find addr_b in
  (* addr_a has 30%, addr_b has 70% *)
  let diff_a = Int64.abs (Int64.sub ra.net_reward 3_000_000L) in
  let diff_b = Int64.abs (Int64.sub rb.net_reward 7_000_000L) in
  Alcotest.(check bool) "addr_a ~30%" true (diff_a <= 1L) ;
  Alcotest.(check bool) "addr_b ~70%" true (diff_b <= 1L)

(* {1 Fee application} *)

let test_global_fee_application () =
  let config = {(default_config ()) with baker_fee = 0.10} in
  let cr =
    make_cycle_rewards
      ~own_staked:0L
      ~block_rewards:10_000_000L
      ~block_fees:0L
      [make_delegator addr_a 10_000_000L]
  in
  let bp =
    Reward_calculator.generate_blueprint
      ~config
      ~network:"ghostnet"
      ~cycle_rewards:cr
  in
  let dr = List.hd bp.delegator_rewards in
  Alcotest.(check bool) "fee_rate is 10%" true (Float.equal dr.fee_rate 0.10) ;
  (* gross = 10M, fee = 1M, net = 9M *)
  let fee_diff = Int64.abs (Int64.sub dr.fee_amount 1_000_000L) in
  let net_diff = Int64.abs (Int64.sub dr.net_reward 9_000_000L) in
  Alcotest.(check bool) "fee ~1M" true (fee_diff <= 1L) ;
  Alcotest.(check bool) "net ~9M" true (net_diff <= 1L)

let test_per_delegator_fee_override () =
  let config =
    {
      (default_config ()) with
      baker_fee = 0.10;
      delegator_overrides =
        [
          ( addr_a,
            {
              Rewards.redirect_to = None;
              custom_fee = Some 0.02;
              custom_min_balance = None;
              max_balance_cap = None;
              baker_pays_tx_fee = None;
              baker_pays_alloc_fee = None;
            } );
        ];
    }
  in
  let cr =
    make_cycle_rewards
      ~own_staked:0L
      ~block_rewards:10_000_000L
      ~block_fees:0L
      [make_delegator addr_a 5_000_000L; make_delegator addr_b 5_000_000L]
  in
  let bp =
    Reward_calculator.generate_blueprint
      ~config
      ~network:"ghostnet"
      ~cycle_rewards:cr
  in
  let find addr =
    List.find
      (fun (r : Rewards.delegator_reward) -> String.equal r.delegator addr)
      bp.delegator_rewards
  in
  let ra = find addr_a in
  let rb = find addr_b in
  (* addr_a has custom 2% fee, addr_b has global 10% fee *)
  Alcotest.(check bool) "addr_a custom fee" true (Float.equal ra.fee_rate 0.02) ;
  Alcotest.(check bool) "addr_b global fee" true (Float.equal rb.fee_rate 0.10)

(* {1 Overdelegation cap} *)

let test_overdelegation_cap () =
  (* Baker has 1M staked. Overdelegation limit is 9x = 9M external.
     But we have 18M delegated, so each delegator gets scaled down by 50%. *)
  let config =
    {(default_config ()) with baker_fee = 0.0; overdelegation_protect = true}
  in
  let cr =
    make_cycle_rewards
      ~own_staked:1_000_000L
      ~block_rewards:10_000_000L
      ~block_fees:0L
      [make_delegator addr_a 9_000_000L; make_delegator addr_b 9_000_000L]
  in
  let bp =
    Reward_calculator.generate_blueprint
      ~config
      ~network:"ghostnet"
      ~cycle_rewards:cr
  in
  let find addr =
    List.find
      (fun (r : Rewards.delegator_reward) -> String.equal r.delegator addr)
      bp.delegator_rewards
  in
  let ra = find addr_a in
  let rb = find addr_b in
  (* Each delegator should get less than they would without the cap.
     Without cap: each gets 9/19 * 10M = 4,736,842.
     With cap: effective external is 8M (9x - own), so each gets
     4M effective out of total 1M+8M = 9M staking.
     Each: 4M/9M * 10M = 4,444,444 *)
  Alcotest.(check bool)
    "overdelegation reduces rewards"
    true
    (ra.net_reward < 4_800_000L) ;
  (* Both get equal share since they're the same size *)
  let diff = Int64.abs (Int64.sub ra.net_reward rb.net_reward) in
  Alcotest.(check bool) "equal share" true (diff <= 1L)

let test_no_overdelegation_when_disabled () =
  let config =
    {(default_config ()) with baker_fee = 0.0; overdelegation_protect = false}
  in
  let cr =
    make_cycle_rewards
      ~own_staked:1_000_000L
      ~block_rewards:10_000_000L
      ~block_fees:0L
      [make_delegator addr_a 18_000_000L]
  in
  let bp =
    Reward_calculator.generate_blueprint
      ~config
      ~network:"ghostnet"
      ~cycle_rewards:cr
  in
  let dr = List.hd bp.delegator_rewards in
  (* Without cap: addr_a gets 18M / (1M + 18M) * 10M = 9,473,684 *)
  Alcotest.(check bool) "no cap applied" true (dr.net_reward > 9_000_000L)

(* {1 Eligibility filtering} *)

let test_min_balance_filter () =
  let config =
    {(default_config ()) with baker_fee = 0.0; min_balance = 5_000_000L}
  in
  let cr =
    make_cycle_rewards
      ~own_staked:0L
      [make_delegator addr_a 10_000_000L; make_delegator addr_b 1_000_000L]
  in
  let bp =
    Reward_calculator.generate_blueprint
      ~config
      ~network:"ghostnet"
      ~cycle_rewards:cr
  in
  let find addr =
    List.find
      (fun (r : Rewards.delegator_reward) -> String.equal r.delegator addr)
      bp.delegator_rewards
  in
  let ra = find addr_a in
  let rb = find addr_b in
  Alcotest.(check string)
    "addr_a eligible"
    "eligible"
    (Rewards.string_of_delegator_status ra.status) ;
  Alcotest.(check string)
    "addr_b below min balance"
    "below min balance"
    (Rewards.string_of_delegator_status rb.status)

let test_min_payout_filter () =
  let config =
    {(default_config ()) with baker_fee = 0.0; min_payout = 5_000_000L}
  in
  let cr =
    make_cycle_rewards
      ~own_staked:0L
      ~block_rewards:10_000_000L
      ~block_fees:0L
      [make_delegator addr_a 9_000_000L; make_delegator addr_b 1_000_000L]
  in
  let bp =
    Reward_calculator.generate_blueprint
      ~config
      ~network:"ghostnet"
      ~cycle_rewards:cr
  in
  let find addr =
    List.find
      (fun (r : Rewards.delegator_reward) -> String.equal r.delegator addr)
      bp.delegator_rewards
  in
  let ra = find addr_a in
  let rb = find addr_b in
  Alcotest.(check string)
    "addr_a eligible"
    "eligible"
    (Rewards.string_of_delegator_status ra.status) ;
  Alcotest.(check string)
    "addr_b below min payout"
    "below min payout"
    (Rewards.string_of_delegator_status rb.status)

let test_blacklist_filter () =
  let config =
    {(default_config ()) with baker_fee = 0.0; blacklist = [addr_b]}
  in
  let cr =
    make_cycle_rewards
      ~own_staked:0L
      [make_delegator addr_a 5_000_000L; make_delegator addr_b 5_000_000L]
  in
  let bp =
    Reward_calculator.generate_blueprint
      ~config
      ~network:"ghostnet"
      ~cycle_rewards:cr
  in
  let find addr =
    List.find
      (fun (r : Rewards.delegator_reward) -> String.equal r.delegator addr)
      bp.delegator_rewards
  in
  let ra = find addr_a in
  let rb = find addr_b in
  Alcotest.(check string)
    "addr_a eligible"
    "eligible"
    (Rewards.string_of_delegator_status ra.status) ;
  Alcotest.(check string)
    "addr_b ignored"
    "ignored"
    (Rewards.string_of_delegator_status rb.status)

let test_whitelist_filter () =
  let config =
    {(default_config ()) with baker_fee = 0.0; whitelist = [addr_a]}
  in
  let cr =
    make_cycle_rewards
      ~own_staked:0L
      [make_delegator addr_a 5_000_000L; make_delegator addr_b 5_000_000L]
  in
  let bp =
    Reward_calculator.generate_blueprint
      ~config
      ~network:"ghostnet"
      ~cycle_rewards:cr
  in
  let find addr =
    List.find
      (fun (r : Rewards.delegator_reward) -> String.equal r.delegator addr)
      bp.delegator_rewards
  in
  let ra = find addr_a in
  let rb = find addr_b in
  Alcotest.(check string)
    "addr_a eligible"
    "eligible"
    (Rewards.string_of_delegator_status ra.status) ;
  Alcotest.(check string)
    "addr_b ignored (not in whitelist)"
    "ignored"
    (Rewards.string_of_delegator_status rb.status)

let test_ignore_contracts_filter () =
  let kt_addr = "KT1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU" in
  let config =
    {(default_config ()) with baker_fee = 0.0; ignore_contracts = true}
  in
  let cr =
    make_cycle_rewards
      ~own_staked:0L
      [make_delegator addr_a 5_000_000L; make_delegator kt_addr 5_000_000L]
  in
  let bp =
    Reward_calculator.generate_blueprint
      ~config
      ~network:"ghostnet"
      ~cycle_rewards:cr
  in
  let find addr =
    List.find
      (fun (r : Rewards.delegator_reward) -> String.equal r.delegator addr)
      bp.delegator_rewards
  in
  let ra = find addr_a in
  let rkt = find kt_addr in
  Alcotest.(check string)
    "tz addr eligible"
    "eligible"
    (Rewards.string_of_delegator_status ra.status) ;
  Alcotest.(check string)
    "KT addr ignored"
    "ignored"
    (Rewards.string_of_delegator_status rkt.status)

(* {1 Below-min redistribution} *)

let test_redistribute_below_min () =
  let config =
    {
      (default_config ()) with
      baker_fee = 0.0;
      min_payout = 2_000_000L;
      below_min_dest = Rewards.Redistribute;
    }
  in
  let cr =
    make_cycle_rewards
      ~own_staked:0L
      ~block_rewards:10_000_000L
      ~block_fees:0L
      [make_delegator addr_a 9_000_000L; make_delegator addr_b 1_000_000L]
  in
  let bp =
    Reward_calculator.generate_blueprint
      ~config
      ~network:"ghostnet"
      ~cycle_rewards:cr
  in
  let find addr =
    List.find
      (fun (r : Rewards.delegator_reward) -> String.equal r.delegator addr)
      bp.delegator_rewards
  in
  let ra = find addr_a in
  let rb = find addr_b in
  (* addr_b's reward (1M) is below min_payout (2M), so it gets zeroed and
     redistributed to addr_a *)
  Alcotest.(check bool) "addr_b zeroed" true (Int64.equal rb.net_reward 0L) ;
  (* addr_a should get more than their original 9M share *)
  Alcotest.(check bool) "addr_a gets bonus" true (ra.net_reward > 9_000_000L)

let test_baker_keeps_below_min () =
  let config =
    {
      (default_config ()) with
      baker_fee = 0.0;
      min_payout = 2_000_000L;
      below_min_dest = Rewards.Baker_keeps;
    }
  in
  let cr =
    make_cycle_rewards
      ~own_staked:0L
      ~block_rewards:10_000_000L
      ~block_fees:0L
      [make_delegator addr_a 9_000_000L; make_delegator addr_b 1_000_000L]
  in
  let bp =
    Reward_calculator.generate_blueprint
      ~config
      ~network:"ghostnet"
      ~cycle_rewards:cr
  in
  let find addr =
    List.find
      (fun (r : Rewards.delegator_reward) -> String.equal r.delegator addr)
      bp.delegator_rewards
  in
  let ra = find addr_a in
  let rb = find addr_b in
  (* Baker keeps mode: below-min rewards stay as-is (not zeroed) *)
  Alcotest.(check bool) "addr_b keeps reward" true (rb.net_reward > 0L) ;
  (* addr_a doesn't get redistributed bonus *)
  let diff = Int64.abs (Int64.sub ra.net_reward 9_000_000L) in
  Alcotest.(check bool) "addr_a ~9M" true (diff <= 1L)

(* {1 Edge cases} *)

let test_zero_delegators () =
  let config = default_config () in
  let cr = make_cycle_rewards ~own_staked:1_000_000L [] in
  let bp =
    Reward_calculator.generate_blueprint
      ~config
      ~network:"ghostnet"
      ~cycle_rewards:cr
  in
  Alcotest.(check int) "no delegators" 0 bp.total_delegators ;
  Alcotest.(check int) "no eligible" 0 bp.eligible_delegators ;
  Alcotest.(check (list pass)) "empty rewards" [] bp.delegator_rewards

let test_zero_rewards () =
  let config = {(default_config ()) with baker_fee = 0.0} in
  let cr =
    make_cycle_rewards
      ~own_staked:1_000_000L
      ~block_rewards:0L
      ~block_fees:0L
      [make_delegator addr_a 1_000_000L]
  in
  let bp =
    Reward_calculator.generate_blueprint
      ~config
      ~network:"ghostnet"
      ~cycle_rewards:cr
  in
  let dr = List.hd bp.delegator_rewards in
  Alcotest.(check bool)
    "zero gross reward"
    true
    (Int64.equal dr.gross_reward 0L) ;
  Alcotest.(check bool) "zero net reward" true (Int64.equal dr.net_reward 0L)

let test_redirect_override () =
  let config =
    {
      (default_config ()) with
      baker_fee = 0.0;
      delegator_overrides =
        [
          ( addr_a,
            {
              Rewards.redirect_to = Some addr_c;
              custom_fee = None;
              custom_min_balance = None;
              max_balance_cap = None;
              baker_pays_tx_fee = None;
              baker_pays_alloc_fee = None;
            } );
        ];
    }
  in
  let cr =
    make_cycle_rewards ~own_staked:0L [make_delegator addr_a 10_000_000L]
  in
  let bp =
    Reward_calculator.generate_blueprint
      ~config
      ~network:"ghostnet"
      ~cycle_rewards:cr
  in
  let dr = List.hd bp.delegator_rewards in
  Alcotest.(check string) "redirected" addr_c dr.recipient

let test_baker_income_and_fees () =
  let config = {(default_config ()) with baker_fee = 0.10} in
  (* Baker contributes 5M to total_delegated via own_delegated; addr_a
     contributes 5M as a delegator. With pool=10M and total_delegated=10M,
     baker keeps 5M and addr_a's gross is 5M (fee 10% = 500K). *)
  let cr =
    make_cycle_rewards
      ~own_staked:0L
      ~own_delegated:5_000_000L
      ~block_rewards:10_000_000L
      ~block_fees:0L
      [make_delegator addr_a 5_000_000L]
  in
  let bp =
    Reward_calculator.generate_blueprint
      ~config
      ~network:"ghostnet"
      ~cycle_rewards:cr
  in
  let bond_diff = Int64.abs (Int64.sub bp.baker_bond_income 5_000_000L) in
  Alcotest.(check bool) "baker bond income ~5M" true (bond_diff <= 1L) ;
  let fee_diff = Int64.abs (Int64.sub bp.baker_fee_income 500_000L) in
  Alcotest.(check bool) "baker fee income ~500K" true (fee_diff <= 1L)

let test_bond_fee_recipient_payouts () =
  let config =
    {
      (default_config ()) with
      baker_fee = 0.10;
      bond_recipients = [(addr_b, 0.5)];
      fee_recipients = [(addr_c, 1.0)];
    }
  in
  let cr =
    make_cycle_rewards
      ~own_staked:0L
      ~own_delegated:5_000_000L
      ~block_rewards:10_000_000L
      ~block_fees:0L
      [make_delegator addr_a 5_000_000L]
  in
  let bp =
    Reward_calculator.generate_blueprint
      ~config
      ~network:"ghostnet"
      ~cycle_rewards:cr
  in
  (* Bond recipient gets 50% of baker_bond_income (~5M) = ~2.5M *)
  Alcotest.(check int) "one bond payout" 1 (List.length bp.bond_payouts) ;
  let _, bond_amt = List.hd bp.bond_payouts in
  let bond_diff = Int64.abs (Int64.sub bond_amt 2_500_000L) in
  Alcotest.(check bool) "bond payout ~2.5M" true (bond_diff <= 1L) ;
  (* Fee recipient gets 100% of baker_fee_income (~500K) *)
  Alcotest.(check int) "one fee payout" 1 (List.length bp.fee_payouts) ;
  let _, fee_amt = List.hd bp.fee_payouts in
  let fee_diff = Int64.abs (Int64.sub fee_amt 500_000L) in
  Alcotest.(check bool) "fee payout ~500K" true (fee_diff <= 1L)

let test_estimated_tx_fees () =
  let config = default_config () in
  let cr =
    make_cycle_rewards
      ~own_staked:0L
      [make_delegator addr_a 5_000_000L; make_delegator addr_b 5_000_000L]
  in
  let bp =
    Reward_calculator.generate_blueprint
      ~config
      ~network:"ghostnet"
      ~cycle_rewards:cr
  in
  (* 2 eligible delegators × 400 mutez estimated fee *)
  Alcotest.(check bool)
    "tx fees for 2 delegators"
    true
    (Int64.equal bp.estimated_tx_fees 800L)

(* {1 Bucket-aware attribution} *)

(** Build a cycle with the given per-bucket reward splits. Used by the
    bucket-aware tests below to drive specific protocol buckets
    independently of each other. *)
let make_cycle_buckets ?(cycle = 100) ?(own_staked = 0L) ?(own_delegated = 0L)
    ?(block_rewards = zero_split) ?(attestation_rewards = zero_split)
    ?(dal_rewards = zero_split) ?(vdf_rewards = zero_split)
    ?(nonce_rewards = zero_split) ?(block_fees = 0L) delegators =
  {
    Rewards.cycle;
    baker = baker_pkh;
    staking_balance = 0L;
    delegated_balance = 0L;
    own_staked_balance = own_staked;
    own_delegated_balance = own_delegated;
    external_staked_balance = 0L;
    external_delegated_balance = 0L;
    block_rewards;
    attestation_rewards;
    dal_rewards;
    vdf_rewards;
    nonce_rewards;
    block_fees;
    num_delegators = List.length delegators;
    delegators;
  }

let split ?(delegated = 0L) ?(staked_own = 0L) ?(staked_edge = 0L)
    ?(staked_shared = 0L) () : Rewards.reward_split =
  {delegated; staked_own; staked_edge; staked_shared}

let test_pure_delegator_only_delegated_pool () =
  (* Delegator with delegated_balance > 0, staked_balance = 0. Cycle has
     non-zero values in every staking sub-field of every reward type.
     The delegator's gross must reflect only the [delegated] sub-fields
     of block, attestation, and DAL — never the staking sub-fields. *)
  let config = {(default_config ()) with baker_fee = 0.0} in
  let cr =
    make_cycle_buckets
      ~own_staked:0L
      ~own_delegated:0L
      ~block_rewards:
        (split
           ~delegated:1_000_000L
           ~staked_own:9_000_000L
           ~staked_edge:5_000_000L
           ~staked_shared:7_000_000L
           ())
      ~attestation_rewards:
        (split
           ~delegated:2_000_000L
           ~staked_own:8_000_000L
           ~staked_edge:4_000_000L
           ~staked_shared:6_000_000L
           ())
      ~dal_rewards:
        (split ~delegated:500_000L ~staked_own:1_500_000L ~staked_edge:0L ())
      ~block_fees:0L
      [make_delegator addr_a 1_000_000L]
  in
  let bp =
    Reward_calculator.generate_blueprint
      ~config
      ~network:"ghostnet"
      ~cycle_rewards:cr
  in
  let dr = List.hd bp.delegator_rewards in
  let expected = Int64.add 1_000_000L (Int64.add 2_000_000L 500_000L) in
  let diff = Int64.abs (Int64.sub dr.gross_reward expected) in
  Alcotest.(check bool)
    "gross matches sum of delegated sub-fields only"
    true
    (diff <= 1L)

let test_pure_external_staker_gets_zero () =
  (* A delegator entry with delegated_balance = 0 represents an external
     staker. The protocol pays them their staked share directly into
     their frozen deposit, so we must pay nothing. *)
  let config = {(default_config ()) with baker_fee = 0.0; min_payout = 0L} in
  let cr =
    make_cycle_buckets
      ~own_staked:0L
      ~own_delegated:1_000_000L
      ~block_rewards:
        (split
           ~delegated:10_000_000L
           ~staked_own:5_000_000L
           ~staked_shared:8_000_000L
           ())
      ~block_fees:500_000L
      [make_delegator ~staked:5_000_000L addr_a 0L]
  in
  let bp =
    Reward_calculator.generate_blueprint
      ~config
      ~network:"ghostnet"
      ~cycle_rewards:cr
  in
  let dr = List.hd bp.delegator_rewards in
  Alcotest.(check int64) "pure staker gross is zero" 0L dr.gross_reward

let test_mixed_delegator_staker () =
  (* Entry has both delegated_balance and staked_balance. Only the
     delegated component contributes to the gross. *)
  let config = {(default_config ()) with baker_fee = 0.0} in
  let cr =
    make_cycle_buckets
      ~own_staked:0L
      ~own_delegated:0L
      ~block_rewards:
        (split
           ~delegated:10_000_000L
           ~staked_own:99_000_000L
           ~staked_shared:99_000_000L
           ())
      [make_delegator ~staked:5_000_000L addr_a 5_000_000L]
  in
  let bp =
    Reward_calculator.generate_blueprint
      ~config
      ~network:"ghostnet"
      ~cycle_rewards:cr
  in
  let dr = List.hd bp.delegator_rewards in
  let diff = Int64.abs (Int64.sub dr.gross_reward 10_000_000L) in
  Alcotest.(check bool)
    "mixed entry gets only delegated portion (staked ignored)"
    true
    (diff <= 1L)

let test_vdf_and_nonce_excluded_from_pool () =
  (* VDF and nonce revelation rewards stay with the baker even when
     their [delegated] sub-field is non-zero. *)
  let config = {(default_config ()) with baker_fee = 0.0} in
  let with_vdf_nonce =
    make_cycle_buckets
      ~own_staked:0L
      ~own_delegated:0L
      ~block_rewards:(split ~delegated:1_000_000L ())
      ~vdf_rewards:(split ~delegated:5_000_000L ())
      ~nonce_rewards:(split ~delegated:5_000_000L ())
      [make_delegator addr_a 1_000_000L]
  in
  let without =
    make_cycle_buckets
      ~own_staked:0L
      ~own_delegated:0L
      ~block_rewards:(split ~delegated:1_000_000L ())
      [make_delegator addr_a 1_000_000L]
  in
  let bp_with =
    Reward_calculator.generate_blueprint
      ~config
      ~network:"ghostnet"
      ~cycle_rewards:with_vdf_nonce
  in
  let bp_without =
    Reward_calculator.generate_blueprint
      ~config
      ~network:"ghostnet"
      ~cycle_rewards:without
  in
  let dr_with = List.hd bp_with.delegator_rewards in
  let dr_without = List.hd bp_without.delegator_rewards in
  Alcotest.(check int64)
    "VDF and nonce do not enter the delegator's gross"
    dr_without.gross_reward
    dr_with.gross_reward

let test_block_fees_in_pool () =
  (* Block fees enter the pool unconditionally. *)
  let config = {(default_config ()) with baker_fee = 0.0} in
  let cr =
    make_cycle_buckets
      ~own_staked:0L
      ~own_delegated:0L
      ~block_rewards:zero_split
      ~block_fees:1_000_000L
      [make_delegator addr_a 1_000_000L]
  in
  let bp =
    Reward_calculator.generate_blueprint
      ~config
      ~network:"ghostnet"
      ~cycle_rewards:cr
  in
  let dr = List.hd bp.delegator_rewards in
  let diff = Int64.abs (Int64.sub dr.gross_reward 1_000_000L) in
  Alcotest.(check bool) "block fees flow to delegator" true (diff <= 1L)

(* {1 Test runner} *)

let () =
  Alcotest.run
    "reward_calculator"
    [
      ( "proportional_shares",
        [
          Alcotest.test_case
            "single delegator"
            `Quick
            test_single_delegator_proportional_share;
          Alcotest.test_case
            "two delegators"
            `Quick
            test_two_delegators_proportional;
        ] );
      ( "fees",
        [
          Alcotest.test_case "global fee" `Quick test_global_fee_application;
          Alcotest.test_case
            "per-delegator override"
            `Quick
            test_per_delegator_fee_override;
        ] );
      ( "overdelegation",
        [
          Alcotest.test_case "cap at 9x" `Quick test_overdelegation_cap;
          Alcotest.test_case
            "disabled"
            `Quick
            test_no_overdelegation_when_disabled;
        ] );
      ( "eligibility",
        [
          Alcotest.test_case "min balance" `Quick test_min_balance_filter;
          Alcotest.test_case "min payout" `Quick test_min_payout_filter;
          Alcotest.test_case "blacklist" `Quick test_blacklist_filter;
          Alcotest.test_case "whitelist" `Quick test_whitelist_filter;
          Alcotest.test_case
            "ignore contracts"
            `Quick
            test_ignore_contracts_filter;
        ] );
      ( "below_min",
        [
          Alcotest.test_case "redistribute" `Quick test_redistribute_below_min;
          Alcotest.test_case "baker keeps" `Quick test_baker_keeps_below_min;
        ] );
      ( "edge_cases",
        [
          Alcotest.test_case "zero delegators" `Quick test_zero_delegators;
          Alcotest.test_case "zero rewards" `Quick test_zero_rewards;
        ] );
      ( "overrides",
        [Alcotest.test_case "redirect" `Quick test_redirect_override] );
      ( "baker_income",
        [
          Alcotest.test_case "income and fees" `Quick test_baker_income_and_fees;
          Alcotest.test_case
            "bond/fee recipients"
            `Quick
            test_bond_fee_recipient_payouts;
          Alcotest.test_case "estimated tx fees" `Quick test_estimated_tx_fees;
        ] );
      ( "buckets",
        [
          Alcotest.test_case
            "pure delegator only delegated pool"
            `Quick
            test_pure_delegator_only_delegated_pool;
          Alcotest.test_case
            "pure external staker gets zero"
            `Quick
            test_pure_external_staker_gets_zero;
          Alcotest.test_case
            "mixed delegator/staker"
            `Quick
            test_mixed_delegator_staker;
          Alcotest.test_case
            "VDF and nonce excluded from pool"
            `Quick
            test_vdf_and_nonce_excluded_from_pool;
          Alcotest.test_case "block fees in pool" `Quick test_block_fees_in_pool;
        ] );
    ]
