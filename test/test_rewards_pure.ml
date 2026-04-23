(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Octez_manager_rewards

(* ── extract_op_hash tests ────────────────────────────────── *)

let test_extract_op_hash_bare_line () =
  let output = "some text\nooBLAH123abc\nmore text\n" in
  let result = Payout_executor.Internal_for_tests.extract_op_hash output in
  Alcotest.(check (option string))
    "bare oo-prefixed line"
    (Some "ooBLAH123abc")
    result

let test_extract_op_hash_quoted () =
  let output = "Operation hash is 'ooABC123def456'\n" in
  let result = Payout_executor.Internal_for_tests.extract_op_hash output in
  Alcotest.(check (option string)) "quoted hash" (Some "ooABC123def456") result

let test_extract_op_hash_no_match () =
  let output = "No operation hash here\njust some text\n" in
  let result = Payout_executor.Internal_for_tests.extract_op_hash output in
  Alcotest.(check (option string)) "no match" None result

let test_extract_op_hash_empty () =
  let result = Payout_executor.Internal_for_tests.extract_op_hash "" in
  Alcotest.(check (option string)) "empty input" None result

let test_extract_op_hash_short () =
  let output = "oo\n" in
  let result = Payout_executor.Internal_for_tests.extract_op_hash output in
  Alcotest.(check (option string)) "too short" None result

(* ── cycles_due tests ─────────────────────────────────────── *)

(* cycles_due checks Payout_report.cycle_is_paid which reads disk.
   We test the interval/offset logic by using a temp dir where nothing
   is paid. *)

let with_temp_instance f =
  let dir = Filename.temp_dir "om-test-" "" in
  let instance = Filename.basename dir in
  Fun.protect
    ~finally:(fun () -> ignore (Sys.command ("rm -rf " ^ dir)))
    (fun () -> f instance)

let test_cycles_due_every_cycle () =
  with_temp_instance (fun instance ->
      let due =
        Payout_continual.cycles_due
          ~instance
          ~current_cycle:105
          ~interval:1
          ~offset:0
      in
      (* Should include 85..104 (20 cycle lookback, all unpaid) *)
      Alcotest.(check int) "20 cycles due" 20 (List.length due) ;
      Alcotest.(check int) "first is 85" 85 (List.hd due) ;
      Alcotest.(check int)
        "last is 104"
        104
        (List.nth due (List.length due - 1)))

let test_cycles_due_interval_2 () =
  with_temp_instance (fun instance ->
      (* Test with current_cycle=105 (odd), interval=2, offset=0
         Since 105 is odd, it's NOT a trigger cycle, so should return [] *)
      let due_no_trigger =
        Payout_continual.cycles_due
          ~instance
          ~current_cycle:105
          ~interval:2
          ~offset:0
      in
      Alcotest.(check (list int)) "no trigger on odd cycle" [] due_no_trigger ;
      (* Test with current_cycle=106 (even), interval=2, offset=0
         Since 106 is even, it IS a trigger cycle, so should return ALL unpaid cycles *)
      let due_trigger =
        Payout_continual.cycles_due
          ~instance
          ~current_cycle:106
          ~interval:2
          ~offset:0
      in
      (* Should include all cycles 86..105 (20 cycle lookback, all unpaid) *)
      Alcotest.(check int)
        "20 cycles due on trigger"
        20
        (List.length due_trigger) ;
      Alcotest.(check int) "first is 86" 86 (List.hd due_trigger) ;
      Alcotest.(check int)
        "last is 105"
        105
        (List.nth due_trigger (List.length due_trigger - 1)))

let test_cycles_due_interval_with_offset () =
  with_temp_instance (fun instance ->
      (* Test with current_cycle=105, interval=3, offset=1
         Check if 105 is a trigger: (105 - 1) mod 3 = 104 mod 3 = 2, NOT a trigger *)
      let due_no_trigger =
        Payout_continual.cycles_due
          ~instance
          ~current_cycle:105
          ~interval:3
          ~offset:1
      in
      Alcotest.(check (list int)) "no trigger" [] due_no_trigger ;
      (* Test with current_cycle=106, interval=3, offset=1
         Check if 106 is a trigger: (106 - 1) mod 3 = 105 mod 3 = 0, IS a trigger *)
      let due_trigger =
        Payout_continual.cycles_due
          ~instance
          ~current_cycle:106
          ~interval:3
          ~offset:1
      in
      (* Should return all unpaid cycles 86..105 *)
      Alcotest.(check int) "20 cycles due" 20 (List.length due_trigger) ;
      Alcotest.(check int) "first is 86" 86 (List.hd due_trigger) ;
      Alcotest.(check int)
        "last is 105"
        105
        (List.nth due_trigger (List.length due_trigger - 1)))

let test_cycles_due_excludes_current () =
  with_temp_instance (fun instance ->
      let due =
        Payout_continual.cycles_due
          ~instance
          ~current_cycle:100
          ~interval:1
          ~offset:0
      in
      Alcotest.(check bool)
        "current cycle 100 not included"
        false
        (List.mem 100 due))

(* ── render_template tests ────────────────────────────────── *)

let make_summary ?(cycle = 42) ?(paid = 5) ?(distributed = 1_000_000L)
    ?(fee = 100_000L) ?(tx = 2_000L) () : Rewards.cycle_summary =
  {
    cycle;
    delegators = 10;
    paid_delegators = paid;
    own_staked_balance = 0L;
    own_delegated_balance = 0L;
    external_staked_balance = 0L;
    external_delegated_balance = 0L;
    earned_rewards = 5_000_000L;
    earned_block_fees = 500_000L;
    distributed_rewards = distributed;
    bond_income = 0L;
    fee_income = fee;
    tx_fees_paid = tx;
    timestamp = "2026-01-01T00:00:00Z";
  }

let test_render_template_basic () =
  let summary = make_summary () in
  let result =
    Payout_notifier.render_template
      ~template:"Cycle <Cycle>: paid <Delegators> delegators"
      ~summary
  in
  Alcotest.(check string) "basic template" "Cycle 42: paid 5 delegators" result

let test_render_template_amounts () =
  let summary = make_summary () in
  let result =
    Payout_notifier.render_template ~template:"Total: <TotalPaid>" ~summary
  in
  Alcotest.(check string) "total paid" "Total: 1.000000" result

let test_render_template_no_placeholders () =
  let summary = make_summary () in
  let result =
    Payout_notifier.render_template ~template:"No placeholders here" ~summary
  in
  Alcotest.(check string) "passthrough" "No placeholders here" result

let test_render_template_timestamp () =
  let summary = make_summary () in
  let result =
    Payout_notifier.render_template ~template:"At <Timestamp>" ~summary
  in
  Alcotest.(check string) "timestamp" "At 2026-01-01T00:00:00Z" result

(* ── tez_of_mutez tests ───────────────────────────────────── *)

let test_tez_of_mutez_zero () =
  Alcotest.(check string) "zero" "0.000000" (Rewards.tez_of_mutez 0L)

let test_tez_of_mutez_one_tez () =
  Alcotest.(check string) "1 tez" "1.000000" (Rewards.tez_of_mutez 1_000_000L)

let test_tez_of_mutez_fraction () =
  Alcotest.(check string) "0.5 tez" "0.500000" (Rewards.tez_of_mutez 500_000L)

let test_tez_of_mutez_small () =
  Alcotest.(check string) "1 mutez" "0.000001" (Rewards.tez_of_mutez 1L)

let test_tez_of_mutez_large () =
  Alcotest.(check string)
    "1M tez"
    "1000000.000000"
    (Rewards.tez_of_mutez 1_000_000_000_000L)

(* ── total_earned tests ───────────────────────────────────── *)

let test_total_earned () =
  let cr : Rewards.cycle_rewards =
    {
      cycle = 1;
      baker = "tz1test";
      staking_balance = 0L;
      delegated_balance = 0L;
      own_staked_balance = 0L;
      own_delegated_balance = 0L;
      external_staked_balance = 0L;
      external_delegated_balance = 0L;
      block_rewards = 100L;
      attestation_rewards = 200L;
      other_rewards = 50L;
      block_fees = 30L;
      num_delegators = 0;
      delegators = [];
    }
  in
  Alcotest.(check int64) "total earned" 380L (Rewards.total_earned cr)

(* ── Test runner ──────────────────────────────────────────── *)

let () =
  Alcotest.run
    "rewards_pure"
    [
      ( "extract_op_hash",
        [
          Alcotest.test_case "bare oo line" `Quick test_extract_op_hash_bare_line;
          Alcotest.test_case "quoted hash" `Quick test_extract_op_hash_quoted;
          Alcotest.test_case "no match" `Quick test_extract_op_hash_no_match;
          Alcotest.test_case "empty input" `Quick test_extract_op_hash_empty;
          Alcotest.test_case "too short" `Quick test_extract_op_hash_short;
        ] );
      ( "cycles_due",
        [
          Alcotest.test_case "every cycle" `Quick test_cycles_due_every_cycle;
          Alcotest.test_case "interval 2" `Quick test_cycles_due_interval_2;
          Alcotest.test_case
            "interval+offset"
            `Quick
            test_cycles_due_interval_with_offset;
          Alcotest.test_case
            "excludes current"
            `Quick
            test_cycles_due_excludes_current;
        ] );
      ( "render_template",
        [
          Alcotest.test_case "basic" `Quick test_render_template_basic;
          Alcotest.test_case "amounts" `Quick test_render_template_amounts;
          Alcotest.test_case
            "no placeholders"
            `Quick
            test_render_template_no_placeholders;
          Alcotest.test_case "timestamp" `Quick test_render_template_timestamp;
        ] );
      ( "tez_of_mutez",
        [
          Alcotest.test_case "zero" `Quick test_tez_of_mutez_zero;
          Alcotest.test_case "one tez" `Quick test_tez_of_mutez_one_tez;
          Alcotest.test_case "fraction" `Quick test_tez_of_mutez_fraction;
          Alcotest.test_case "small" `Quick test_tez_of_mutez_small;
          Alcotest.test_case "large" `Quick test_tez_of_mutez_large;
        ] );
      ( "total_earned",
        [Alcotest.test_case "sum of rewards" `Quick test_total_earned] );
    ]
