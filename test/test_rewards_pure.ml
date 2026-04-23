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
      (* interval=1: only look back 1 cycle, so just cycle 104 *)
      Alcotest.(check (list int)) "1 cycle due" [104] due)

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
         Since 106 is even, it IS a trigger cycle, returns last 2 unpaid *)
      let due_trigger =
        Payout_continual.cycles_due
          ~instance
          ~current_cycle:106
          ~interval:2
          ~offset:0
      in
      Alcotest.(check (list int))
        "2 cycles due on trigger"
        [104; 105]
        due_trigger)

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
      (* Should return last 3 unpaid cycles: 103, 104, 105 *)
      Alcotest.(check (list int)) "3 cycles due" [103; 104; 105] due_trigger)

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

(* ── format_time_ago tests ────────────────────────────────── *)

let test_format_time_ago_just_now () =
  let now = 1000.0 in
  let timestamp = 970.0 in
  (* 30 seconds ago *)
  Alcotest.(check string)
    "30 seconds ago"
    "just now"
    (Rewards.format_time_ago ~now ~timestamp)

let test_format_time_ago_minutes () =
  let now = 1000.0 in
  let timestamp = 700.0 in
  (* 300 seconds = 5 minutes ago *)
  Alcotest.(check string)
    "5 minutes ago"
    "5 min ago"
    (Rewards.format_time_ago ~now ~timestamp)

let test_format_time_ago_one_minute () =
  let now = 1000.0 in
  let timestamp = 940.0 in
  (* 60 seconds = 1 minute ago *)
  Alcotest.(check string)
    "1 minute ago"
    "1 min ago"
    (Rewards.format_time_ago ~now ~timestamp)

let test_format_time_ago_one_hour () =
  let now = 10000.0 in
  let timestamp = 6400.0 in
  (* 3600 seconds = 1 hour ago *)
  Alcotest.(check string)
    "1 hour ago"
    "1 hour ago"
    (Rewards.format_time_ago ~now ~timestamp)

let test_format_time_ago_hours () =
  let now = 10000.0 in
  let timestamp = 0.0 in
  (* 10000 seconds = 2.77 hours ago *)
  Alcotest.(check string)
    "2 hours ago"
    "2 hours ago"
    (Rewards.format_time_ago ~now ~timestamp)

let test_format_time_ago_one_day () =
  let now = 100000.0 in
  let timestamp = 13600.0 in
  (* 86400 seconds = 1 day ago *)
  Alcotest.(check string)
    "1 day ago"
    "1 day ago"
    (Rewards.format_time_ago ~now ~timestamp)

let test_format_time_ago_days () =
  let now = 500000.0 in
  let timestamp = 68000.0 in
  (* 432000 seconds = 5 days ago *)
  Alcotest.(check string)
    "5 days ago"
    "5 days ago"
    (Rewards.format_time_ago ~now ~timestamp)

let test_format_time_ago_one_week () =
  let now = 700000.0 in
  let timestamp = 95200.0 in
  (* 604800 seconds = 1 week ago *)
  Alcotest.(check string)
    "1 week ago"
    "1 week ago"
    (Rewards.format_time_ago ~now ~timestamp)

let test_format_time_ago_weeks () =
  let now = 2000000.0 in
  let timestamp = 790000.0 in
  (* 1210000 seconds = ~2 weeks ago *)
  Alcotest.(check string)
    "2 weeks ago"
    "2 weeks ago"
    (Rewards.format_time_ago ~now ~timestamp)

let test_format_time_ago_months () =
  let now = 10000000.0 in
  let timestamp = 4800000.0 in
  (* 5200000 seconds = ~2 months ago *)
  Alcotest.(check string)
    "2 months ago"
    "2 months ago"
    (Rewards.format_time_ago ~now ~timestamp)

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
      ( "format_time_ago",
        [
          Alcotest.test_case "just now" `Quick test_format_time_ago_just_now;
          Alcotest.test_case "minutes" `Quick test_format_time_ago_minutes;
          Alcotest.test_case "one minute" `Quick test_format_time_ago_one_minute;
          Alcotest.test_case "one hour" `Quick test_format_time_ago_one_hour;
          Alcotest.test_case "hours" `Quick test_format_time_ago_hours;
          Alcotest.test_case "one day" `Quick test_format_time_ago_one_day;
          Alcotest.test_case "days" `Quick test_format_time_ago_days;
          Alcotest.test_case "one week" `Quick test_format_time_ago_one_week;
          Alcotest.test_case "weeks" `Quick test_format_time_ago_weeks;
          Alcotest.test_case "months" `Quick test_format_time_ago_months;
        ] );
    ]
