(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Octez_manager_rewards

(* ── Trigger cycle logic ──────────────────────────── *)

let test_cycles_due_no_unpaid () =
  (* With interval=1, only the last 1 cycle is checked *)
  let instance = "test-continual-cycles-none" in
  let due =
    Payout_continual.cycles_due
      ~instance
      ~current_cycle:10
      ~interval:1
      ~offset:0
  in
  (* Only cycle 9 should be due (lookback = interval = 1) *)
  Alcotest.(check (list int)) "1 cycle due" [9] due

(* ── Pure trigger check tests ─────────────────────── *)

let test_is_trigger_cycle_interval_1 () =
  (* interval=1 means every cycle is a trigger *)
  let open Payout_continual.Internal_for_tests in
  Alcotest.(check bool)
    "cycle 100 triggers"
    true
    (is_trigger_cycle ~current_cycle:100 ~interval:1 ~offset:0) ;
  Alcotest.(check bool)
    "cycle 101 triggers"
    true
    (is_trigger_cycle ~current_cycle:101 ~interval:1 ~offset:0) ;
  Alcotest.(check bool)
    "cycle 102 triggers"
    true
    (is_trigger_cycle ~current_cycle:102 ~interval:1 ~offset:0)

let test_is_trigger_cycle_interval_2_offset_0 () =
  (* interval=2, offset=0: triggers on even cycles *)
  let open Payout_continual.Internal_for_tests in
  Alcotest.(check bool)
    "cycle 100 triggers (even)"
    true
    (is_trigger_cycle ~current_cycle:100 ~interval:2 ~offset:0) ;
  Alcotest.(check bool)
    "cycle 101 no trigger (odd)"
    false
    (is_trigger_cycle ~current_cycle:101 ~interval:2 ~offset:0) ;
  Alcotest.(check bool)
    "cycle 102 triggers (even)"
    true
    (is_trigger_cycle ~current_cycle:102 ~interval:2 ~offset:0)

let test_is_trigger_cycle_interval_2_offset_1 () =
  (* interval=2, offset=1: triggers on odd cycles *)
  let open Payout_continual.Internal_for_tests in
  Alcotest.(check bool)
    "cycle 100 no trigger (even)"
    false
    (is_trigger_cycle ~current_cycle:100 ~interval:2 ~offset:1) ;
  Alcotest.(check bool)
    "cycle 101 triggers (odd)"
    true
    (is_trigger_cycle ~current_cycle:101 ~interval:2 ~offset:1) ;
  Alcotest.(check bool)
    "cycle 102 no trigger (even)"
    false
    (is_trigger_cycle ~current_cycle:102 ~interval:2 ~offset:1)

let test_is_trigger_cycle_interval_3 () =
  (* interval=3, offset=0: triggers on multiples of 3 *)
  let open Payout_continual.Internal_for_tests in
  Alcotest.(check bool)
    "cycle 99 triggers"
    true
    (is_trigger_cycle ~current_cycle:99 ~interval:3 ~offset:0) ;
  Alcotest.(check bool)
    "cycle 100 no trigger"
    false
    (is_trigger_cycle ~current_cycle:100 ~interval:3 ~offset:0) ;
  Alcotest.(check bool)
    "cycle 101 no trigger"
    false
    (is_trigger_cycle ~current_cycle:101 ~interval:3 ~offset:0) ;
  Alcotest.(check bool)
    "cycle 102 triggers"
    true
    (is_trigger_cycle ~current_cycle:102 ~interval:3 ~offset:0)

let test_is_trigger_cycle_interval_3_offset_2 () =
  (* interval=3, offset=2: triggers when (cycle-2) mod 3 = 0 *)
  let open Payout_continual.Internal_for_tests in
  Alcotest.(check bool)
    "cycle 98 triggers"
    true
    (is_trigger_cycle ~current_cycle:98 ~interval:3 ~offset:2) ;
  Alcotest.(check bool)
    "cycle 99 no trigger"
    false
    (is_trigger_cycle ~current_cycle:99 ~interval:3 ~offset:2) ;
  Alcotest.(check bool)
    "cycle 100 no trigger"
    false
    (is_trigger_cycle ~current_cycle:100 ~interval:3 ~offset:2) ;
  Alcotest.(check bool)
    "cycle 101 triggers"
    true
    (is_trigger_cycle ~current_cycle:101 ~interval:3 ~offset:2)

(* ── Pure cycle collection tests ──────────────────── *)

let test_collect_due_cycles_all_unpaid () =
  let open Payout_continual.Internal_for_tests in
  let is_paid _c = false in
  let due = collect_due_cycles ~current_cycle:10 ~is_paid in
  Alcotest.(check int) "10 cycles due" 10 (List.length due) ;
  Alcotest.(check (list int)) "cycles 0-9" [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] due

let test_collect_due_cycles_all_paid () =
  let open Payout_continual.Internal_for_tests in
  let is_paid _c = true in
  let due = collect_due_cycles ~current_cycle:10 ~is_paid in
  Alcotest.(check (list int)) "no cycles due" [] due

let test_collect_due_cycles_some_paid () =
  let open Payout_continual.Internal_for_tests in
  let paid_cycles = [1; 3; 5; 7] in
  let is_paid c = List.mem c paid_cycles in
  let due = collect_due_cycles ~current_cycle:10 ~is_paid in
  Alcotest.(check (list int)) "only unpaid cycles" [0; 2; 4; 6; 8; 9] due

let test_collect_due_cycles_window_small () =
  let open Payout_continual.Internal_for_tests in
  let is_paid _c = false in
  let due = collect_due_cycles ~current_cycle:5 ~is_paid in
  (* check_from = max 0 (5-20) = 0, so cycles 0..4 *)
  Alcotest.(check (list int)) "cycles 0-4" [0; 1; 2; 3; 4] due

let test_collect_due_cycles_window_large () =
  let open Payout_continual.Internal_for_tests in
  let is_paid _c = false in
  let due = collect_due_cycles ~current_cycle:25 ~is_paid in
  (* check_from = max 0 (25-20) = 5, so cycles 5..24 *)
  Alcotest.(check int) "20 cycles due" 20 (List.length due) ;
  Alcotest.(check int) "first is 5" 5 (List.hd due) ;
  Alcotest.(check int) "last is 24" 24 (List.nth due 19)

(* ── Integration: combined trigger + collection ────── *)

let test_cycles_due_interval_2_trigger () =
  (* interval=2, offset=0, current=100: trigger, returns last 2 unpaid [98, 99] *)
  let instance = "test-continual-interval-2-trigger" in
  let due =
    Payout_continual.cycles_due
      ~instance
      ~current_cycle:100
      ~interval:2
      ~offset:0
  in
  (* Cycle 100 is even, so it triggers. Last 2 cycles should be returned. *)
  Alcotest.(check (list int)) "2 cycles due" [98; 99] due

let test_cycles_due_interval_2_no_trigger () =
  (* interval=2, offset=0, current=101: no trigger, returns [] *)
  let instance = "test-continual-interval-2-no-trigger" in
  let due =
    Payout_continual.cycles_due
      ~instance
      ~current_cycle:101
      ~interval:2
      ~offset:0
  in
  (* Cycle 101 is odd, so no trigger *)
  Alcotest.(check (list int)) "no cycles due" [] due

let test_cycles_due_interval_1_always_triggers () =
  (* interval=1: always trigger, returns last 1 unpaid cycle *)
  let instance = "test-continual-interval-1-always" in
  let due =
    Payout_continual.cycles_due
      ~instance
      ~current_cycle:10
      ~interval:1
      ~offset:0
  in
  Alcotest.(check (list int)) "1 cycle due" [9] due

let test_cycles_due_large_cycle () =
  (* For large cycle numbers with interval=1, only last 1 cycle is checked *)
  let instance = "test-continual-large" in
  let due =
    Payout_continual.cycles_due
      ~instance
      ~current_cycle:100
      ~interval:1
      ~offset:0
  in
  Alcotest.(check (list int)) "1 cycle due" [99] due

let test_cycles_due_large_interval () =
  (* interval=5, offset=0, current=100: trigger, returns last 5 unpaid *)
  let instance = "test-continual-large-interval" in
  let due =
    Payout_continual.cycles_due
      ~instance
      ~current_cycle:100
      ~interval:5
      ~offset:0
  in
  Alcotest.(check (list int)) "5 cycles due" [95; 96; 97; 98; 99] due

let test_cycles_due_current_excluded () =
  (* current_cycle itself should never be in the due list *)
  let instance = "test-continual-current" in
  let due =
    Payout_continual.cycles_due ~instance ~current_cycle:5 ~interval:1 ~offset:0
  in
  Alcotest.(check bool) "current cycle excluded" false (List.mem 5 due)

let test_cycles_due_zero_current () =
  let instance = "test-continual-zero" in
  let due =
    Payout_continual.cycles_due ~instance ~current_cycle:0 ~interval:1 ~offset:0
  in
  Alcotest.(check (list int)) "no cycles due at cycle 0" [] due

let () =
  Alcotest.run
    "payout_continual"
    [
      ( "is_trigger_cycle",
        [
          Alcotest.test_case
            "interval=1 always triggers"
            `Quick
            test_is_trigger_cycle_interval_1;
          Alcotest.test_case
            "interval=2 offset=0 (even)"
            `Quick
            test_is_trigger_cycle_interval_2_offset_0;
          Alcotest.test_case
            "interval=2 offset=1 (odd)"
            `Quick
            test_is_trigger_cycle_interval_2_offset_1;
          Alcotest.test_case
            "interval=3 offset=0"
            `Quick
            test_is_trigger_cycle_interval_3;
          Alcotest.test_case
            "interval=3 offset=2"
            `Quick
            test_is_trigger_cycle_interval_3_offset_2;
        ] );
      ( "collect_due_cycles",
        [
          Alcotest.test_case
            "all unpaid"
            `Quick
            test_collect_due_cycles_all_unpaid;
          Alcotest.test_case "all paid" `Quick test_collect_due_cycles_all_paid;
          Alcotest.test_case
            "some paid"
            `Quick
            test_collect_due_cycles_some_paid;
          Alcotest.test_case
            "small window"
            `Quick
            test_collect_due_cycles_window_small;
          Alcotest.test_case
            "large window"
            `Quick
            test_collect_due_cycles_window_large;
        ] );
      ( "cycles_due_integration",
        [
          Alcotest.test_case "all unpaid" `Quick test_cycles_due_no_unpaid;
          Alcotest.test_case
            "interval=2 trigger"
            `Quick
            test_cycles_due_interval_2_trigger;
          Alcotest.test_case
            "interval=2 no trigger"
            `Quick
            test_cycles_due_interval_2_no_trigger;
          Alcotest.test_case
            "interval=1 always triggers"
            `Quick
            test_cycles_due_interval_1_always_triggers;
          Alcotest.test_case
            "large cycle window"
            `Quick
            test_cycles_due_large_cycle;
          Alcotest.test_case
            "large interval"
            `Quick
            test_cycles_due_large_interval;
          Alcotest.test_case
            "current excluded"
            `Quick
            test_cycles_due_current_excluded;
          Alcotest.test_case "zero current" `Quick test_cycles_due_zero_current;
        ] );
    ]
