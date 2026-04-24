(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Octez_manager_rewards

(* ── Test helpers ──────────────────────────────────────── *)

let make_delegator_reward ~delegator ~recipient ~net_reward ~status =
  {
    Rewards.delegator;
    delegated_balance = 0L;
    staked_balance = 0L;
    gross_reward = net_reward;
    fee_rate = 0.0;
    fee_amount = 0L;
    net_reward;
    recipient;
    status;
  }

let make_blueprint ~cycle ~delegator_rewards ~bond_payouts ~fee_payouts =
  {
    Rewards.cycle;
    baker = "tz1baker";
    network = "mainnet";
    earned_rewards = 0L;
    earned_block_fees = 0L;
    total_delegators = List.length delegator_rewards;
    eligible_delegators =
      List.filter
        (fun r -> match r.Rewards.status with Eligible -> true | _ -> false)
        delegator_rewards
      |> List.length;
    delegator_rewards;
    baker_bond_income = 0L;
    baker_fee_income = 0L;
    estimated_tx_fees = 0L;
    bond_payouts;
    fee_payouts;
  }

(* ── collect_payouts tests ────────────────────────────── *)

let test_collect_payouts_eligible_only () =
  let bp =
    make_blueprint
      ~cycle:100
      ~delegator_rewards:
        [
          make_delegator_reward
            ~delegator:"tz1alice"
            ~recipient:"tz1alice"
            ~net_reward:1000L
            ~status:Eligible;
          make_delegator_reward
            ~delegator:"tz1bob"
            ~recipient:"tz1bob"
            ~net_reward:2000L
            ~status:Below_minimum_payout;
          make_delegator_reward
            ~delegator:"tz1charlie"
            ~recipient:"tz1charlie"
            ~net_reward:3000L
            ~status:Eligible;
        ]
      ~bond_payouts:[]
      ~fee_payouts:[]
  in
  let payouts = Payout_executor.Internal_for_tests.collect_payouts bp in
  Alcotest.(check int) "2 eligible payouts" 2 (List.length payouts) ;
  Alcotest.(check bool)
    "alice included"
    true
    (List.exists (fun (d, _, _) -> String.equal d "tz1alice") payouts) ;
  Alcotest.(check bool)
    "charlie included"
    true
    (List.exists (fun (d, _, _) -> String.equal d "tz1charlie") payouts) ;
  Alcotest.(check bool)
    "bob excluded"
    false
    (List.exists (fun (d, _, _) -> String.equal d "tz1bob") payouts)

let test_collect_payouts_with_redirect () =
  let bp =
    make_blueprint
      ~cycle:100
      ~delegator_rewards:
        [
          make_delegator_reward
            ~delegator:"tz1alice"
            ~recipient:"tz1redirect"
            ~net_reward:1000L
            ~status:Eligible;
        ]
      ~bond_payouts:[]
      ~fee_payouts:[]
  in
  let payouts = Payout_executor.Internal_for_tests.collect_payouts bp in
  Alcotest.(check int) "1 payout" 1 (List.length payouts) ;
  match payouts with
  | [(delegator, recipient, amount)] ->
      Alcotest.(check string) "delegator is alice" "tz1alice" delegator ;
      Alcotest.(check string) "recipient is redirect" "tz1redirect" recipient ;
      Alcotest.(check int64) "amount is 1000" 1000L amount
  | _ -> Alcotest.fail "Expected exactly 1 payout"

let test_collect_payouts_with_bonds_and_fees () =
  let bp =
    make_blueprint
      ~cycle:100
      ~delegator_rewards:
        [
          make_delegator_reward
            ~delegator:"tz1alice"
            ~recipient:"tz1alice"
            ~net_reward:1000L
            ~status:Eligible;
        ]
      ~bond_payouts:[("tz1bond", 500L)]
      ~fee_payouts:[("tz1fee", 250L)]
  in
  let payouts = Payout_executor.Internal_for_tests.collect_payouts bp in
  Alcotest.(check int) "3 payouts total" 3 (List.length payouts) ;
  Alcotest.(check bool)
    "alice included"
    true
    (List.exists (fun (d, _, _) -> String.equal d "tz1alice") payouts) ;
  Alcotest.(check bool)
    "bond included"
    true
    (List.exists (fun (d, _, _) -> String.equal d "tz1bond") payouts) ;
  Alcotest.(check bool)
    "fee included"
    true
    (List.exists (fun (d, _, _) -> String.equal d "tz1fee") payouts)

let test_collect_payouts_zero_amounts_excluded () =
  let bp =
    make_blueprint
      ~cycle:100
      ~delegator_rewards:
        [
          make_delegator_reward
            ~delegator:"tz1alice"
            ~recipient:"tz1alice"
            ~net_reward:0L
            ~status:Eligible;
          make_delegator_reward
            ~delegator:"tz1bob"
            ~recipient:"tz1bob"
            ~net_reward:1000L
            ~status:Eligible;
        ]
      ~bond_payouts:[("tz1bond", 0L)]
      ~fee_payouts:[("tz1fee", 100L)]
  in
  let payouts = Payout_executor.Internal_for_tests.collect_payouts bp in
  Alcotest.(check int) "2 payouts (zero excluded)" 2 (List.length payouts) ;
  Alcotest.(check bool)
    "alice excluded (zero)"
    false
    (List.exists (fun (d, _, _) -> String.equal d "tz1alice") payouts) ;
  Alcotest.(check bool)
    "bond excluded (zero)"
    false
    (List.exists (fun (d, _, _) -> String.equal d "tz1bond") payouts)

(* ── merge_payouts tests ──────────────────────────────── *)

let test_merge_payouts_single_cycle () =
  let payouts1 =
    [("tz1alice", "tz1alice", 1000L); ("tz1bob", "tz1bob", 2000L)]
  in
  let merged = Payout_executor.merge_payouts [payouts1] in
  Alcotest.(check int) "2 payouts" 2 (List.length merged) ;
  Alcotest.(check bool)
    "alice present"
    true
    (List.exists (fun (d, _, _) -> String.equal d "tz1alice") merged) ;
  Alcotest.(check bool)
    "bob present"
    true
    (List.exists (fun (d, _, _) -> String.equal d "tz1bob") merged)

let test_merge_payouts_two_cycles_same_delegators () =
  let payouts1 =
    [("tz1alice", "tz1alice", 1000L); ("tz1bob", "tz1bob", 2000L)]
  in
  let payouts2 =
    [("tz1alice", "tz1alice", 500L); ("tz1bob", "tz1bob", 1500L)]
  in
  let merged = Payout_executor.merge_payouts [payouts1; payouts2] in
  Alcotest.(check int) "2 merged payouts" 2 (List.length merged) ;
  let alice_amount =
    List.find_opt (fun (d, _, _) -> String.equal d "tz1alice") merged
    |> Option.map (fun (_, _, amt) -> amt)
  in
  let bob_amount =
    List.find_opt (fun (d, _, _) -> String.equal d "tz1bob") merged
    |> Option.map (fun (_, _, amt) -> amt)
  in
  Alcotest.(check (option int64))
    "alice total is 1500"
    (Some 1500L)
    alice_amount ;
  Alcotest.(check (option int64)) "bob total is 3500" (Some 3500L) bob_amount

let test_merge_payouts_two_cycles_different_delegators () =
  let payouts1 = [("tz1alice", "tz1alice", 1000L)] in
  let payouts2 = [("tz1bob", "tz1bob", 2000L)] in
  let merged = Payout_executor.merge_payouts [payouts1; payouts2] in
  Alcotest.(check int) "2 distinct payouts" 2 (List.length merged) ;
  Alcotest.(check bool)
    "alice present"
    true
    (List.exists (fun (d, _, _) -> String.equal d "tz1alice") merged) ;
  Alcotest.(check bool)
    "bob present"
    true
    (List.exists (fun (d, _, _) -> String.equal d "tz1bob") merged)

let test_merge_payouts_two_cycles_overlapping () =
  let payouts1 =
    [("tz1alice", "tz1alice", 1000L); ("tz1bob", "tz1bob", 2000L)]
  in
  let payouts2 =
    [("tz1bob", "tz1bob", 1500L); ("tz1charlie", "tz1charlie", 3000L)]
  in
  let merged = Payout_executor.merge_payouts [payouts1; payouts2] in
  Alcotest.(check int) "3 merged payouts" 3 (List.length merged) ;
  let alice_amount =
    List.find_opt (fun (d, _, _) -> String.equal d "tz1alice") merged
    |> Option.map (fun (_, _, amt) -> amt)
  in
  let bob_amount =
    List.find_opt (fun (d, _, _) -> String.equal d "tz1bob") merged
    |> Option.map (fun (_, _, amt) -> amt)
  in
  let charlie_amount =
    List.find_opt (fun (d, _, _) -> String.equal d "tz1charlie") merged
    |> Option.map (fun (_, _, amt) -> amt)
  in
  Alcotest.(check (option int64))
    "alice total is 1000"
    (Some 1000L)
    alice_amount ;
  Alcotest.(check (option int64)) "bob total is 3500" (Some 3500L) bob_amount ;
  Alcotest.(check (option int64))
    "charlie total is 3000"
    (Some 3000L)
    charlie_amount

let test_merge_payouts_empty_input () =
  let merged = Payout_executor.merge_payouts [] in
  Alcotest.(check int) "empty output" 0 (List.length merged)

let test_merge_payouts_with_redirect () =
  let payouts1 = [("tz1alice", "tz1redirect", 1000L)] in
  let payouts2 = [("tz1alice", "tz1redirect", 500L)] in
  let merged = Payout_executor.merge_payouts [payouts1; payouts2] in
  Alcotest.(check int) "1 merged payout" 1 (List.length merged) ;
  match merged with
  | [(delegator, recipient, amount)] ->
      Alcotest.(check string) "delegator is alice" "tz1alice" delegator ;
      Alcotest.(check string) "recipient is redirect" "tz1redirect" recipient ;
      Alcotest.(check int64) "amount is 1500" 1500L amount
  | _ -> Alcotest.fail "Expected exactly 1 merged payout"

let test_merge_payouts_different_recipients_same_delegator () =
  (* Edge case: same delegator but different recipients should NOT merge *)
  let payouts1 = [("tz1alice", "tz1alice", 1000L)] in
  let payouts2 = [("tz1alice", "tz1redirect", 500L)] in
  let merged = Payout_executor.merge_payouts [payouts1; payouts2] in
  Alcotest.(check int) "2 separate payouts" 2 (List.length merged) ;
  let alice_to_alice =
    List.find_opt
      (fun (d, r, _) -> String.equal d "tz1alice" && String.equal r "tz1alice")
      merged
    |> Option.map (fun (_, _, amt) -> amt)
  in
  let alice_to_redirect =
    List.find_opt
      (fun (d, r, _) ->
        String.equal d "tz1alice" && String.equal r "tz1redirect")
      merged
    |> Option.map (fun (_, _, amt) -> amt)
  in
  Alcotest.(check (option int64))
    "alice->alice is 1000"
    (Some 1000L)
    alice_to_alice ;
  Alcotest.(check (option int64))
    "alice->redirect is 500"
    (Some 500L)
    alice_to_redirect

let () =
  Alcotest.run
    "payout_executor"
    [
      ( "collect_payouts",
        [
          Alcotest.test_case
            "eligible only"
            `Quick
            test_collect_payouts_eligible_only;
          Alcotest.test_case
            "with redirect"
            `Quick
            test_collect_payouts_with_redirect;
          Alcotest.test_case
            "with bonds and fees"
            `Quick
            test_collect_payouts_with_bonds_and_fees;
          Alcotest.test_case
            "zero amounts excluded"
            `Quick
            test_collect_payouts_zero_amounts_excluded;
        ] );
      ( "merge_payouts",
        [
          Alcotest.test_case
            "single cycle"
            `Quick
            test_merge_payouts_single_cycle;
          Alcotest.test_case
            "two cycles same delegators"
            `Quick
            test_merge_payouts_two_cycles_same_delegators;
          Alcotest.test_case
            "two cycles different delegators"
            `Quick
            test_merge_payouts_two_cycles_different_delegators;
          Alcotest.test_case
            "two cycles overlapping"
            `Quick
            test_merge_payouts_two_cycles_overlapping;
          Alcotest.test_case "empty input" `Quick test_merge_payouts_empty_input;
          Alcotest.test_case
            "with redirect"
            `Quick
            test_merge_payouts_with_redirect;
          Alcotest.test_case
            "different recipients same delegator"
            `Quick
            test_merge_payouts_different_recipients_same_delegator;
        ] );
      ( "extract_op_hash",
        [
          Alcotest.test_case "finds op hash in output" `Quick (fun () ->
              let output =
                "Operation hash is \
                 'ooXYZ123abc456def789ghi012jkl345mno678pqr901stu234vwx567yz'\n\
                 Simulation result:"
              in
              let result =
                Payout_executor.Internal_for_tests.extract_op_hash output
              in
              Alcotest.(check (option string))
                "should extract op hash"
                (Some
                   "ooXYZ123abc456def789ghi012jkl345mno678pqr901stu234vwx567yz")
                result);
          Alcotest.test_case "returns None when no op hash" `Quick (fun () ->
              let output =
                "Error:\n  Unrecognized command.\n  Try using the man command."
              in
              let result =
                Payout_executor.Internal_for_tests.extract_op_hash output
              in
              Alcotest.(check (option string)) "should return None" None result);
          Alcotest.test_case "finds bare op hash on line" `Quick (fun () ->
              let output =
                "ooABC123def456ghi789jkl012mno345pqr678stu901vwx234yz567ab"
              in
              let result =
                Payout_executor.Internal_for_tests.extract_op_hash output
              in
              Alcotest.(check (option string))
                "should extract bare op hash"
                (Some
                   "ooABC123def456ghi789jkl012mno345pqr678stu901vwx234yz567ab")
                result);
        ] );
    ]
