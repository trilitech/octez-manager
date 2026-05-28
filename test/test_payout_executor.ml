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

let temp_counter = ref 0

let with_temp_xdg f =
  incr temp_counter ;
  let old_xdg = Sys.getenv_opt "XDG_CONFIG_HOME" in
  let dir =
    Filename.concat
      (Filename.get_temp_dir_name ())
      (Printf.sprintf
         "octez-manager-payout-test-%d-%d"
         (Unix.getpid ())
         !temp_counter)
  in
  Unix.mkdir dir 0o700 ;
  Unix.putenv "XDG_CONFIG_HOME" dir ;
  Fun.protect
    ~finally:(fun () ->
      Cmd_runner.reset_run_out_with_timeout_combined_hook () ;
      match old_xdg with
      | Some v -> Unix.putenv "XDG_CONFIG_HOME" v
      | None -> Unix.putenv "XDG_CONFIG_HOME" "")
    f

let success_hash = "opVMd9YJV2tdkwDPN7CzXmEUmKYY27Phc8aRz3HEgFKUvW1ZBnp"

let make_ctx ~instance =
  {
    Payout_executor.octez_client_bin = "octez-client";
    endpoint = "http://localhost:8732";
    base_dir = None;
    password_file = None;
    payout_key_alias = "payout-key";
    instance;
  }

let blueprint_with_payouts ~cycle count =
  let rewards =
    List.init count (fun i ->
        let delegator = Printf.sprintf "tz1delegator%02d" i in
        make_delegator_reward
          ~delegator
          ~recipient:delegator
          ~net_reward:1000L
          ~status:Eligible)
  in
  make_blueprint
    ~cycle
    ~delegator_rewards:rewards
    ~bond_payouts:[]
    ~fee_payouts:[]

let test_failed_real_payout_does_not_mark_paid () =
  with_temp_xdg (fun () ->
      Cmd_runner.set_run_out_with_timeout_combined_hook (fun ~timeout:_ _argv ->
          Error (`Msg "simulated transfer failure")) ;
      let instance = "failed-real" in
      let cycle = 4242 in
      let ctx = make_ctx ~instance in
      let blueprint = blueprint_with_payouts ~cycle 1 in
      match Payout_executor.execute ~ctx ~blueprint ~batch_size:1 () with
      | Ok _ -> Alcotest.fail "failed real payout must return Error"
      | Error _ ->
          Alcotest.(check bool)
            "cycle remains unpaid"
            false
            (Payout_report.cycle_is_paid ~instance ~cycle) ;
          Alcotest.(check bool)
            "diagnostic payouts.csv written"
            true
            (Sys.file_exists
               (Filename.concat
                  (Payout_report.report_dir ~instance ~cycle)
                  "payouts.csv")))

let test_no_op_hash_real_payout_does_not_mark_paid () =
  with_temp_xdg (fun () ->
      Cmd_runner.set_run_out_with_timeout_combined_hook (fun ~timeout:_ _argv ->
          Ok "Injected operation but no operation hash was returned") ;
      let instance = "no-op-hash" in
      let cycle = 4243 in
      let ctx = make_ctx ~instance in
      let blueprint = blueprint_with_payouts ~cycle 1 in
      match Payout_executor.execute ~ctx ~blueprint ~batch_size:1 () with
      | Ok _ -> Alcotest.fail "real payout without op hash must return Error"
      | Error _ ->
          Alcotest.(check bool)
            "cycle remains unpaid"
            false
            (Payout_report.cycle_is_paid ~instance ~cycle))

let test_partial_real_payout_does_not_mark_paid () =
  with_temp_xdg (fun () ->
      let calls = ref 0 in
      Cmd_runner.set_run_out_with_timeout_combined_hook (fun ~timeout:_ _argv ->
          incr calls ;
          if !calls = 1 then Ok ("Operation hash is '" ^ success_hash ^ "'")
          else Error (`Msg "simulated second batch failure")) ;
      let instance = "partial-real" in
      let cycle = 4244 in
      let ctx = make_ctx ~instance in
      let blueprint = blueprint_with_payouts ~cycle 2 in
      match Payout_executor.execute ~ctx ~blueprint ~batch_size:1 () with
      | Ok _ -> Alcotest.fail "partial real payout must return Error"
      | Error _ ->
          Alcotest.(check int) "two batches attempted" 2 !calls ;
          Alcotest.(check bool)
            "cycle remains unpaid"
            false
            (Payout_report.cycle_is_paid ~instance ~cycle))

let test_report_write_failure_returns_error () =
  with_temp_xdg (fun () ->
      Cmd_runner.set_run_out_with_timeout_combined_hook (fun ~timeout:_ _argv ->
          Ok ("Operation hash is '" ^ success_hash ^ "'")) ;
      let instance = "report-write-fails" in
      let cycle = 4245 in
      let ctx = make_ctx ~instance in
      let blueprint = blueprint_with_payouts ~cycle 1 in
      let report_dir = Payout_report.report_dir ~instance ~cycle in
      let rec mkdir_p path =
        if Sys.file_exists path then ()
        else (
          mkdir_p (Filename.dirname path) ;
          Unix.mkdir path 0o755)
      in
      mkdir_p report_dir ;
      Unix.mkdir (Filename.concat report_dir "payouts.csv") 0o755 ;
      match Payout_executor.execute ~ctx ~blueprint ~batch_size:1 () with
      | Ok _ -> Alcotest.fail "report write failure must return Error"
      | Error msg ->
          Alcotest.(check bool)
            "mentions payouts.csv"
            true
            (String.contains msg 'p'))

let test_rejects_zero_batch_size () =
  with_temp_xdg (fun () ->
      let instance = "zero-batch" in
      let ctx = make_ctx ~instance in
      let blueprint = blueprint_with_payouts ~cycle:4246 1 in
      Alcotest.(check (result reject string))
        "execute rejects zero batch"
        (Error "batch_size must be > 0")
        (Payout_executor.execute ~ctx ~blueprint ~batch_size:0 ()) ;
      Alcotest.(check (result reject string))
        "execute_merged rejects zero batch"
        (Error "batch_size must be > 0")
        (Payout_executor.execute_merged
           ~ctx
           ~payouts:[("tz1delegator", "tz1delegator", 1L)]
           ~batch_size:0
           ()))

(* ── TC-1: successful full payout writes summary.json ─────────────────────── *)

let test_successful_payout_marks_paid () =
  with_temp_xdg (fun () ->
      Cmd_runner.set_run_out_with_timeout_combined_hook (fun ~timeout:_ _argv ->
          Ok ("Operation hash is '" ^ success_hash ^ "'")) ;
      let instance = "full-success" in
      let cycle = 5001 in
      let ctx = make_ctx ~instance in
      let blueprint = blueprint_with_payouts ~cycle 1 in
      match Payout_executor.execute ~ctx ~blueprint ~batch_size:1 () with
      | Error msg -> Alcotest.fail ("expected Ok, got Error: " ^ msg)
      | Ok _ ->
          Alcotest.(check bool)
            "cycle is marked paid"
            true
            (Payout_report.cycle_is_paid ~instance ~cycle) ;
          Alcotest.(check bool)
            "summary.json written"
            true
            (Sys.file_exists
               (Filename.concat
                  (Payout_report.report_dir ~instance ~cycle)
                  "summary.json")))

(* ── TC-2: consecutive batch abort after 2 failures ──────────────────────── *)

let test_consecutive_batch_abort () =
  with_temp_xdg (fun () ->
      let calls = ref 0 in
      Cmd_runner.set_run_out_with_timeout_combined_hook (fun ~timeout:_ _argv ->
          incr calls ;
          if !calls = 1 then Ok ("Operation hash is '" ^ success_hash ^ "'")
          else Error (`Msg "simulated batch failure")) ;
      let instance = "consec-abort" in
      let cycle = 5002 in
      let ctx = make_ctx ~instance in
      (* 4 delegators, batch_size=1 → 4 separate batches.
         Batch 1 succeeds; batches 2 and 3 fail (consecutive=2 → abort);
         batch 4 is never sent. *)
      let blueprint = blueprint_with_payouts ~cycle 4 in
      match Payout_executor.execute ~ctx ~blueprint ~batch_size:1 () with
      | Ok _ -> Alcotest.fail "consecutive abort must return Error"
      | Error _ ->
          Alcotest.(check int) "3 batches attempted (4th aborted)" 3 !calls ;
          Alcotest.(check bool)
            "cycle remains unpaid"
            false
            (Payout_report.cycle_is_paid ~instance ~cycle))

(* ── TC-3: dry-run does not mark paid and writes to dry dir ──────────────── *)

let test_dry_run_does_not_mark_paid () =
  with_temp_xdg (fun () ->
      (* Hook returns Ok so the batch command path runs without calling a real
         octez-client binary (which is unavailable in the test environment). *)
      Cmd_runner.set_run_out_with_timeout_combined_hook (fun ~timeout:_ _argv ->
          Ok "dry-run") ;
      let instance = "dry-run-test" in
      let cycle = 5003 in
      let ctx = make_ctx ~instance in
      let blueprint = blueprint_with_payouts ~cycle 2 in
      match
        Payout_executor.execute ~ctx ~blueprint ~dry_run:true ~batch_size:1 ()
      with
      | Error msg -> Alcotest.fail ("dry-run returned Error: " ^ msg)
      | Ok (results, _summary) ->
          Alcotest.(check bool)
            "real cycle not marked paid"
            false
            (Payout_report.cycle_is_paid ~instance ~cycle) ;
          Alcotest.(check bool)
            "dry report dir created"
            true
            (Sys.file_exists (Payout_report.dry_report_dir ~instance ~cycle)) ;
          List.iter
            (fun (r : Rewards.payout_result) ->
              Alcotest.(check bool)
                ("dry-run result success=true for " ^ r.delegator)
                true
                r.success ;
              Alcotest.(check (option string))
                ("dry-run op_hash=None for " ^ r.delegator)
                None
                r.op_hash)
            results)

(* ── TC-4: empty payout_key_alias returns early Error ────────────────────── *)

let test_empty_payout_key_alias_rejected () =
  with_temp_xdg (fun () ->
      let call_count = ref 0 in
      Cmd_runner.set_run_out_with_timeout_combined_hook (fun ~timeout:_ _argv ->
          incr call_count ;
          Ok "should not be called") ;
      let instance = "empty-key" in
      let cycle = 5004 in
      let blueprint = blueprint_with_payouts ~cycle 1 in
      let check_alias alias =
        let ctx =
          {
            Payout_executor.octez_client_bin = "octez-client";
            endpoint = "http://localhost:8732";
            base_dir = None;
            password_file = None;
            payout_key_alias = alias;
            instance;
          }
        in
        match Payout_executor.execute ~ctx ~blueprint () with
        | Ok _ ->
            Alcotest.fail (Printf.sprintf "expected Error for alias %S" alias)
        | Error msg ->
            Alcotest.(check bool)
              (Printf.sprintf "error mentions payout_key_alias for %S" alias)
              true
              (let low = String.lowercase_ascii msg in
               String.length low > 0
               &&
               let rec contains s sub i =
                 if i + String.length sub > String.length s then false
                 else if String.sub s i (String.length sub) = sub then true
                 else contains s sub (i + 1)
               in
               contains low "payout_key_alias" 0
               || contains low "not configured" 0)
      in
      check_alias "" ;
      check_alias "   " ;
      Alcotest.(check int) "no batch calls made" 0 !call_count)

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
              let hash =
                "oo7TpRPvfbf3hqkfDXRb9wGaScNQT2Tx9d8DqNwh4u14oVqK2gz"
              in
              let output =
                "Operation hash is '" ^ hash ^ "'\nSimulation result:"
              in
              let result =
                Payout_executor.Internal_for_tests.extract_op_hash output
              in
              Alcotest.(check (option string))
                "should extract op hash"
                (Some hash)
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
              let hash =
                "onuUUFwfL2pPq8RU8Aw1aJaybrgFeyt6h1pVyMKLSqwXbE2ZfDz"
              in
              let result =
                Payout_executor.Internal_for_tests.extract_op_hash hash
              in
              Alcotest.(check (option string))
                "should extract bare op hash"
                (Some hash)
                result);
          (* Regression: real op hashes start with [op…], [on…], … not just
             [oo…]. Pre-fix [extract_op_hash] hard-coded the second char as 'o'
             and silently dropped every real-world hash, so successful payouts
             were recorded as failures. *)
          Alcotest.test_case
            "extracts hash with non-oo prefix"
            `Quick
            (fun () ->
              let hash =
                "opVMd9YJV2tdkwDPN7CzXmEUmKYY27Phc8aRz3HEgFKUvW1ZBnp"
              in
              let output = "Operation hash is '" ^ hash ^ "'\n" in
              let result =
                Payout_executor.Internal_for_tests.extract_op_hash output
              in
              Alcotest.(check (option string))
                "should extract op-prefixed hash"
                (Some hash)
                result);
        ] );
      ( "execute",
        [
          Alcotest.test_case
            "failed real payout does not mark paid"
            `Quick
            test_failed_real_payout_does_not_mark_paid;
          Alcotest.test_case
            "no op hash does not mark paid"
            `Quick
            test_no_op_hash_real_payout_does_not_mark_paid;
          Alcotest.test_case
            "partial real payout does not mark paid"
            `Quick
            test_partial_real_payout_does_not_mark_paid;
          Alcotest.test_case
            "report write failure returns error"
            `Quick
            test_report_write_failure_returns_error;
          Alcotest.test_case
            "rejects zero batch size"
            `Quick
            test_rejects_zero_batch_size;
          Alcotest.test_case
            "successful payout marks paid"
            `Quick
            test_successful_payout_marks_paid;
          Alcotest.test_case
            "consecutive batch abort"
            `Quick
            test_consecutive_batch_abort;
          Alcotest.test_case
            "dry run does not mark paid"
            `Quick
            test_dry_run_does_not_mark_paid;
          Alcotest.test_case
            "empty payout key alias rejected"
            `Quick
            test_empty_payout_key_alias_rejected;
        ] );
    ]
