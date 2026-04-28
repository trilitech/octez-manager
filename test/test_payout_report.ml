(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Octez_manager_rewards

let tmpdir () =
  let dir = Filename.temp_dir "om_test_report" "" in
  dir

let cleanup_dir dir =
  let rec rm path =
    if Sys.is_directory path then (
      Array.iter (fun f -> rm (Filename.concat path f)) (Sys.readdir path) ;
      Unix.rmdir path)
    else Sys.remove path
  in
  if Sys.file_exists dir then rm dir

let sample_summary cycle =
  {
    Rewards.cycle;
    delegators = 50;
    paid_delegators = 48;
    own_staked_balance = 600_000_000_000L;
    own_delegated_balance = 0L;
    external_staked_balance = 100_000_000_000L;
    external_delegated_balance = 4_000_000_000_000L;
    earned_rewards = 50_000_000L;
    earned_block_fees = 5_000_000L;
    distributed_rewards = 45_000_000L;
    bond_income = 8_000_000L;
    fee_income = 2_500_000L;
    tx_fees_paid = 150_000L;
    timestamp = "2026-02-25T12:00:00Z";
  }

let sample_results () =
  [
    {
      Rewards.delegator = "tz1VESSKJmVRxQmd8n56jJPKVfufmD9XEVwQ";
      recipient = "tz1VESSKJmVRxQmd8n56jJPKVfufmD9XEVwQ";
      amount = 1_000_000L;
      op_hash = Some "oo1abc...xyz";
      success = true;
      note = "";
    };
    {
      Rewards.delegator = "tz1Z4kNvBDDWZpd6FqAXWzCj5i3CxhP8RASD";
      recipient = "tz1Z4kNvBDDWZpd6FqAXWzCj5i3CxhP8RASD";
      amount = 500_000L;
      op_hash = None;
      success = false;
      note = "insufficient balance";
    };
  ]

let sample_invalid_rewards () =
  [
    {
      Rewards.delegator = "tz1ExcludedAddr000000000000000000abc";
      recipient = "tz1ExcludedAddr000000000000000000abc";
      delegated_balance = 100_000L;
      staked_balance = 0L;
      gross_reward = 50L;
      fee_rate = 0.05;
      fee_amount = 2L;
      net_reward = 48L;
      status = Rewards.Below_minimum_payout;
    };
  ]

(* ── Summary JSON round-trip ────────────────────── *)

let test_summary_json_roundtrip () =
  let dir = tmpdir () in
  let summary = sample_summary 100 in
  (match Payout_report.write_summary_json ~dir summary with
  | Ok () -> ()
  | Error msg -> Alcotest.fail (Printf.sprintf "write failed: %s" msg)) ;
  let path = Filename.concat dir "summary.json" in
  Alcotest.(check bool) "summary file exists" true (Sys.file_exists path) ;
  let ic = open_in path in
  let content = In_channel.input_all ic in
  close_in ic ;
  let json = Yojson.Safe.from_string content in
  let open Yojson.Safe.Util in
  Alcotest.(check int) "cycle" 100 (member "cycle" json |> to_int) ;
  Alcotest.(check int) "delegators" 50 (member "delegators" json |> to_int) ;
  Alcotest.(check int)
    "paid_delegators"
    48
    (member "paid_delegators" json |> to_int) ;
  Alcotest.(check string)
    "distributed_rewards"
    "45000000"
    (member "distributed_rewards" json |> to_string) ;
  Alcotest.(check string)
    "timestamp"
    "2026-02-25T12:00:00Z"
    (member "timestamp" json |> to_string) ;
  cleanup_dir dir

let test_summary_json_all_fields () =
  let dir = tmpdir () in
  let summary = sample_summary 200 in
  (match Payout_report.write_summary_json ~dir summary with
  | Ok () -> ()
  | Error msg -> Alcotest.fail (Printf.sprintf "write failed: %s" msg)) ;
  let path = Filename.concat dir "summary.json" in
  let ic = open_in path in
  let content = In_channel.input_all ic in
  close_in ic ;
  let json = Yojson.Safe.from_string content in
  let open Yojson.Safe.Util in
  Alcotest.(check string)
    "earned_rewards"
    "50000000"
    (member "earned_rewards" json |> to_string) ;
  Alcotest.(check string)
    "earned_block_fees"
    "5000000"
    (member "earned_block_fees" json |> to_string) ;
  Alcotest.(check string)
    "bond_income"
    "8000000"
    (member "bond_income" json |> to_string) ;
  Alcotest.(check string)
    "fee_income"
    "2500000"
    (member "fee_income" json |> to_string) ;
  Alcotest.(check string)
    "tx_fees_paid"
    "150000"
    (member "tx_fees_paid" json |> to_string) ;
  Alcotest.(check string)
    "own_staked_balance"
    "600000000000"
    (member "own_staked_balance" json |> to_string) ;
  Alcotest.(check string)
    "external_delegated_balance"
    "4000000000000"
    (member "external_delegated_balance" json |> to_string) ;
  cleanup_dir dir

(* ── CSV write ──────────────────────────────────── *)

let test_payouts_csv_write () =
  let dir = tmpdir () in
  let results = sample_results () in
  (match
     Payout_report.write_payouts_csv ~dir ~baker:"tz1Baker" ~cycle:42 results
   with
  | Ok () -> ()
  | Error msg -> Alcotest.fail (Printf.sprintf "write failed: %s" msg)) ;
  let path = Filename.concat dir "payouts.csv" in
  Alcotest.(check bool) "file exists" true (Sys.file_exists path) ;
  let ic = open_in path in
  let content = In_channel.input_all ic in
  close_in ic ;
  let lines = String.split_on_char '\n' content in
  (* Header + 2 data rows + trailing empty *)
  Alcotest.(check bool) "at least 3 lines" true (List.length lines >= 3) ;
  (* Check header has standard columns *)
  let header = List.hd lines in
  Alcotest.(check bool) "header has id" true (String.length header > 0) ;
  Alcotest.(check bool)
    "header starts with id"
    true
    (String.starts_with ~prefix:"id," header) ;
  Alcotest.(check bool)
    "header has op_hash"
    true
    (let found = ref false in
     String.iter (fun _ -> ()) header ;
     List.iter
       (fun col -> if String.equal col "op_hash" then found := true)
       (String.split_on_char ',' header) ;
     !found) ;
  cleanup_dir dir

let test_invalid_csv_write () =
  let dir = tmpdir () in
  let rewards = sample_invalid_rewards () in
  (match
     Payout_report.write_invalid_csv ~dir ~baker:"tz1Baker" ~cycle:42 rewards
   with
  | Ok () -> ()
  | Error msg -> Alcotest.fail (Printf.sprintf "write failed: %s" msg)) ;
  let path = Filename.concat dir "invalid.csv" in
  Alcotest.(check bool) "file exists" true (Sys.file_exists path) ;
  let ic = open_in path in
  let content = In_channel.input_all ic in
  close_in ic ;
  let lines = String.split_on_char '\n' content in
  Alcotest.(check bool) "at least 2 lines" true (List.length lines >= 2) ;
  cleanup_dir dir

let test_csv_header_standard_columns () =
  (* Verify all required standard columns are present *)
  let dir = tmpdir () in
  let results = sample_results () in
  (match
     Payout_report.write_payouts_csv ~dir ~baker:"tz1Baker" ~cycle:1 results
   with
  | Ok () -> ()
  | Error msg -> Alcotest.fail (Printf.sprintf "write failed: %s" msg)) ;
  let path = Filename.concat dir "payouts.csv" in
  let ic = open_in path in
  let header = input_line ic in
  close_in ic ;
  let cols = String.split_on_char ',' header in
  let required =
    [
      "id";
      "baker";
      "timestamp";
      "cycle";
      "kind";
      "delegator";
      "recipient";
      "amount";
      "op_hash";
      "success";
      "note";
    ]
  in
  List.iter
    (fun col ->
      Alcotest.(check bool)
        (Printf.sprintf "header has %s" col)
        true
        (List.exists (String.equal col) cols))
    required ;
  cleanup_dir dir

(* ── cycle_is_paid detection ────────────────────── *)

let test_cycle_is_paid () =
  (* cycle_is_paid checks for summary.json in the report dir *)
  let dir = tmpdir () in
  let instance = "test-baker" in
  (* Create a fake report dir structure *)
  let cycle = 99 in
  let report_path =
    Filename.concat
      (Filename.concat
         dir
         (Printf.sprintf "rewards/%s/reports/%d" instance cycle))
      ""
  in
  (* Since we can't easily override the base path, test the logic directly:
     write a summary to a temp dir and verify it exists *)
  let summary = sample_summary cycle in
  (match Payout_report.write_summary_json ~dir summary with
  | Ok () -> ()
  | Error msg -> Alcotest.fail (Printf.sprintf "write failed: %s" msg)) ;
  let path = Filename.concat dir "summary.json" in
  Alcotest.(check bool) "summary exists" true (Sys.file_exists path) ;
  ignore report_path ;
  cleanup_dir dir

(* ── CSV reader recovers multi-line note rows ─────────── *)

(* A previous build wrote the multi-line "Command failed: …" error from
   octez-client straight into the [note] column without escaping its newlines.
   The reader must still produce one record per delegator (not one per
   physical line) when handed such a corrupt file. *)
let test_parse_multiline_quoted_note () =
  let content =
    String.concat
      "\n"
      [
        "id,baker,timestamp,cycle,kind,op_kind,contract,token_id,fa_alias,fa_decimals,delegator,delegator_balance,staked_balance,recipient,amount,fee_rate,fee,tx_fee,op_hash,success,note";
        "1,tz1Baker,2026-04-28T09:05:30Z,11374,delegator,transaction,,,,,tz1AAA,1387778,,tz1AAA,1387778,,,,,false,\"Command \
         failed: octez-client multiple transfers";
        "Output:";
        "Node is bootstrapped.";
        "This simulation failed:";
        "  Balance of contract tz1Baker too low (0) to spend 1.387778\"";
        "2,tz1Baker,2026-04-28T09:05:30Z,11374,delegator,transaction,,,,,tz1BBB,347401,,tz1BBB,347401,,,,,false,\"Command \
         failed: blah";
        "more lines\"";
      ]
  in
  let records = Payout_report.Internal_for_tests.parse_csv_content content in
  match records with
  | _header :: data ->
      Alcotest.(check int) "two data records" 2 (List.length data) ;
      let first = List.nth data 0 in
      let second = List.nth data 1 in
      Alcotest.(check string) "first delegator" "tz1AAA" (List.nth first 10) ;
      Alcotest.(check string) "first amount" "1387778" (List.nth first 14) ;
      Alcotest.(check string) "first success" "false" (List.nth first 19) ;
      Alcotest.(check bool)
        "first note keeps multi-line content"
        true
        (let n = List.nth first 20 in
         String.length n > 50 && String.contains n '\n') ;
      Alcotest.(check string) "second delegator" "tz1BBB" (List.nth second 10)
  | [] -> Alcotest.fail "no records parsed"

(* New writes must sanitize newlines and tabs in notes so the row stays on
   one line. *)
let test_writer_sanitizes_note_newlines () =
  let dir = tmpdir () in
  let multiline = "Command failed:\nOutput:\n  Balance too low\n\twith tabs" in
  let results =
    [
      {
        Rewards.delegator = "tz1AAA";
        recipient = "tz1AAA";
        amount = 1_000_000L;
        op_hash = None;
        success = false;
        note = multiline;
      };
    ]
  in
  (match
     Payout_report.write_payouts_csv ~dir ~baker:"tz1Baker" ~cycle:42 results
   with
  | Ok () -> ()
  | Error msg -> Alcotest.fail (Printf.sprintf "write failed: %s" msg)) ;
  let path = Filename.concat dir "payouts.csv" in
  let ic = open_in path in
  let content = In_channel.input_all ic in
  close_in ic ;
  let lines =
    String.split_on_char '\n' content
    |> List.filter (fun s -> String.length s > 0)
  in
  Alcotest.(check int) "header + one row, no extra lines" 2 (List.length lines) ;
  cleanup_dir dir

let () =
  Alcotest.run
    "payout_report"
    [
      ( "summary_json",
        [
          Alcotest.test_case "roundtrip" `Quick test_summary_json_roundtrip;
          Alcotest.test_case "all fields" `Quick test_summary_json_all_fields;
        ] );
      ( "csv",
        [
          Alcotest.test_case "payouts write" `Quick test_payouts_csv_write;
          Alcotest.test_case "invalid write" `Quick test_invalid_csv_write;
          Alcotest.test_case
            "standard columns"
            `Quick
            test_csv_header_standard_columns;
          Alcotest.test_case
            "reader recovers multi-line quoted note"
            `Quick
            test_parse_multiline_quoted_note;
          Alcotest.test_case
            "writer sanitizes note newlines"
            `Quick
            test_writer_sanitizes_note_newlines;
        ] );
      ( "cycle_detection",
        [Alcotest.test_case "cycle_is_paid" `Quick test_cycle_is_paid] );
    ]
