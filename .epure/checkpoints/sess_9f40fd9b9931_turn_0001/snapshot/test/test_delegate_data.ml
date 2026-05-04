(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Tests for Delegate_data module.

    Covers parse_participation, parse_dal_participation, of_json,
    missed_slots_status, format_tez, and cache operations. *)

open Alcotest
module DD = Octez_manager_ui.Delegate_data

(* ── Helpers ─────────────────────────────────────────────────── *)

let default_participation : DD.participation =
  {
    expected_cycle_activity = 0;
    minimal_cycle_activity = 0;
    missed_slots = 0;
    missed_levels = 0;
    remaining_allowed_missed_slots = 0;
    expected_attesting_rewards = "0";
  }

let default_dal_participation : DD.dal_participation =
  {
    expected_assigned_shards_per_slot = 0;
    delegate_attested_dal_slots = 0;
    delegate_attestable_dal_slots = 0;
    expected_dal_rewards = "0";
    sufficient_dal_participation = false;
    denounced = false;
  }

let make_delegate ?(pkh = "tz1test") ?(missed_slots = 0)
    ?(remaining_allowed_missed_slots = 100) () : DD.t =
  {
    pkh;
    deactivated = false;
    is_forbidden = false;
    participation =
      {default_participation with missed_slots; remaining_allowed_missed_slots};
    dal_participation = default_dal_participation;
    baking_power = "0";
    total_staked = "0";
    total_delegated = "0";
    own_full_balance = "0";
    fetched_at = Unix.gettimeofday ();
  }

(* ── parse_participation ─────────────────────────────────────── *)

let test_parse_participation_complete () =
  let json =
    Yojson.Safe.from_string
      {|{
    "expected_cycle_activity": 100,
    "minimal_cycle_activity": 50,
    "missed_slots": 3,
    "missed_levels": 1,
    "remaining_allowed_missed_slots": 47,
    "expected_attesting_rewards": "5000000"
  }|}
  in
  let p = DD.For_tests.parse_participation json in
  check int "expected" 100 p.expected_cycle_activity ;
  check int "minimal" 50 p.minimal_cycle_activity ;
  check int "missed_slots" 3 p.missed_slots ;
  check int "missed_levels" 1 p.missed_levels ;
  check int "remaining" 47 p.remaining_allowed_missed_slots ;
  check string "rewards" "5000000" p.expected_attesting_rewards

let test_parse_participation_empty_defaults () =
  let p = DD.For_tests.parse_participation (`Assoc []) in
  check int "default expected" 0 p.expected_cycle_activity ;
  check int "default minimal" 0 p.minimal_cycle_activity ;
  check int "default missed" 0 p.missed_slots ;
  check int "default levels" 0 p.missed_levels ;
  check int "default remaining" 0 p.remaining_allowed_missed_slots ;
  check string "default rewards" "0" p.expected_attesting_rewards

let test_parse_participation_partial () =
  let json =
    Yojson.Safe.from_string
      {|{"missed_slots": 5, "expected_cycle_activity": 200}|}
  in
  let p = DD.For_tests.parse_participation json in
  check int "missed_slots" 5 p.missed_slots ;
  check int "expected" 200 p.expected_cycle_activity ;
  check int "remaining defaults" 0 p.remaining_allowed_missed_slots

let test_parse_participation_wrong_types () =
  (* to_int_option throws on string values, so parse_participation
     is expected to be called only with well-formed JSON from RPC.
     Null values do fall back to defaults. *)
  let json =
    Yojson.Safe.from_string
      {|{"missed_slots": null, "expected_cycle_activity": null}|}
  in
  let p = DD.For_tests.parse_participation json in
  check int "null falls back to default" 0 p.missed_slots ;
  check int "null falls back to default" 0 p.expected_cycle_activity

(* ── parse_dal_participation ─────────────────────────────────── *)

let test_parse_dal_participation_complete () =
  let json =
    Yojson.Safe.from_string
      {|{
    "expected_assigned_shards_per_slot": 8,
    "delegate_attested_dal_slots": 10,
    "delegate_attestable_dal_slots": 12,
    "expected_dal_rewards": "4200000",
    "sufficient_dal_participation": true,
    "denounced": false
  }|}
  in
  let p = DD.For_tests.parse_dal_participation json in
  check int "shards" 8 p.expected_assigned_shards_per_slot ;
  check int "attested" 10 p.delegate_attested_dal_slots ;
  check int "attestable" 12 p.delegate_attestable_dal_slots ;
  check string "rewards" "4200000" p.expected_dal_rewards ;
  check bool "sufficient" true p.sufficient_dal_participation ;
  check bool "denounced" false p.denounced

let test_parse_dal_participation_empty_defaults () =
  let p = DD.For_tests.parse_dal_participation (`Assoc []) in
  check int "default shards" 0 p.expected_assigned_shards_per_slot ;
  check bool "default sufficient" false p.sufficient_dal_participation ;
  check bool "default denounced" false p.denounced

let test_parse_dal_participation_denounced () =
  let json =
    Yojson.Safe.from_string
      {|{"denounced": true, "sufficient_dal_participation": false}|}
  in
  let p = DD.For_tests.parse_dal_participation json in
  check bool "denounced" true p.denounced ;
  check bool "not sufficient" false p.sufficient_dal_participation

(* ── of_json ─────────────────────────────────────────────────── *)

let test_of_json_happy_path () =
  let json =
    Yojson.Safe.from_string
      {|{
    "deactivated": false,
    "is_forbidden": false,
    "participation": {
      "expected_cycle_activity": 100,
      "missed_slots": 2,
      "remaining_allowed_missed_slots": 48,
      "expected_attesting_rewards": "3000000"
    },
    "dal_participation": {},
    "baking_power": "1000",
    "total_staked": "500",
    "total_delegated": "250",
    "own_full_balance": "750"
  }|}
  in
  match DD.For_tests.of_json ~pkh:"tz1abc" json with
  | None -> fail "should parse"
  | Some d ->
      check string "pkh" "tz1abc" d.pkh ;
      check bool "not deactivated" false d.deactivated ;
      check int "missed" 2 d.participation.missed_slots ;
      check string "baking_power" "1000" d.baking_power

let test_of_json_deactivated () =
  let json =
    Yojson.Safe.from_string
      {|{"deactivated": true, "participation": {}, "dal_participation": {}}|}
  in
  match DD.For_tests.of_json ~pkh:"tz1deact" json with
  | None -> fail "should parse"
  | Some d -> check bool "deactivated" true d.deactivated

let test_of_json_minimal () =
  (* Needs at least participation/dal_participation objects since
     member on Null raises Type_error *)
  let json =
    Yojson.Safe.from_string {|{"participation": {}, "dal_participation": {}}|}
  in
  match DD.For_tests.of_json ~pkh:"tz1min" json with
  | None -> fail "should parse"
  | Some d ->
      check string "pkh" "tz1min" d.pkh ;
      check bool "default deactivated" false d.deactivated ;
      check string "default baking_power" "0" d.baking_power

let test_of_json_missing_participation () =
  (* Without participation key, member returns Null, parse_participation
     fails, and of_json catches the exception returning None *)
  let json = Yojson.Safe.from_string {|{"deactivated": false}|} in
  check
    bool
    "None without participation"
    true
    (DD.For_tests.of_json ~pkh:"tz1" json = None)

let test_of_json_malformed () =
  let json = Yojson.Safe.from_string {|"not an object"|} in
  check bool "None for string" true (DD.For_tests.of_json ~pkh:"tz1" json = None)

let test_of_json_null () =
  check bool "None for null" true (DD.For_tests.of_json ~pkh:"tz1" `Null = None)

(* ── missed_slots_status ─────────────────────────────────────── *)

let test_missed_status_good_zero () =
  let d = make_delegate ~missed_slots:0 ~remaining_allowed_missed_slots:50 () in
  check bool "Good" true (DD.missed_slots_status d = DD.Good)

let test_missed_status_good_low () =
  let d = make_delegate ~missed_slots:5 ~remaining_allowed_missed_slots:50 () in
  check bool "Good low" true (DD.missed_slots_status d = DD.Good)

let test_missed_status_warning () =
  let d =
    make_delegate ~missed_slots:25 ~remaining_allowed_missed_slots:50 ()
  in
  check bool "Warning" true (DD.missed_slots_status d = DD.Warning)

let test_missed_status_warning_boundary () =
  let d =
    make_delegate ~missed_slots:25 ~remaining_allowed_missed_slots:50 ()
  in
  check bool "Warning at boundary" true (DD.missed_slots_status d = DD.Warning)

let test_missed_status_critical () =
  let d =
    make_delegate ~missed_slots:50 ~remaining_allowed_missed_slots:50 ()
  in
  check bool "Critical" true (DD.missed_slots_status d = DD.Critical)

let test_missed_status_critical_exceeded () =
  let d =
    make_delegate ~missed_slots:60 ~remaining_allowed_missed_slots:50 ()
  in
  check bool "Critical exceeded" true (DD.missed_slots_status d = DD.Critical)

let test_missed_status_remaining_zero () =
  let d = make_delegate ~missed_slots:5 ~remaining_allowed_missed_slots:0 () in
  check bool "Good when remaining=0" true (DD.missed_slots_status d = DD.Good)

let test_missed_status_both_zero () =
  let d = make_delegate ~missed_slots:0 ~remaining_allowed_missed_slots:0 () in
  check bool "Good both zero" true (DD.missed_slots_status d = DD.Good)

(* ── format_tez ──────────────────────────────────────────────── *)

let test_format_tez_zero () = check string "zero" "0" (DD.format_tez "0")

let test_format_tez_one_tez () =
  (* 1 tez = 1_000_000 mutez *)
  check string "1 tez" "1" (DD.format_tez "1000000")

let test_format_tez_fractional () =
  (* 1.5 tez = 1_500_000 mutez *)
  check string "1.5 tez" "2" (DD.format_tez "1500000")

let test_format_tez_small () =
  (* 100 tez = 100_000_000 mutez *)
  check string "100 tez" "100" (DD.format_tez "100000000")

let test_format_tez_thousands () =
  (* 1500 tez = 1_500_000_000 mutez *)
  check string "1.5K" "1.5K" (DD.format_tez "1500000000")

let test_format_tez_exact_thousand () =
  (* 1000 tez = 1_000_000_000 mutez *)
  check string "1.0K" "1.0K" (DD.format_tez "1000000000")

let test_format_tez_millions () =
  (* 2_000_000 tez = 2_000_000_000_000 mutez *)
  check string "2.0M" "2.0M" (DD.format_tez "2000000000000")

let test_format_tez_millions_fractional () =
  (* 1_500_000 tez = 1_500_000_000_000 mutez *)
  check string "1.5M" "1.5M" (DD.format_tez "1500000000000")

let test_format_tez_exact_million () =
  (* 1_000_000 tez = 1_000_000_000_000 mutez *)
  check string "1.0M" "1.0M" (DD.format_tez "1000000000000")

let test_format_tez_invalid () =
  check
    string
    "invalid returns input"
    "not_a_number"
    (DD.format_tez "not_a_number")

let test_format_tez_empty () =
  check string "empty returns input" "" (DD.format_tez "")

let test_format_tez_negative () =
  (* Negative mutez should still compute *)
  let result = DD.format_tez "-1000000" in
  check bool "produces output" true (String.length result > 0)

(* ── Cache operations ────────────────────────────────────────── *)

let test_cache_set_and_get () =
  DD.clear () ;
  let d =
    make_delegate
      ~pkh:"tz1cache"
      ~missed_slots:5
      ~remaining_allowed_missed_slots:95
      ()
  in
  DD.set d ;
  match DD.get ~pkh:"tz1cache" with
  | None -> fail "should find delegate"
  | Some found ->
      check string "pkh" "tz1cache" found.pkh ;
      check int "missed" 5 found.participation.missed_slots

let test_cache_get_missing () =
  DD.clear () ;
  check bool "not found" true (DD.get ~pkh:"tz1nonexistent" = None)

let test_cache_get_all () =
  DD.clear () ;
  DD.set (make_delegate ~pkh:"tz1aaa" ()) ;
  DD.set (make_delegate ~pkh:"tz1bbb" ()) ;
  let all = DD.get_all () in
  check int "two delegates" 2 (List.length all) ;
  let pkhs = List.map (fun (d : DD.t) -> d.pkh) all in
  check bool "contains tz1aaa" true (List.mem "tz1aaa" pkhs) ;
  check bool "contains tz1bbb" true (List.mem "tz1bbb" pkhs)

let test_cache_clear () =
  DD.clear () ;
  DD.set (make_delegate ~pkh:"tz1clear" ()) ;
  check int "one before clear" 1 (List.length (DD.get_all ())) ;
  DD.clear () ;
  check int "empty after clear" 0 (List.length (DD.get_all ()))

let test_cache_overwrite () =
  DD.clear () ;
  DD.set
    (make_delegate
       ~pkh:"tz1over"
       ~missed_slots:5
       ~remaining_allowed_missed_slots:100
       ()) ;
  DD.set
    (make_delegate
       ~pkh:"tz1over"
       ~missed_slots:10
       ~remaining_allowed_missed_slots:90
       ()) ;
  match DD.get ~pkh:"tz1over" with
  | None -> fail "should find updated delegate"
  | Some found ->
      check int "missed_slots updated" 10 found.participation.missed_slots ;
      check int "still one entry" 1 (List.length (DD.get_all ()))

let test_is_stale_fresh () =
  DD.clear () ;
  let d = make_delegate () in
  check bool "just created is not stale" false (DD.is_stale ~max_age:60.0 d)

let test_is_stale_old () =
  DD.clear () ;
  let d =
    {(make_delegate ()) with fetched_at = Unix.gettimeofday () -. 120.0}
  in
  check
    bool
    "120s old with 60s max_age is stale"
    true
    (DD.is_stale ~max_age:60.0 d)

let test_is_stale_boundary () =
  DD.clear () ;
  let d = {(make_delegate ()) with fetched_at = Unix.gettimeofday () -. 60.1} in
  check bool "just past max_age is stale" true (DD.is_stale ~max_age:60.0 d)

(* ── Test suite ──────────────────────────────────────────────── *)

let () =
  Alcotest.run
    "Delegate_data"
    [
      ( "parse_participation",
        [
          test_case "complete JSON" `Quick test_parse_participation_complete;
          test_case
            "empty JSON defaults"
            `Quick
            test_parse_participation_empty_defaults;
          test_case "partial JSON" `Quick test_parse_participation_partial;
          test_case "wrong types" `Quick test_parse_participation_wrong_types;
        ] );
      ( "parse_dal_participation",
        [
          test_case "complete JSON" `Quick test_parse_dal_participation_complete;
          test_case
            "empty JSON defaults"
            `Quick
            test_parse_dal_participation_empty_defaults;
          test_case "denounced" `Quick test_parse_dal_participation_denounced;
        ] );
      ( "of_json",
        [
          test_case "happy path" `Quick test_of_json_happy_path;
          test_case "deactivated" `Quick test_of_json_deactivated;
          test_case "minimal JSON" `Quick test_of_json_minimal;
          test_case "malformed" `Quick test_of_json_malformed;
          test_case "null" `Quick test_of_json_null;
          test_case
            "missing participation"
            `Quick
            test_of_json_missing_participation;
        ] );
      ( "missed_slots_status",
        [
          test_case "zero is Good" `Quick test_missed_status_good_zero;
          test_case "low is Good" `Quick test_missed_status_good_low;
          test_case "warning" `Quick test_missed_status_warning;
          test_case
            "warning boundary"
            `Quick
            test_missed_status_warning_boundary;
          test_case "critical" `Quick test_missed_status_critical;
          test_case
            "critical exceeded"
            `Quick
            test_missed_status_critical_exceeded;
          test_case "remaining=0" `Quick test_missed_status_remaining_zero;
          test_case "both zero" `Quick test_missed_status_both_zero;
        ] );
      ( "format_tez",
        [
          test_case "zero" `Quick test_format_tez_zero;
          test_case "1 tez" `Quick test_format_tez_one_tez;
          test_case "fractional" `Quick test_format_tez_fractional;
          test_case "100 tez" `Quick test_format_tez_small;
          test_case "1.5K" `Quick test_format_tez_thousands;
          test_case "1.0K" `Quick test_format_tez_exact_thousand;
          test_case "2.0M" `Quick test_format_tez_millions;
          test_case "1.5M" `Quick test_format_tez_millions_fractional;
          test_case "1.0M" `Quick test_format_tez_exact_million;
          test_case "invalid" `Quick test_format_tez_invalid;
          test_case "empty" `Quick test_format_tez_empty;
          test_case "negative" `Quick test_format_tez_negative;
        ] );
      ( "cache",
        [
          test_case "set and get" `Quick test_cache_set_and_get;
          test_case "get missing" `Quick test_cache_get_missing;
          test_case "get_all" `Quick test_cache_get_all;
          test_case "clear" `Quick test_cache_clear;
          test_case "overwrite" `Quick test_cache_overwrite;
          test_case "is_stale fresh" `Quick test_is_stale_fresh;
          test_case "is_stale old" `Quick test_is_stale_old;
          test_case "is_stale boundary" `Quick test_is_stale_boundary;
        ] );
    ]
