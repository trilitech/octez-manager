(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Tests for Baker_wallet_data module.

    Covers parse_delegate_aggregate, parse_spendable,
    parse_staking_parameters, parse_unstake_requests,
    parse_voting_info, format_tez, format_staking_limit,
    format_baking_edge, and cache operations. *)

open Alcotest
module BWD = Octez_manager_ui.Baker_wallet_data

(* ── parse_spendable ───────────────────────────────────────── *)

let test_parse_spendable_string () =
  let json = Yojson.Safe.from_string {|"1234567890"|} in
  check
    (option string)
    "parses string"
    (Some "1234567890")
    (BWD.For_tests.parse_spendable json)

let test_parse_spendable_non_string () =
  let json = Yojson.Safe.from_string {|42|} in
  check
    (option string)
    "rejects non-string"
    None
    (BWD.For_tests.parse_spendable json)

let test_parse_spendable_null () =
  check
    (option string)
    "rejects null"
    None
    (BWD.For_tests.parse_spendable `Null)

(* ── parse_staking_parameters ──────────────────────────────── *)

let test_parse_staking_parameters_complete () =
  let json =
    Yojson.Safe.from_string
      {|{
    "limit_of_staking_over_baking_millionth": 5000000,
    "edge_of_baking_over_staking_billionth": 100000000
  }|}
  in
  match BWD.For_tests.parse_staking_parameters json with
  | None -> fail "should parse"
  | Some p ->
      check int "limit" 5000000 p.limit_of_staking_over_baking ;
      check int "edge" 100000000 p.edge_of_baking_over_staking

let test_parse_staking_parameters_defaults () =
  let json = Yojson.Safe.from_string {|{}|} in
  match BWD.For_tests.parse_staking_parameters json with
  | None -> fail "should parse with defaults"
  | Some p ->
      check int "default limit" 0 p.limit_of_staking_over_baking ;
      check int "default edge" 0 p.edge_of_baking_over_staking

(* ── parse_unstake_requests ────────────────────────────────── *)

let test_parse_unstake_requests_empty () =
  let json = Yojson.Safe.from_string {|{}|} in
  let reqs = BWD.For_tests.parse_unstake_requests json in
  check int "no finalizable" 0 (List.length reqs.finalizable) ;
  check int "no unfinalizable" 0 (List.length reqs.unfinalizable)

let test_parse_unstake_requests_with_finalizable () =
  let json =
    Yojson.Safe.from_string
      {|{
    "finalizable": [
      {"delegate": {"cycle": 100, "amount": "200000000"}}
    ]
  }|}
  in
  let reqs = BWD.For_tests.parse_unstake_requests json in
  check int "one finalizable" 1 (List.length reqs.finalizable) ;
  let r = List.hd reqs.finalizable in
  check int "cycle" 100 r.cycle ;
  check string "amount" "200000000" r.amount

let test_parse_unstake_requests_with_unfinalizable () =
  let json =
    Yojson.Safe.from_string
      {|{
    "unfinalizable": {
      "requests": [
        {"delegate": {"cycle": 102, "amount": "100000000"}}
      ]
    }
  }|}
  in
  let reqs = BWD.For_tests.parse_unstake_requests json in
  check int "one unfinalizable" 1 (List.length reqs.unfinalizable) ;
  let r = List.hd reqs.unfinalizable in
  check int "cycle" 102 r.cycle ;
  check string "amount" "100000000" r.amount

(* ── parse_delegate_aggregate ──────────────────────────────── *)

let test_parse_delegate_aggregate_full () =
  let json =
    Yojson.Safe.from_string
      {|{
    "deactivated": false,
    "current_frozen_deposits": "5000000000",
    "frozen_deposits": "200000000",
    "full_balance": "6434567890",
    "active_consensus_key": "tz1abc",
    "pending_consensus_keys": [{"cycle": 150, "pkh": "tz1new"}]
  }|}
  in
  match BWD.For_tests.parse_delegate_aggregate ~pkh:"tz1test" json with
  | None -> fail "should parse"
  | Some d ->
      check string "pkh" "tz1test" d.pkh ;
      check bool "registered" true d.is_registered ;
      check bool "not deactivated" false d.deactivated ;
      check string "staked" "5000000000" d.staked_balance ;
      check string "unstaked_frozen" "200000000" d.unstaked_frozen ;
      check string "full_balance" "6434567890" d.full_balance ;
      check string "consensus_key" "tz1abc" d.active_consensus_key ;
      check int "pending keys" 1 (List.length d.pending_consensus_keys) ;
      let cycle, pkh = List.hd d.pending_consensus_keys in
      check int "pending cycle" 150 cycle ;
      check string "pending pkh" "tz1new" pkh

let test_parse_delegate_aggregate_deactivated () =
  let json =
    Yojson.Safe.from_string
      {|{
    "deactivated": true,
    "current_frozen_deposits": "0",
    "frozen_deposits": "0",
    "full_balance": "1000000"
  }|}
  in
  match BWD.For_tests.parse_delegate_aggregate ~pkh:"tz1deact" json with
  | None -> fail "should parse"
  | Some d ->
      check bool "deactivated" true d.deactivated ;
      check
        string
        "consensus key defaults to pkh"
        "tz1deact"
        d.active_consensus_key

let test_parse_delegate_aggregate_minimal () =
  let json = Yojson.Safe.from_string {|{}|} in
  match BWD.For_tests.parse_delegate_aggregate ~pkh:"tz1min" json with
  | None -> fail "should parse"
  | Some d ->
      check string "pkh" "tz1min" d.pkh ;
      check bool "default deactivated" false d.deactivated ;
      check string "default staked" "0" d.staked_balance

(* ── parse_voting_info ─────────────────────────────────────── *)

let test_parse_voting_info_proposal () =
  let period_json =
    Yojson.Safe.from_string
      {|{
    "voting_period": {"kind": "proposal", "index": 50},
    "position": 1234,
    "remaining": 5678
  }|}
  in
  let proposals_json =
    Yojson.Safe.from_string {|[["PtParis...", 123], ["PtQuebec...", 45]]|}
  in
  let ballot_list_json = Yojson.Safe.from_string {|[]|} in
  let current_proposal_json = `Null in
  match
    BWD.For_tests.parse_voting_info
      ~period_json
      ~proposals_json
      ~ballot_list_json
      ~current_proposal_json
  with
  | None -> fail "should parse"
  | Some vi ->
      check bool "proposal period" true (vi.period_kind = BWD.Proposal) ;
      check int "position" 1234 vi.period_position ;
      check int "remaining" 5678 vi.period_remaining ;
      check int "2 proposals" 2 (List.length vi.proposals) ;
      let hash, count = List.hd vi.proposals in
      check string "first hash" "PtParis..." hash ;
      check int "first count" 123 count ;
      check (option string) "no current proposal" None vi.current_proposal

let test_parse_voting_info_exploration () =
  let period_json =
    Yojson.Safe.from_string
      {|{
    "voting_period": {"kind": "exploration"},
    "position": 100,
    "remaining": 200
  }|}
  in
  let proposals_json = Yojson.Safe.from_string {|[]|} in
  let ballot_list_json =
    Yojson.Safe.from_string
      {|[{"pkh": "tz1abc", "ballot": "yay"}, {"pkh": "tz1def", "ballot": "nay"}]|}
  in
  let current_proposal_json = Yojson.Safe.from_string {|"PtCurrentProto..."|} in
  match
    BWD.For_tests.parse_voting_info
      ~period_json
      ~proposals_json
      ~ballot_list_json
      ~current_proposal_json
  with
  | None -> fail "should parse"
  | Some vi ->
      check bool "exploration" true (vi.period_kind = BWD.Exploration) ;
      check int "2 ballots" 2 (List.length vi.ballots) ;
      let pkh, ballot = List.hd vi.ballots in
      check string "ballot pkh" "tz1abc" pkh ;
      check string "ballot vote" "yay" ballot ;
      check
        (option string)
        "current proposal"
        (Some "PtCurrentProto...")
        vi.current_proposal

let test_parse_voting_info_cooldown () =
  let period_json =
    Yojson.Safe.from_string
      {|{"voting_period": {"kind": "cooldown"}, "position": 0, "remaining": 100}|}
  in
  match
    BWD.For_tests.parse_voting_info
      ~period_json
      ~proposals_json:(`List [])
      ~ballot_list_json:(`List [])
      ~current_proposal_json:`Null
  with
  | None -> fail "should parse"
  | Some vi -> check bool "cooldown" true (vi.period_kind = BWD.Cooldown)

let test_parse_voting_info_promotion () =
  let period_json =
    Yojson.Safe.from_string
      {|{"voting_period": {"kind": "promotion"}, "position": 50, "remaining": 150}|}
  in
  let current_proposal_json = Yojson.Safe.from_string {|"PtPromoProto..."|} in
  match
    BWD.For_tests.parse_voting_info
      ~period_json
      ~proposals_json:(`List [])
      ~ballot_list_json:(`List [])
      ~current_proposal_json
  with
  | None -> fail "should parse"
  | Some vi ->
      check bool "promotion" true (vi.period_kind = BWD.Promotion) ;
      check int "position" 50 vi.period_position ;
      check int "remaining" 150 vi.period_remaining ;
      check
        (option string)
        "current proposal"
        (Some "PtPromoProto...")
        vi.current_proposal

let test_parse_voting_info_adoption () =
  let period_json =
    Yojson.Safe.from_string
      {|{"voting_period": {"kind": "adoption"}, "position": 10, "remaining": 90}|}
  in
  match
    BWD.For_tests.parse_voting_info
      ~period_json
      ~proposals_json:(`List [])
      ~ballot_list_json:(`List [])
      ~current_proposal_json:`Null
  with
  | None -> fail "should parse"
  | Some vi ->
      check bool "adoption" true (vi.period_kind = BWD.Adoption) ;
      check int "position" 10 vi.period_position

let test_parse_voting_info_unknown_kind () =
  let period_json =
    Yojson.Safe.from_string
      {|{"voting_period": {"kind": "unknown_kind"}, "position": 0, "remaining": 0}|}
  in
  match
    BWD.For_tests.parse_voting_info
      ~period_json
      ~proposals_json:(`List [])
      ~ballot_list_json:(`List [])
      ~current_proposal_json:`Null
  with
  | None -> fail "should parse (defaults to Proposal)"
  | Some vi ->
      check bool "defaults to proposal" true (vi.period_kind = BWD.Proposal)

let test_parse_voting_info_no_current_proposal () =
  let period_json =
    Yojson.Safe.from_string
      {|{"voting_period": {"kind": "proposal"}, "position": 0, "remaining": 100}|}
  in
  match
    BWD.For_tests.parse_voting_info
      ~period_json
      ~proposals_json:(`List [])
      ~ballot_list_json:(`List [])
      ~current_proposal_json:`Null
  with
  | None -> fail "should parse"
  | Some vi -> check (option string) "no proposal" None vi.current_proposal

(* ── format_tez ────────────────────────────────────────────── *)

let test_format_tez_zero () =
  check string "zero" "0.000000 ꜩ" (BWD.format_tez "0")

let test_format_tez_one () =
  check string "1 tez" "1.000000 ꜩ" (BWD.format_tez "1000000")

let test_format_tez_fractional () =
  check string "1.5 tez" "1.500000 ꜩ" (BWD.format_tez "1500000")

let test_format_tez_large () =
  check string "1,234.567890 tez" "1,234.567890 ꜩ" (BWD.format_tez "1234567890")

let test_format_tez_millions () =
  check string "6,434.567890 tez" "6,434.567890 ꜩ" (BWD.format_tez "6434567890")

let test_format_tez_small_fraction () =
  check string "0.000001 tez" "0.000001 ꜩ" (BWD.format_tez "1")

let test_format_tez_invalid () =
  check
    string
    "invalid returns input"
    "not_a_number"
    (BWD.format_tez "not_a_number")

(* ── format_staking_limit ──────────────────────────────────── *)

let test_format_staking_limit_zero () =
  check string "0x" "0.0x" (BWD.format_staking_limit 0)

let test_format_staking_limit_five () =
  check string "5.0x" "5.0x" (BWD.format_staking_limit 5000000)

let test_format_staking_limit_nine () =
  check string "9.0x" "9.0x" (BWD.format_staking_limit 9000000)

(* ── format_baking_edge ────────────────────────────────────── *)

let test_format_baking_edge_zero () =
  check string "0.0%" "0.0%" (BWD.format_baking_edge 0)

let test_format_baking_edge_ten () =
  check string "10.0%" "10.0%" (BWD.format_baking_edge 100000000)

let test_format_baking_edge_hundred () =
  check string "100.0%" "100.0%" (BWD.format_baking_edge 1000000000)

(* ── Cache operations ──────────────────────────────────────── *)

let make_wallet_data ?(pkh = "tz1test") () : BWD.t =
  {
    pkh;
    spendable_balance = "1000000";
    staked_balance = "0";
    unstaked_frozen = "0";
    full_balance = "1000000";
    is_registered = false;
    deactivated = false;
    active_consensus_key = pkh;
    pending_consensus_keys = [];
    staking_parameters = None;
    pending_staking_parameters = None;
    unstake_requests = {finalizable = []; unfinalizable = []};
    fetched_at = Unix.gettimeofday ();
  }

let test_cache_set_and_get () =
  BWD.clear () ;
  let d = make_wallet_data ~pkh:"tz1cache" () in
  BWD.set d ;
  match BWD.get ~pkh:"tz1cache" with
  | None -> fail "should find"
  | Some found -> check string "pkh" "tz1cache" found.pkh

let test_cache_get_missing () =
  BWD.clear () ;
  check bool "not found" true (BWD.get ~pkh:"tz1nope" = None)

let test_cache_get_all () =
  BWD.clear () ;
  BWD.set (make_wallet_data ~pkh:"tz1a" ()) ;
  BWD.set (make_wallet_data ~pkh:"tz1b" ()) ;
  check int "two entries" 2 (List.length (BWD.get_all ()))

let test_cache_clear () =
  BWD.clear () ;
  BWD.set (make_wallet_data ~pkh:"tz1c" ()) ;
  BWD.clear () ;
  check int "empty" 0 (List.length (BWD.get_all ()))

let test_is_stale_fresh () =
  let d = make_wallet_data () in
  check bool "not stale" false (BWD.is_stale ~max_age:60.0 d)

let test_is_stale_old () =
  let d =
    {(make_wallet_data ()) with fetched_at = Unix.gettimeofday () -. 120.0}
  in
  check bool "stale" true (BWD.is_stale ~max_age:60.0 d)

(* ── Voting info cache ─────────────────────────────────────── *)

let test_voting_cache_set_and_get () =
  let vi : BWD.voting_info =
    {
      period_kind = BWD.Proposal;
      period_position = 5;
      period_remaining = 95;
      proposals = [("PtTest", 10)];
      current_proposal = None;
      ballots = [];
    }
  in
  BWD.set_voting_info ~node_endpoint:"http://localhost:8732" vi ;
  match BWD.get_voting_info ~node_endpoint:"http://localhost:8732" with
  | None -> fail "should find voting info"
  | Some found ->
      check bool "proposal" true (found.period_kind = BWD.Proposal) ;
      check int "position" 5 found.period_position ;
      check int "1 proposal" 1 (List.length found.proposals)

let test_voting_cache_missing () =
  check
    bool
    "not found"
    true
    (BWD.get_voting_info ~node_endpoint:"http://nope:9999" = None)

(* ── string_of helpers ─────────────────────────────────────── *)

let test_string_of_ballot_vote () =
  check string "yay" "yay" (BWD.string_of_ballot_vote BWD.Yay) ;
  check string "nay" "nay" (BWD.string_of_ballot_vote BWD.Nay) ;
  check string "pass" "pass" (BWD.string_of_ballot_vote BWD.Pass)

let test_string_of_voting_period_kind () =
  check
    string
    "proposal"
    "proposal"
    (BWD.string_of_voting_period_kind BWD.Proposal) ;
  check
    string
    "exploration"
    "exploration"
    (BWD.string_of_voting_period_kind BWD.Exploration) ;
  check
    string
    "cooldown"
    "cooldown"
    (BWD.string_of_voting_period_kind BWD.Cooldown) ;
  check
    string
    "promotion"
    "promotion"
    (BWD.string_of_voting_period_kind BWD.Promotion) ;
  check
    string
    "adoption"
    "adoption"
    (BWD.string_of_voting_period_kind BWD.Adoption)

(* ── Test suite ────────────────────────────────────────────── *)

let () =
  Alcotest.run
    "Baker_wallet_data"
    [
      ( "parse_spendable",
        [
          test_case "string value" `Quick test_parse_spendable_string;
          test_case "non-string" `Quick test_parse_spendable_non_string;
          test_case "null" `Quick test_parse_spendable_null;
        ] );
      ( "parse_staking_parameters",
        [
          test_case "complete" `Quick test_parse_staking_parameters_complete;
          test_case "defaults" `Quick test_parse_staking_parameters_defaults;
        ] );
      ( "parse_unstake_requests",
        [
          test_case "empty" `Quick test_parse_unstake_requests_empty;
          test_case
            "finalizable"
            `Quick
            test_parse_unstake_requests_with_finalizable;
          test_case
            "unfinalizable"
            `Quick
            test_parse_unstake_requests_with_unfinalizable;
        ] );
      ( "parse_delegate_aggregate",
        [
          test_case "full" `Quick test_parse_delegate_aggregate_full;
          test_case
            "deactivated"
            `Quick
            test_parse_delegate_aggregate_deactivated;
          test_case "minimal" `Quick test_parse_delegate_aggregate_minimal;
        ] );
      ( "parse_voting_info",
        [
          test_case "proposal period" `Quick test_parse_voting_info_proposal;
          test_case
            "exploration period"
            `Quick
            test_parse_voting_info_exploration;
          test_case "cooldown period" `Quick test_parse_voting_info_cooldown;
          test_case "promotion period" `Quick test_parse_voting_info_promotion;
          test_case "adoption period" `Quick test_parse_voting_info_adoption;
          test_case
            "unknown kind defaults"
            `Quick
            test_parse_voting_info_unknown_kind;
          test_case
            "no current proposal"
            `Quick
            test_parse_voting_info_no_current_proposal;
        ] );
      ( "format_tez",
        [
          test_case "zero" `Quick test_format_tez_zero;
          test_case "one tez" `Quick test_format_tez_one;
          test_case "fractional" `Quick test_format_tez_fractional;
          test_case "large" `Quick test_format_tez_large;
          test_case "millions" `Quick test_format_tez_millions;
          test_case "small fraction" `Quick test_format_tez_small_fraction;
          test_case "invalid" `Quick test_format_tez_invalid;
        ] );
      ( "format_staking_limit",
        [
          test_case "zero" `Quick test_format_staking_limit_zero;
          test_case "five" `Quick test_format_staking_limit_five;
          test_case "nine" `Quick test_format_staking_limit_nine;
        ] );
      ( "format_baking_edge",
        [
          test_case "zero" `Quick test_format_baking_edge_zero;
          test_case "ten percent" `Quick test_format_baking_edge_ten;
          test_case "hundred percent" `Quick test_format_baking_edge_hundred;
        ] );
      ( "cache",
        [
          test_case "set and get" `Quick test_cache_set_and_get;
          test_case "get missing" `Quick test_cache_get_missing;
          test_case "get_all" `Quick test_cache_get_all;
          test_case "clear" `Quick test_cache_clear;
          test_case "is_stale fresh" `Quick test_is_stale_fresh;
          test_case "is_stale old" `Quick test_is_stale_old;
        ] );
      ( "voting_cache",
        [
          test_case "set and get" `Quick test_voting_cache_set_and_get;
          test_case "missing endpoint" `Quick test_voting_cache_missing;
        ] );
      ( "string_of helpers",
        [
          test_case "ballot_vote" `Quick test_string_of_ballot_vote;
          test_case
            "voting_period_kind"
            `Quick
            test_string_of_voting_period_kind;
        ] );
    ]
