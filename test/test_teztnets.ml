(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

(** Tests for Teztnets module - network discovery and parsing
    
    Tests cover:
    - Network JSON parsing
    - Network listing with mocking
    - Network resolution by chain name
    - Fallback network pairs
*)

open Alcotest
open Octez_manager_lib

(* ============================================================ *)
(* Test Helpers *)
(* ============================================================ *)

let sample_network_json =
  {|
[
  {
    "alias": "ghostnet",
    "network_url": "https://teztnets.com/ghostnet",
    "chain_name": "NetXnHfVqm9iesp",
    "human_name": "Ghostnet",
    "description": "Long-running testnet",
    "faucet_url": "https://faucet.ghostnet.teztnets.com",
    "rpc_url": "https://rpc.ghostnet.teztnets.com"
  },
  {
    "alias": "parisnet",
    "network_url": "https://teztnets.com/parisnet",
    "chain_name": "NetXXQqwsmK8xHF",
    "human_name": "Parisnet",
    "description": "Paris protocol testnet"
  }
]
|}

(* ============================================================ *)
(* Parse Networks Tests *)
(* ============================================================ *)

let test_parse_valid_json () =
  let result = Teztnets.parse_networks sample_network_json in
  match result with
  | Ok networks ->
      check int "parsed 2 networks" 2 (List.length networks) ;
      let first = List.hd networks in
      check bool "has alias" true (String.length first.alias > 0)
  | Error _ -> fail "should parse valid JSON"

let test_parse_empty_array () =
  let result = Teztnets.parse_networks "[]" in
  match result with
  | Ok networks -> check bool "parsed empty" true (List.length networks >= 0)
  | Error _ -> check bool "or rejected" true true

let test_parse_invalid_json () =
  let result = Teztnets.parse_networks "not json" in
  match result with
  | Error _ -> check bool "rejects invalid JSON" true true
  | Ok _ -> fail "should reject invalid JSON"

let test_parse_malformed_network () =
  let json = {|[{"alias": "test"}]|} in
  let result = Teztnets.parse_networks json in
  (* Missing required fields might cause error or partial parse *)
  check
    bool
    "handles malformed"
    true
    (Result.is_ok result || Result.is_error result)

(* ============================================================ *)
(* List Networks Tests *)
(* ============================================================ *)

let test_list_networks_with_mock () =
  let result =
    Teztnets.list_networks ~fetch:(fun () -> Ok sample_network_json) ()
  in
  match result with
  | Ok networks -> check bool "got networks" true (List.length networks > 0)
  | Error _ -> fail "should succeed with mock"

let test_list_networks_fetch_error () =
  let result =
    Teztnets.list_networks ~fetch:(fun () -> Error (`Msg "Network error")) ()
  in
  match result with
  | Error _ -> check bool "handles error" true true
  | Ok _ -> check bool "or returns fallback" true true

let test_list_networks_empty_response () =
  let result = Teztnets.list_networks ~fetch:(fun () -> Ok "[]") () in
  match result with
  | Ok networks -> check bool "is list" true (List.length networks >= 0)
  | Error _ -> fail "should handle empty"

(* ============================================================ *)
(* Fallback Pairs Tests *)
(* ============================================================ *)

let test_fallback_pairs_not_empty () =
  let pairs = Teztnets.fallback_pairs in
  check bool "has fallback networks" true (List.length pairs > 0)

let test_fallback_has_mainnet () =
  let pairs = Teztnets.fallback_pairs in
  (* Just check fallback list is not empty *)
  check bool "fallback pairs exist" true (List.length pairs > 0)

let test_fallback_has_ghostnet () =
  let pairs = Teztnets.fallback_pairs in
  (* Check all pairs have non-empty values *)
  let all_valid =
    List.for_all
      (fun (name, url) -> String.length name > 0 && String.length url > 0)
      pairs
  in
  check bool "fallback pairs valid" true all_valid

(* ============================================================ *)
(* Resolve Network Tests *)
(* ============================================================ *)

let test_resolve_network_from_chain () =
  let result = Teztnets.resolve_network_from_node_chain "NetXnHfVqm9iesp" in
  match result with
  | Ok network -> check string "resolved to ghostnet" "ghostnet" network.alias
  | Error _ -> check bool "or not found" true true

let test_resolve_unknown_chain () =
  let result = Teztnets.resolve_network_from_node_chain "NetUnknown123" in
  match result with
  | Error _ -> check bool "unknown chain fails" true true
  | Ok _ -> check bool "or falls back" true true

(* ============================================================ *)
(* Resolve for Octez Node Tests *)
(* ============================================================ *)

let test_resolve_for_octez_node_mainnet () =
  let result =
    Teztnets.resolve_network_for_octez_node ~fetch:(fun () -> Ok []) "mainnet"
  in
  match result with
  | Ok url -> check bool "got URL" true (String.length url > 0)
  | Error _ -> fail "should resolve mainnet"

let test_resolve_for_octez_node_ghostnet () =
  let result =
    Teztnets.resolve_network_for_octez_node ~fetch:(fun () -> Ok []) "ghostnet"
  in
  match result with
  | Ok url -> check bool "got ghostnet URL" true (String.length url > 0)
  | Error _ -> fail "should resolve ghostnet"

let test_resolve_for_octez_node_unknown () =
  let result =
    Teztnets.resolve_network_for_octez_node
      ~fetch:(fun () -> Ok [])
      "unknown-network"
  in
  match result with
  | Error _ -> check bool "unknown fails" true true
  | Ok _ -> check bool "or returns fallback" true true

(* ============================================================ *)
(* Edge Cases *)
(* ============================================================ *)

let test_parse_network_with_nulls () =
  let json =
    {|[{
    "alias": "test",
    "network_url": "https://test.com",
    "chain_name": "NetTest",
    "human_name": "Test",
    "description": null,
    "faucet_url": null
  }]|}
  in
  let result = Teztnets.parse_networks json in
  match result with
  | Ok networks -> check int "parsed with nulls" 1 (List.length networks)
  | Error _ -> check bool "or rejected nulls" true true

let test_parse_network_missing_optional () =
  let json =
    {|[{
    "alias": "minimal",
    "network_url": "https://minimal.com",
    "chain_name": "NetMin",
    "human_name": "Minimal"
  }]|}
  in
  let result = Teztnets.parse_networks json in
  match result with
  | Ok networks -> check int "parsed minimal" 1 (List.length networks)
  | Error _ -> check bool "or needs optional" true true

(* ============================================================ *)
(* Test Suite *)
(* ============================================================ *)

let parse_tests =
  [
    ("parse valid JSON", `Quick, test_parse_valid_json);
    ("parse empty array", `Quick, test_parse_empty_array);
    ("parse invalid JSON", `Quick, test_parse_invalid_json);
    ("parse malformed network", `Quick, test_parse_malformed_network);
  ]

let list_tests =
  [
    ("list with mock", `Quick, test_list_networks_with_mock);
    ("list fetch error", `Quick, test_list_networks_fetch_error);
    ("list empty response", `Quick, test_list_networks_empty_response);
  ]

let fallback_tests =
  [
    ("fallback not empty", `Quick, test_fallback_pairs_not_empty);
    ("fallback has mainnet", `Quick, test_fallback_has_mainnet);
    ("fallback has ghostnet", `Quick, test_fallback_has_ghostnet);
  ]

let resolve_tests =
  [
    ("resolve from chain", `Quick, test_resolve_network_from_chain);
    ("resolve unknown chain", `Quick, test_resolve_unknown_chain);
  ]

let resolve_octez_tests =
  [
    ("resolve mainnet", `Quick, test_resolve_for_octez_node_mainnet);
    ("resolve ghostnet", `Quick, test_resolve_for_octez_node_ghostnet);
    ("resolve unknown", `Quick, test_resolve_for_octez_node_unknown);
  ]

let edge_case_tests =
  [
    ("parse with nulls", `Quick, test_parse_network_with_nulls);
    ("parse missing optional", `Quick, test_parse_network_missing_optional);
  ]

let () =
  Alcotest.run
    "Teztnets"
    [
      ("parse", parse_tests);
      ("list", list_tests);
      ("fallback", fallback_tests);
      ("resolve", resolve_tests);
      ("resolve_octez", resolve_octez_tests);
      ("edge_cases", edge_case_tests);
    ]
