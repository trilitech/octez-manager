(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Extended tests for External_service_detector module
    
    Tests cover:
    - String search utilities
    - Unit name pattern matching
    - ExecStart command extraction
    - Chain ID to network mapping
    - Systemctl command construction
*)

open Alcotest
open Octez_manager_lib
module ESD = External_service_detector.For_tests

(* ============================================================ *)
(* String Contains Tests *)
(* ============================================================ *)

let test_string_contains_basic () =
  let result = ESD.string_contains ~needle:"test" "this is a test string" in
  check bool "finds substring" true result

let test_string_contains_not_found () =
  let result = ESD.string_contains ~needle:"missing" "this is a test" in
  check bool "missing substring" false result

let test_string_contains_empty_needle () =
  let result = ESD.string_contains ~needle:"" "any string" in
  check bool "empty needle always found" true result

let test_string_contains_empty_haystack () =
  let result = ESD.string_contains ~needle:"test" "" in
  check bool "empty haystack" false result

let test_string_contains_at_start () =
  let result = ESD.string_contains ~needle:"hello" "hello world" in
  check bool "at start" true result

let test_string_contains_at_end () =
  let result = ESD.string_contains ~needle:"world" "hello world" in
  check bool "at end" true result

let test_string_contains_exact_match () =
  let result = ESD.string_contains ~needle:"test" "test" in
  check bool "exact match" true result

(* ============================================================ *)
(* Unit Name Pattern Tests *)
(* ============================================================ *)

let test_is_managed_unit_name_valid () =
  let result = ESD.is_managed_unit_name "octez-node@mainnet.service" in
  check bool "valid managed unit" true result

let test_is_managed_unit_name_baker () =
  let result = ESD.is_managed_unit_name "octez-baker@ghostnet.service" in
  check bool "baker unit" true result

let test_is_managed_unit_name_dal () =
  let result = ESD.is_managed_unit_name "octez-dal-node@test.service" in
  check bool "dal node unit" true result

let test_is_managed_unit_name_missing_prefix () =
  let result = ESD.is_managed_unit_name "tezos-node@mainnet.service" in
  check bool "wrong prefix" false result

let test_is_managed_unit_name_no_at () =
  let result = ESD.is_managed_unit_name "octez-node-mainnet.service" in
  check bool "no @ symbol" false result

let test_is_managed_unit_name_multiple_at () =
  let result = ESD.is_managed_unit_name "octez-node@test@prod.service" in
  check bool "multiple @ symbols" false result

let test_is_managed_unit_name_missing_suffix () =
  let result = ESD.is_managed_unit_name "octez-node@mainnet" in
  check bool "missing .service suffix" false result

let test_is_managed_unit_name_wrong_suffix () =
  let result = ESD.is_managed_unit_name "octez-node@mainnet.timer" in
  check bool "wrong suffix" false result

(* ============================================================ *)
(* ExecStart Command Extraction Tests *)
(* ============================================================ *)

let test_extract_command_structured_format () =
  let input =
    "{ path=/bin/octez-node ; argv[]=/bin/octez-node run --network=mainnet ; \
     ignore_errors=no ; ... }"
  in
  match ESD.extract_command_from_systemd_format input with
  | Some cmd ->
      check bool "extracts command" true (String.contains cmd 'o') ;
      check bool "contains network" true (String.contains cmd 'n')
  | None -> fail "should extract command"

let test_extract_command_with_arguments () =
  let input =
    "{ path=/usr/bin/foo ; argv[]=/usr/bin/foo --arg1 value1 --arg2 ; ... }"
  in
  match ESD.extract_command_from_systemd_format input with
  | Some cmd ->
      check bool "contains foo" true (String.contains cmd 'f') ;
      check bool "contains args" true (String.contains cmd '-')
  | None -> fail "should extract command with args"

let test_extract_command_no_argv () =
  let input = "{ path=/bin/test ; ... }" in
  let result = ESD.extract_command_from_systemd_format input in
  check bool "no argv returns None" true (Option.is_none result)

let test_extract_command_malformed () =
  let input = "not a structured format" in
  let result = ESD.extract_command_from_systemd_format input in
  check bool "malformed returns None" true (Option.is_none result)

let test_extract_command_empty () =
  let input = "" in
  let result = ESD.extract_command_from_systemd_format input in
  check bool "empty returns None" true (Option.is_none result)

(* ============================================================ *)
(* Chain ID to Network Mapping Tests *)
(* ============================================================ *)

let test_chain_id_mainnet () =
  match ESD.chain_id_to_network "NetXdQprcVkpaWU" with
  | Some network -> check string "mainnet chain id" "mainnet" network
  | None -> fail "should recognize mainnet"

let test_chain_id_ghostnet () =
  match ESD.chain_id_to_network "NetXnHfVqm9iesp" with
  | Some network -> check string "ghostnet chain id" "ghostnet" network
  | None -> fail "should recognize ghostnet"

let test_chain_id_shadownet () =
  match ESD.chain_id_to_network "NetXsqzbfFenSTS" with
  | Some network -> check string "shadownet chain id" "shadownet" network
  | None -> fail "should recognize shadownet"

let test_chain_id_unknown () =
  let result = ESD.chain_id_to_network "NetUnknownChainId" in
  check bool "unknown chain id" true (Option.is_none result)

let test_chain_id_empty () =
  let result = ESD.chain_id_to_network "" in
  check bool "empty chain id" true (Option.is_none result)

let test_chain_id_case_sensitive () =
  let result = ESD.chain_id_to_network "netxdqprcvkpawu" in
  check bool "lowercase not recognized" true (Option.is_none result)

(* ============================================================ *)
(* Systemctl Command Construction Tests *)
(* ============================================================ *)

let test_systemctl_cmd () =
  let result = ESD.systemctl_cmd () in
  check bool "returns list" true (List.length result >= 1) ;
  check bool "contains systemctl" true (List.mem "systemctl" result)

(* ============================================================ *)
(* Test Suite *)
(* ============================================================ *)

let string_contains_tests =
  [
    ("contains basic", `Quick, test_string_contains_basic);
    ("contains not found", `Quick, test_string_contains_not_found);
    ("contains empty needle", `Quick, test_string_contains_empty_needle);
    ("contains empty haystack", `Quick, test_string_contains_empty_haystack);
    ("contains at start", `Quick, test_string_contains_at_start);
    ("contains at end", `Quick, test_string_contains_at_end);
    ("contains exact match", `Quick, test_string_contains_exact_match);
  ]

let unit_name_tests =
  [
    ("managed unit name valid", `Quick, test_is_managed_unit_name_valid);
    ("managed unit name baker", `Quick, test_is_managed_unit_name_baker);
    ("managed unit name dal", `Quick, test_is_managed_unit_name_dal);
    ( "managed unit name wrong prefix",
      `Quick,
      test_is_managed_unit_name_missing_prefix );
    ("managed unit name no @", `Quick, test_is_managed_unit_name_no_at);
    ( "managed unit name multiple @",
      `Quick,
      test_is_managed_unit_name_multiple_at );
    ( "managed unit name missing suffix",
      `Quick,
      test_is_managed_unit_name_missing_suffix );
    ( "managed unit name wrong suffix",
      `Quick,
      test_is_managed_unit_name_wrong_suffix );
  ]

let extract_command_tests =
  [
    ("extract structured format", `Quick, test_extract_command_structured_format);
    ("extract with arguments", `Quick, test_extract_command_with_arguments);
    ("extract no argv", `Quick, test_extract_command_no_argv);
    ("extract malformed", `Quick, test_extract_command_malformed);
    ("extract empty", `Quick, test_extract_command_empty);
  ]

let chain_id_tests =
  [
    ("chain id mainnet", `Quick, test_chain_id_mainnet);
    ("chain id ghostnet", `Quick, test_chain_id_ghostnet);
    ("chain id shadownet", `Quick, test_chain_id_shadownet);
    ("chain id unknown", `Quick, test_chain_id_unknown);
    ("chain id empty", `Quick, test_chain_id_empty);
    ("chain id case sensitive", `Quick, test_chain_id_case_sensitive);
  ]

let systemctl_tests = [("systemctl_cmd", `Quick, test_systemctl_cmd)]

let () =
  Alcotest.run
    "External_service_detector_extended"
    [
      ("string_contains", string_contains_tests);
      ("unit_name_pattern", unit_name_tests);
      ("extract_command", extract_command_tests);
      ("chain_id_mapping", chain_id_tests);
      ("systemctl", systemctl_tests);
    ]
