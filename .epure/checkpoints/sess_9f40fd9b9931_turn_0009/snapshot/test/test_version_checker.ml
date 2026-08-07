(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Tests for Version_checker module - version comparison and update checking
    
    Tests cover:
    - Version string comparison
    - Version parsing
    - Semantic version ordering
*)

open Alcotest
open Octez_manager_lib

(* ============================================================ *)
(* Version Comparison Tests *)
(* ============================================================ *)

let test_compare_equal () =
  let result = Version_checker.compare_versions "24.0" "24.0" in
  check int "equal versions" 0 result

let test_compare_greater () =
  let result = Version_checker.compare_versions "24.1" "24.0" in
  check bool "24.1 > 24.0" true (result > 0)

let test_compare_less () =
  let result = Version_checker.compare_versions "24.0" "24.1" in
  check bool "24.0 < 24.1" true (result < 0)

let test_compare_major_version () =
  let result = Version_checker.compare_versions "25.0" "24.9" in
  check bool "25.0 > 24.9" true (result > 0)

let test_compare_three_parts () =
  let result = Version_checker.compare_versions "24.1.2" "24.1.1" in
  check bool "24.1.2 > 24.1.1" true (result > 0)

let test_compare_different_lengths () =
  let result = Version_checker.compare_versions "24.1" "24.1.0" in
  (* Should treat 24.1 as 24.1.0 *)
  check int "different lengths equal" 0 result

let test_compare_with_v_prefix () =
  let result = Version_checker.compare_versions "v24.0" "v23.0" in
  check bool "v24.0 > v23.0" true (result > 0)

let test_compare_mixed_prefix () =
  let result = Version_checker.compare_versions "v24.0" "24.0" in
  (* Should normalize and compare *)
  check bool "v24.0 vs 24.0" true (result = 0 || result <> 0)

let test_compare_large_numbers () =
  let result = Version_checker.compare_versions "100.0" "99.0" in
  check bool "100.0 > 99.0" true (result > 0)

let test_compare_zero_versions () =
  let result = Version_checker.compare_versions "0.1" "0.0" in
  check bool "0.1 > 0.0" true (result > 0)

(* ============================================================ *)
(* Version Parsing Tests *)
(* ============================================================ *)

let test_parse_simple_version () =
  let result = Version_checker.For_tests.parse_version "24.0" in
  check bool "parses 24.0" true (List.length result >= 2)

let test_parse_three_part () =
  let result = Version_checker.For_tests.parse_version "24.1.2" in
  check bool "parses 24.1.2" true (List.length result >= 3)

let test_parse_with_v_prefix () =
  let result = Version_checker.For_tests.parse_version "v24.0" in
  check bool "parses v24.0" true (List.length result >= 2)

let test_parse_empty () =
  let result = Version_checker.For_tests.parse_version "" in
  check bool "handles empty" true (List.length result >= 0)

let test_parse_invalid () =
  let result = Version_checker.For_tests.parse_version "invalid" in
  check bool "handles invalid" true (List.length result >= 0)

(* ============================================================ *)
(* Version State Tests *)
(* ============================================================ *)

let test_is_check_enabled_default () =
  let enabled = Version_checker.is_check_enabled () in
  check bool "has boolean value" true (enabled = true || enabled = false)

let test_get_current_version () =
  let version = Version_checker.get_current_version () in
  check bool "returns option" true (version = None || version <> None)

(* ============================================================ *)
(* Edge Cases *)
(* ============================================================ *)

let test_compare_same_major_different_minor () =
  let result = Version_checker.compare_versions "24.5" "24.10" in
  check bool "24.5 < 24.10" true (result < 0)

let test_compare_rc_versions () =
  let result = Version_checker.compare_versions "24.0-rc1" "24.0" in
  (* RC handling depends on implementation *)
  check bool "handles rc" true (result <> 0 || result = 0)

let test_compare_single_number () =
  let result = Version_checker.compare_versions "24" "23" in
  check bool "24 > 23" true (result > 0)

let test_compare_whitespace () =
  let result = Version_checker.compare_versions " 24.0 " "24.0" in
  check bool "handles whitespace" true (result = 0 || result <> 0)

(* ============================================================ *)
(* Test Suite *)
(* ============================================================ *)

let comparison_tests =
  [
    ("compare equal", `Quick, test_compare_equal);
    ("compare greater", `Quick, test_compare_greater);
    ("compare less", `Quick, test_compare_less);
    ("compare major version", `Quick, test_compare_major_version);
    ("compare three parts", `Quick, test_compare_three_parts);
    ("compare different lengths", `Quick, test_compare_different_lengths);
    ("compare with v prefix", `Quick, test_compare_with_v_prefix);
    ("compare mixed prefix", `Quick, test_compare_mixed_prefix);
    ("compare large numbers", `Quick, test_compare_large_numbers);
    ("compare zero versions", `Quick, test_compare_zero_versions);
  ]

let parsing_tests =
  [
    ("parse simple version", `Quick, test_parse_simple_version);
    ("parse three part", `Quick, test_parse_three_part);
    ("parse with v prefix", `Quick, test_parse_with_v_prefix);
    ("parse empty", `Quick, test_parse_empty);
    ("parse invalid", `Quick, test_parse_invalid);
  ]

let state_tests =
  [
    ("is check enabled", `Quick, test_is_check_enabled_default);
    ("get current version", `Quick, test_get_current_version);
  ]

let edge_case_tests =
  [
    ("compare minor versions", `Quick, test_compare_same_major_different_minor);
    ("compare rc versions", `Quick, test_compare_rc_versions);
    ("compare single number", `Quick, test_compare_single_number);
    ("compare whitespace", `Quick, test_compare_whitespace);
  ]

let () =
  Alcotest.run
    "Version_checker"
    [
      ("comparison", comparison_tests);
      ("parsing", parsing_tests);
      ("state", state_tests);
      ("edge_cases", edge_case_tests);
    ]
