(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Unit tests for Self_update_checker version parsing and comparison. *)

open Alcotest
module SUC = Octez_manager_lib.Self_update_checker

(* ── parse_version ───────────────────────────────────────────── *)

let test_parse_version_simple () =
  check (list int) "0.1.1" [0; 1; 1] (SUC.For_tests.parse_version "0.1.1")

let test_parse_version_two_parts () =
  check (list int) "1.2" [1; 2] (SUC.For_tests.parse_version "1.2")

let test_parse_version_leading_v () =
  check (list int) "v1.2.3" [1; 2; 3] (SUC.For_tests.parse_version "v1.2.3")

let test_parse_version_rc_suffix () =
  check
    (list int)
    "1.2.3-rc1"
    [1; 2; 3]
    (SUC.For_tests.parse_version "1.2.3-rc1")

let test_parse_version_empty () =
  check (list int) "empty" [] (SUC.For_tests.parse_version "")

let test_parse_version_garbage () =
  check (list int) "garbage" [] (SUC.For_tests.parse_version "abc.def")

let test_parse_version_single () =
  check (list int) "single" [42] (SUC.For_tests.parse_version "42")

(* ── is_rc ───────────────────────────────────────────────────── *)

let test_is_rc_true () =
  check bool "1.0.0-rc1" true (SUC.For_tests.is_rc "1.0.0-rc1")

let test_is_rc_false () = check bool "1.0.0" false (SUC.For_tests.is_rc "1.0.0")

let test_is_rc_double_dash () =
  check bool "1.0.0-beta-1" true (SUC.For_tests.is_rc "1.0.0-beta-1")

(* ── extract_rc_number ───────────────────────────────────────── *)

let test_extract_rc_number_rc1 () =
  check
    (option int)
    "rc1"
    (Some 1)
    (SUC.For_tests.extract_rc_number "1.0.0-rc1")

let test_extract_rc_number_rc42 () =
  check
    (option int)
    "rc42"
    (Some 42)
    (SUC.For_tests.extract_rc_number "1.0.0-rc42")

let test_extract_rc_number_not_rc () =
  check (option int) "not rc" None (SUC.For_tests.extract_rc_number "1.0.0")

let test_extract_rc_number_beta () =
  (* beta prefix != "rc" so extraction fails *)
  check (option int) "beta" None (SUC.For_tests.extract_rc_number "1.0.0-beta1")

(* ── compare_versions ────────────────────────────────────────── *)

let test_compare_equal () =
  check int "equal" 0 (SUC.compare_versions "1.0.0" "1.0.0")

let test_compare_less () =
  check bool "1.0.0 < 2.0.0" true (SUC.compare_versions "1.0.0" "2.0.0" < 0)

let test_compare_greater () =
  check bool "2.0.0 > 1.0.0" true (SUC.compare_versions "2.0.0" "1.0.0" > 0)

let test_compare_minor () =
  check bool "1.1.0 < 1.2.0" true (SUC.compare_versions "1.1.0" "1.2.0" < 0)

let test_compare_patch () =
  check bool "1.0.1 < 1.0.2" true (SUC.compare_versions "1.0.1" "1.0.2" < 0)

let test_compare_trailing_zero () =
  check int "1.0 == 1.0.0" 0 (SUC.compare_versions "1.0" "1.0.0")

let test_compare_rc_vs_release () =
  check bool "rc < release" true (SUC.compare_versions "1.0.0-rc1" "1.0.0" < 0)

let test_compare_release_vs_rc () =
  check bool "release > rc" true (SUC.compare_versions "1.0.0" "1.0.0-rc1" > 0)

let test_compare_rc1_vs_rc2 () =
  check bool "rc1 < rc2" true (SUC.compare_versions "1.0.0-rc1" "1.0.0-rc2" < 0)

let test_compare_leading_v () =
  check int "v prefix" 0 (SUC.compare_versions "v1.0.0" "1.0.0")

(* ── is_major_update ─────────────────────────────────────────── *)

let test_is_major_true () =
  check bool "0->1" true (SUC.is_major_update ~current:"0.5.0" ~latest:"1.0.0")

let test_is_major_false_minor () =
  check
    bool
    "minor"
    false
    (SUC.is_major_update ~current:"1.0.0" ~latest:"1.1.0")

let test_is_major_false_patch () =
  check
    bool
    "patch"
    false
    (SUC.is_major_update ~current:"1.0.0" ~latest:"1.0.1")

let test_is_major_same () =
  check bool "same" false (SUC.is_major_update ~current:"1.0.0" ~latest:"1.0.0")

(* ── Suite ───────────────────────────────────────────────────── *)

let () =
  run
    "Self_update_checker"
    [
      ( "parse_version",
        [
          test_case "simple" `Quick test_parse_version_simple;
          test_case "two parts" `Quick test_parse_version_two_parts;
          test_case "leading v" `Quick test_parse_version_leading_v;
          test_case "rc suffix" `Quick test_parse_version_rc_suffix;
          test_case "empty" `Quick test_parse_version_empty;
          test_case "garbage" `Quick test_parse_version_garbage;
          test_case "single" `Quick test_parse_version_single;
        ] );
      ( "is_rc",
        [
          test_case "true" `Quick test_is_rc_true;
          test_case "false" `Quick test_is_rc_false;
          test_case "double dash" `Quick test_is_rc_double_dash;
        ] );
      ( "extract_rc_number",
        [
          test_case "rc1" `Quick test_extract_rc_number_rc1;
          test_case "rc42" `Quick test_extract_rc_number_rc42;
          test_case "not rc" `Quick test_extract_rc_number_not_rc;
          test_case "beta" `Quick test_extract_rc_number_beta;
        ] );
      ( "compare_versions",
        [
          test_case "equal" `Quick test_compare_equal;
          test_case "less" `Quick test_compare_less;
          test_case "greater" `Quick test_compare_greater;
          test_case "minor" `Quick test_compare_minor;
          test_case "patch" `Quick test_compare_patch;
          test_case "trailing zero" `Quick test_compare_trailing_zero;
          test_case "rc vs release" `Quick test_compare_rc_vs_release;
          test_case "release vs rc" `Quick test_compare_release_vs_rc;
          test_case "rc1 vs rc2" `Quick test_compare_rc1_vs_rc2;
          test_case "leading v" `Quick test_compare_leading_v;
        ] );
      ( "is_major_update",
        [
          test_case "true" `Quick test_is_major_true;
          test_case "false minor" `Quick test_is_major_false_minor;
          test_case "false patch" `Quick test_is_major_false_patch;
          test_case "same" `Quick test_is_major_same;
        ] );
    ]
