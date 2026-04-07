(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Tests for Gen_completion_helpers — pure functions used by the completion
    script generator. *)

open Alcotest
module H = Gen_completion_helpers

(* ============================================================ *)
(* escape_zsh_single *)
(* ============================================================ *)

let test_escape_zsh_single_no_quote () =
  check string "no quote unchanged" "hello" (H.escape_zsh_single "hello")

let test_escape_zsh_single_with_quote () =
  check string "single quote escaped" "it'\\''s" (H.escape_zsh_single "it's")

let test_escape_zsh_single_multiple_quotes () =
  check string "multiple quotes" "a'\\''b'\\''c" (H.escape_zsh_single "a'b'c")

let test_escape_zsh_single_empty () =
  check string "empty string" "" (H.escape_zsh_single "")

(* ============================================================ *)
(* escape_zsh_description *)
(* ============================================================ *)

let test_escape_zsh_description_colon () =
  check string "colon escaped" "a\\:b" (H.escape_zsh_description "a:b")

let test_escape_zsh_description_close_bracket () =
  check string "close bracket escaped" "a\\]b" (H.escape_zsh_description "a]b")

let test_escape_zsh_description_open_bracket () =
  check string "open bracket escaped" "a\\[b" (H.escape_zsh_description "a[b")

let test_escape_zsh_description_both_brackets () =
  check
    string
    "both brackets escaped"
    "a\\[b\\]"
    (H.escape_zsh_description "a[b]")

let test_escape_zsh_description_quote () =
  check
    string
    "single quote escaped"
    "it'\\''s"
    (H.escape_zsh_description "it's")

let test_escape_zsh_description_combined () =
  (* Simulate a real doc string with colons, brackets, and quotes *)
  let input = "Example: 'tz1abc:block'" in
  let result = H.escape_zsh_description input in
  (* Verify \: appears where : was, quotes shell-escaped as '\'' *)
  check string "combined escaping" "Example\\: '\\''tz1abc\\:block'\\''" result

let test_escape_zsh_description_plain () =
  check
    string
    "plain text unchanged"
    "hello world"
    (H.escape_zsh_description "hello world")

let test_escape_zsh_description_full50 () =
  (* Regression: 'full:50' must not produce bare colon in value position *)
  let result = H.escape_zsh_description "full:50" in
  check string "full:50 colon escaped" "full\\:50" result

(* ============================================================ *)
(* is_valid_cmd_name *)
(* ============================================================ *)

let test_is_valid_cmd_name_simple () =
  check bool "simple name" true (H.is_valid_cmd_name "baker")

let test_is_valid_cmd_name_with_dash () =
  check bool "dash allowed" true (H.is_valid_cmd_name "install-node")

let test_is_valid_cmd_name_with_underscore () =
  check bool "underscore allowed" true (H.is_valid_cmd_name "my_cmd")

let test_is_valid_cmd_name_rejects_equals () =
  check
    bool
    "equals rejected"
    false
    (H.is_valid_cmd_name "--octez-version=VERSION")

let test_is_valid_cmd_name_rejects_bracket () =
  check bool "bracket rejected" false (H.is_valid_cmd_name "--flag[=VAL]")

let test_is_valid_cmd_name_rejects_slash () =
  check bool "slash rejected" false (H.is_valid_cmd_name "path/to/cmd")

let test_is_valid_cmd_name_rejects_empty () =
  check bool "empty rejected" false (H.is_valid_cmd_name "")

let test_is_valid_cmd_name_alphanumeric () =
  check bool "alphanumeric" true (H.is_valid_cmd_name "cmd123")

(* ============================================================ *)
(* quote_shell *)
(* ============================================================ *)

let test_quote_shell_simple () =
  check
    string
    "simple path quoted"
    "'/usr/bin/om'"
    (H.quote_shell "/usr/bin/om")

let test_quote_shell_with_spaces () =
  check
    string
    "path with spaces"
    "'/path/with spaces/om'"
    (H.quote_shell "/path/with spaces/om")

let test_quote_shell_with_single_quote () =
  check
    string
    "path with single quote"
    "'/it'\\''s/om'"
    (H.quote_shell "/it's/om")

let test_quote_shell_empty () =
  check string "empty string quoted" "''" (H.quote_shell "")

let test_quote_shell_metacharacters () =
  (* Shell metacharacters are safely quoted *)
  check
    string
    "metacharacters quoted"
    "'/path/$HOME/om'"
    (H.quote_shell "/path/$HOME/om")

(* ============================================================ *)
(* Test Suite *)
(* ============================================================ *)

let escape_single_tests =
  [
    ("no quote unchanged", `Quick, test_escape_zsh_single_no_quote);
    ("single quote escaped", `Quick, test_escape_zsh_single_with_quote);
    ("multiple quotes", `Quick, test_escape_zsh_single_multiple_quotes);
    ("empty string", `Quick, test_escape_zsh_single_empty);
  ]

let escape_description_tests =
  [
    ("colon escaped", `Quick, test_escape_zsh_description_colon);
    ("close bracket escaped", `Quick, test_escape_zsh_description_close_bracket);
    ("open bracket escaped", `Quick, test_escape_zsh_description_open_bracket);
    ("both brackets escaped", `Quick, test_escape_zsh_description_both_brackets);
    ("single quote escaped", `Quick, test_escape_zsh_description_quote);
    ("combined escaping", `Quick, test_escape_zsh_description_combined);
    ("plain text unchanged", `Quick, test_escape_zsh_description_plain);
    ( "full:50 colon escaped (regression)",
      `Quick,
      test_escape_zsh_description_full50 );
  ]

let is_valid_cmd_name_tests =
  [
    ("simple name", `Quick, test_is_valid_cmd_name_simple);
    ("dash allowed", `Quick, test_is_valid_cmd_name_with_dash);
    ("underscore allowed", `Quick, test_is_valid_cmd_name_with_underscore);
    ("equals rejected", `Quick, test_is_valid_cmd_name_rejects_equals);
    ("bracket rejected", `Quick, test_is_valid_cmd_name_rejects_bracket);
    ("slash rejected", `Quick, test_is_valid_cmd_name_rejects_slash);
    ("empty rejected", `Quick, test_is_valid_cmd_name_rejects_empty);
    ("alphanumeric", `Quick, test_is_valid_cmd_name_alphanumeric);
  ]

let quote_shell_tests =
  [
    ("simple path", `Quick, test_quote_shell_simple);
    ("path with spaces", `Quick, test_quote_shell_with_spaces);
    ("path with single quote", `Quick, test_quote_shell_with_single_quote);
    ("empty string", `Quick, test_quote_shell_empty);
    ("metacharacters", `Quick, test_quote_shell_metacharacters);
  ]

let () =
  Alcotest.run
    "Gen_completion_helpers"
    [
      ("escape_zsh_single", escape_single_tests);
      ("escape_zsh_description", escape_description_tests);
      ("is_valid_cmd_name", is_valid_cmd_name_tests);
      ("quote_shell", quote_shell_tests);
    ]
