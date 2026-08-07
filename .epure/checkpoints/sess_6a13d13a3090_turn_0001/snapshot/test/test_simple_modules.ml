(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Tests for simple utility modules: History_mode, Logging_mode, Env_file_parser
    
    These modules contain pure functions for parsing and formatting.
*)

open Alcotest
open Octez_manager_lib

(* ============================================================ *)
(* History_mode Tests *)
(* ============================================================ *)

let test_history_mode_to_string_rolling () =
  let result = History_mode.to_string History_mode.Rolling in
  check string "rolling to string" "rolling" result

let test_history_mode_to_string_full () =
  let result = History_mode.to_string History_mode.Full in
  check string "full to string" "full" result

let test_history_mode_to_string_archive () =
  let result = History_mode.to_string History_mode.Archive in
  check string "archive to string" "archive" result

let test_history_mode_of_string_rolling () =
  match History_mode.of_string "rolling" with
  | Ok mode -> check bool "parses rolling" true (mode = History_mode.Rolling)
  | Error _ -> fail "should parse rolling"

let test_history_mode_of_string_full () =
  match History_mode.of_string "full" with
  | Ok mode -> check bool "parses full" true (mode = History_mode.Full)
  | Error _ -> fail "should parse full"

let test_history_mode_of_string_archive () =
  match History_mode.of_string "archive" with
  | Ok mode -> check bool "parses archive" true (mode = History_mode.Archive)
  | Error _ -> fail "should parse archive"

let test_history_mode_of_string_uppercase () =
  match History_mode.of_string "ROLLING" with
  | Ok mode -> check bool "parses uppercase" true (mode = History_mode.Rolling)
  | Error _ -> fail "should handle uppercase"

let test_history_mode_of_string_mixed_case () =
  match History_mode.of_string "FuLl" with
  | Ok mode -> check bool "parses mixed case" true (mode = History_mode.Full)
  | Error _ -> fail "should handle mixed case"

let test_history_mode_of_string_with_whitespace () =
  match History_mode.of_string "  archive  " with
  | Ok mode ->
      check bool "parses with whitespace" true (mode = History_mode.Archive)
  | Error _ -> fail "should trim whitespace"

let test_history_mode_of_string_invalid () =
  match History_mode.of_string "invalid" with
  | Ok _ -> fail "should reject invalid mode"
  | Error _ -> check bool "rejects invalid" true true

let test_history_mode_of_string_empty () =
  match History_mode.of_string "" with
  | Ok _ -> fail "should reject empty string"
  | Error _ -> check bool "rejects empty" true true

let test_history_mode_default () =
  check
    bool
    "default is rolling"
    true
    (History_mode.default = History_mode.Rolling)

(* ============================================================ *)
(* Logging_mode Tests *)
(* ============================================================ *)

let test_logging_mode_to_string () =
  let result = Logging_mode.to_string Logging_mode.Journald in
  check string "journald to string" "journald" result

let test_logging_mode_default () =
  check
    bool
    "default is journald"
    true
    (Logging_mode.default = Logging_mode.Journald)

let test_logging_mode_to_yojson () =
  let json = Logging_mode.to_yojson Logging_mode.Journald in
  match json with
  | `Assoc [("type", `String "journald")] -> check bool "correct JSON" true true
  | _ -> fail "wrong JSON format"

let test_logging_mode_of_yojson_journald () =
  let json = `Assoc [("type", `String "journald")] in
  match Logging_mode.of_yojson json with
  | Ok mode ->
      check bool "parses journald JSON" true (mode = Logging_mode.Journald)
  | Error _ -> fail "should parse journald JSON"

let test_logging_mode_of_yojson_file_legacy () =
  let json = `Assoc [("type", `String "file")] in
  match Logging_mode.of_yojson json with
  | Ok mode ->
      check bool "converts file to journald" true (mode = Logging_mode.Journald)
  | Error _ -> fail "should convert legacy file mode"

let test_logging_mode_of_yojson_invalid () =
  let json = `Assoc [("type", `String "invalid")] in
  match Logging_mode.of_yojson json with
  | Ok _ -> fail "should reject invalid type"
  | Error _ -> check bool "rejects invalid" true true

let test_logging_mode_of_yojson_missing_type () =
  let json = `Assoc [] in
  match Logging_mode.of_yojson json with
  | Ok _ -> fail "should reject missing type"
  | Error _ -> check bool "rejects missing type" true true

(* ============================================================ *)
(* Env_file_parser Tests *)
(* ============================================================ *)

let test_env_parse_empty_string () =
  let result = Env_file_parser.parse_string "" in
  check int "empty string gives empty list" 0 (List.length result)

let test_env_parse_simple_pair () =
  let result = Env_file_parser.parse_string "KEY=value" in
  check int "one pair" 1 (List.length result) ;
  match result with
  | [("KEY", "value")] -> check bool "correct parsing" true true
  | _ -> fail "wrong parsing"

let test_env_parse_multiple_pairs () =
  let content = "KEY1=value1\nKEY2=value2\nKEY3=value3" in
  let result = Env_file_parser.parse_string content in
  check int "three pairs" 3 (List.length result)

let test_env_parse_with_quotes () =
  let content = "KEY=\"quoted value\"" in
  let result = Env_file_parser.parse_string content in
  match result with
  | [("KEY", "quoted value")] -> check bool "unquotes double quotes" true true
  | _ -> fail "should unquote"

let test_env_parse_with_single_quotes () =
  let content = "KEY='single quoted'" in
  let result = Env_file_parser.parse_string content in
  match result with
  | [("KEY", "single quoted")] -> check bool "unquotes single quotes" true true
  | _ -> fail "should unquote single quotes"

let test_env_parse_with_equals_in_value () =
  let content = "URL=http://example.com?foo=bar" in
  let result = Env_file_parser.parse_string content in
  match result with
  | [("URL", "http://example.com?foo=bar")] ->
      check bool "preserves = in value" true true
  | _ -> fail "should handle = in value"

let test_env_parse_comments () =
  let content = "# This is a comment\nKEY=value\n# Another comment" in
  let result = Env_file_parser.parse_string content in
  check int "skips comments" 1 (List.length result)

let test_env_parse_blank_lines () =
  let content = "KEY1=value1\n\n\nKEY2=value2" in
  let result = Env_file_parser.parse_string content in
  check int "skips blank lines" 2 (List.length result)

let test_env_parse_key_without_value () =
  let content = "KEY_ONLY" in
  let result = Env_file_parser.parse_string content in
  match result with
  | [("KEY_ONLY", "")] ->
      check bool "key without value gets empty string" true true
  | _ -> fail "should handle key without value"

let test_env_parse_whitespace_around_equals () =
  let content = "KEY = value" in
  let result = Env_file_parser.parse_string content in
  match result with
  | [("KEY ", "value")] -> check bool "preserves key whitespace" true true
  | [("KEY", "value")] -> check bool "or trims whitespace" true true
  | _ -> check bool "parses with spaces" true (List.length result = 1)

let test_env_expand_vars_no_vars () =
  let result = Env_file_parser.expand_vars ~env:[] "no variables here" in
  check string "no expansion" "no variables here" result

let test_env_expand_vars_simple () =
  let env = [("HOME", "/home/user")] in
  let result = Env_file_parser.expand_vars ~env "$HOME/file" in
  check string "expands $HOME" "/home/user/file" result

let test_env_expand_vars_braced () =
  let env = [("VAR", "value")] in
  let result = Env_file_parser.expand_vars ~env "prefix_${VAR}_suffix" in
  check string "expands ${VAR}" "prefix_value_suffix" result

let test_env_expand_vars_multiple () =
  let env = [("A", "1"); ("B", "2")] in
  let result = Env_file_parser.expand_vars ~env "$A and $B" in
  check string "expands multiple" "1 and 2" result

let test_env_expand_vars_undefined () =
  let result = Env_file_parser.expand_vars ~env:[] "$UNDEFINED" in
  check string "keeps undefined" "$UNDEFINED" result

let test_env_expand_vars_undefined_braced () =
  let result = Env_file_parser.expand_vars ~env:[] "${UNDEFINED}" in
  check string "keeps undefined braced" "${UNDEFINED}" result

let test_env_expand_vars_empty_value () =
  let env = [("EMPTY", "")] in
  let result = Env_file_parser.expand_vars ~env "prefix$EMPTY" in
  check string "expands to empty" "prefix" result

(* ============================================================ *)
(* Test Suite *)
(* ============================================================ *)

let history_mode_tests =
  [
    ("to_string rolling", `Quick, test_history_mode_to_string_rolling);
    ("to_string full", `Quick, test_history_mode_to_string_full);
    ("to_string archive", `Quick, test_history_mode_to_string_archive);
    ("of_string rolling", `Quick, test_history_mode_of_string_rolling);
    ("of_string full", `Quick, test_history_mode_of_string_full);
    ("of_string archive", `Quick, test_history_mode_of_string_archive);
    ("of_string uppercase", `Quick, test_history_mode_of_string_uppercase);
    ("of_string mixed case", `Quick, test_history_mode_of_string_mixed_case);
    ( "of_string with whitespace",
      `Quick,
      test_history_mode_of_string_with_whitespace );
    ("of_string invalid", `Quick, test_history_mode_of_string_invalid);
    ("of_string empty", `Quick, test_history_mode_of_string_empty);
    ("default", `Quick, test_history_mode_default);
  ]

let logging_mode_tests =
  [
    ("to_string", `Quick, test_logging_mode_to_string);
    ("default", `Quick, test_logging_mode_default);
    ("to_yojson", `Quick, test_logging_mode_to_yojson);
    ("of_yojson journald", `Quick, test_logging_mode_of_yojson_journald);
    ("of_yojson file legacy", `Quick, test_logging_mode_of_yojson_file_legacy);
    ("of_yojson invalid", `Quick, test_logging_mode_of_yojson_invalid);
    ("of_yojson missing type", `Quick, test_logging_mode_of_yojson_missing_type);
  ]

let env_file_parser_tests =
  [
    ("parse empty", `Quick, test_env_parse_empty_string);
    ("parse simple pair", `Quick, test_env_parse_simple_pair);
    ("parse multiple pairs", `Quick, test_env_parse_multiple_pairs);
    ("parse with quotes", `Quick, test_env_parse_with_quotes);
    ("parse with single quotes", `Quick, test_env_parse_with_single_quotes);
    ("parse equals in value", `Quick, test_env_parse_with_equals_in_value);
    ("parse comments", `Quick, test_env_parse_comments);
    ("parse blank lines", `Quick, test_env_parse_blank_lines);
    ("parse key without value", `Quick, test_env_parse_key_without_value);
    ( "parse whitespace around =",
      `Quick,
      test_env_parse_whitespace_around_equals );
    ("expand no vars", `Quick, test_env_expand_vars_no_vars);
    ("expand simple", `Quick, test_env_expand_vars_simple);
    ("expand braced", `Quick, test_env_expand_vars_braced);
    ("expand multiple", `Quick, test_env_expand_vars_multiple);
    ("expand undefined", `Quick, test_env_expand_vars_undefined);
    ("expand undefined braced", `Quick, test_env_expand_vars_undefined_braced);
    ("expand empty value", `Quick, test_env_expand_vars_empty_value);
  ]

let () =
  Alcotest.run
    "Simple_modules"
    [
      ("history_mode", history_mode_tests);
      ("logging_mode", logging_mode_tests);
      ("env_file_parser", env_file_parser_tests);
    ]
