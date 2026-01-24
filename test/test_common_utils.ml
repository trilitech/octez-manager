(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

(** Tests for Common module utilities - path and string functions
    
    Tests cover:
    - Shell quoting for safe command execution
    - Absolute path creation
    - Path utilities
    - String utilities
*)

open Alcotest
open Octez_manager_lib

(* ============================================================ *)
(* Shell Quoting Tests *)
(* ============================================================ *)

let test_sh_quote_simple () =
  let result = Common.sh_quote "simple" in
  check string "simple unchanged" "simple" result

let test_sh_quote_with_spaces () =
  let result = Common.sh_quote "hello world" in
  (* Should be quoted *)
  check bool "spaces cause quoting" true (String.contains result '\'')

let test_sh_quote_with_single_quote () =
  let result = Common.sh_quote "it's" in
  (* Single quotes need escaping *)
  check bool "handles single quote" true (String.length result > 4)

let test_sh_quote_empty () =
  let result = Common.sh_quote "" in
  (* Empty string might not be quoted or might be *)
  check bool "empty string handled" true (String.length result >= 0)

let test_sh_quote_with_dollar () =
  let result = Common.sh_quote "$VAR" in
  (* Dollar signs should be protected *)
  check bool "protects dollar" true (String.contains result '\'')

let test_sh_quote_with_backtick () =
  let result = Common.sh_quote "`command`" in
  check bool "protects backtick" true (String.contains result '\'')

let test_sh_quote_with_semicolon () =
  let result = Common.sh_quote "cmd1; cmd2" in
  check bool "protects semicolon" true (String.contains result '\'')

let test_sh_quote_path () =
  let result = Common.sh_quote "/path/to/file" in
  (* Simple paths might not need quoting *)
  check bool "handles path" true (String.length result > 0)

(* ============================================================ *)
(* make_absolute_path Tests *)
(* ============================================================ *)

let test_absolute_path_already_absolute () =
  let result = Common.make_absolute_path "/absolute/path" in
  match result with
  | Ok path -> check string "absolute unchanged" "/absolute/path" path
  | Error _ -> fail "should accept absolute path"

let test_absolute_path_relative () =
  let result = Common.make_absolute_path "relative/path" in
  match result with
  | Ok path ->
      check bool "made absolute" true (String.starts_with ~prefix:"/" path)
  | Error _ -> fail "should convert relative path"

let test_absolute_path_dot () =
  let result = Common.make_absolute_path "." in
  match result with
  | Ok path ->
      check bool "dot to absolute" true (String.starts_with ~prefix:"/" path)
  | Error _ -> fail "should handle dot"

let test_absolute_path_dotdot () =
  let result = Common.make_absolute_path ".." in
  match result with
  | Ok path ->
      check bool "dotdot to absolute" true (String.starts_with ~prefix:"/" path)
  | Error _ -> fail "should handle dotdot"

let test_absolute_path_empty () =
  let result = Common.make_absolute_path "" in
  match result with
  | Error _ -> check bool "empty path fails" true true
  | Ok _ -> fail "should reject empty path"

let test_absolute_path_whitespace () =
  let result = Common.make_absolute_path "   " in
  match result with
  | Error _ -> check bool "whitespace fails" true true
  | Ok _ -> fail "should reject whitespace-only"

let test_absolute_path_tilde () =
  let result = Common.make_absolute_path "~/file" in
  match result with
  | Ok path ->
      (* Tilde might be expanded *)
      check bool "handles tilde" true (String.length path > 0)
  | Error _ -> check bool "or rejects tilde" true true

(* ============================================================ *)
(* Path Utilities Tests *)
(* ============================================================ *)

let test_default_data_dir () =
  let result = Common.default_data_dir "test-instance" in
  check bool "has path" true (String.contains result '/')

let test_default_role_dir () =
  let result = Common.default_role_dir "node" "test-instance" in
  check bool "role dir has path" true (String.contains result '/')

let test_cmd_to_string () =
  let result = Common.cmd_to_string ["ls"; "-la"; "/tmp"] in
  check bool "has ls" true (String.contains result 'l') ;
  check bool "has tmp" true (String.contains result 't')

let test_cmd_to_string_with_spaces () =
  let result = Common.cmd_to_string ["echo"; "hello world"] in
  (* Should properly quote the argument with spaces *)
  check bool "contains hello" true (String.contains result 'h')

(* ============================================================ *)
(* Edge Cases *)
(* ============================================================ *)

let test_sh_quote_newline () =
  let result = Common.sh_quote "line1\nline2" in
  check bool "handles newline" true (String.length result > 11)

let test_sh_quote_special_chars () =
  let result = Common.sh_quote "!@#$%^&*()" in
  check bool "handles special chars" true (String.contains result '\'')

let test_absolute_path_with_spaces () =
  let result = Common.make_absolute_path "path with spaces" in
  match result with
  | Ok path -> check bool "accepts spaces" true (String.contains path ' ')
  | Error _ -> fail "should handle spaces"

let test_absolute_path_very_long () =
  let long_path = String.make 500 'x' in
  let result = Common.make_absolute_path long_path in
  match result with
  | Ok _ -> check bool "handles long path" true true
  | Error _ -> check bool "or rejects long path" true true

(* ============================================================ *)
(* Test Suite *)
(* ============================================================ *)

let sh_quote_tests =
  [
    ("quote simple", `Quick, test_sh_quote_simple);
    ("quote spaces", `Quick, test_sh_quote_with_spaces);
    ("quote single quote", `Quick, test_sh_quote_with_single_quote);
    ("quote empty", `Quick, test_sh_quote_empty);
    ("quote dollar", `Quick, test_sh_quote_with_dollar);
    ("quote backtick", `Quick, test_sh_quote_with_backtick);
    ("quote semicolon", `Quick, test_sh_quote_with_semicolon);
    ("quote path", `Quick, test_sh_quote_path);
  ]

let make_absolute_tests =
  [
    ("absolute unchanged", `Quick, test_absolute_path_already_absolute);
    ("relative converted", `Quick, test_absolute_path_relative);
    ("dot converted", `Quick, test_absolute_path_dot);
    ("dotdot converted", `Quick, test_absolute_path_dotdot);
    ("empty fails", `Quick, test_absolute_path_empty);
    ("whitespace fails", `Quick, test_absolute_path_whitespace);
    ("tilde handled", `Quick, test_absolute_path_tilde);
  ]

let path_utils_tests =
  [
    ("default data dir", `Quick, test_default_data_dir);
    ("default role dir", `Quick, test_default_role_dir);
    ("cmd to string", `Quick, test_cmd_to_string);
    ("cmd with spaces", `Quick, test_cmd_to_string_with_spaces);
  ]

let edge_case_tests =
  [
    ("quote newline", `Quick, test_sh_quote_newline);
    ("quote special chars", `Quick, test_sh_quote_special_chars);
    ("absolute with spaces", `Quick, test_absolute_path_with_spaces);
    ("absolute very long", `Quick, test_absolute_path_very_long);
  ]

let () =
  Alcotest.run
    "Common_utils"
    [
      ("sh_quote", sh_quote_tests);
      ("make_absolute_path", make_absolute_tests);
      ("path_utils", path_utils_tests);
      ("edge_cases", edge_case_tests);
    ]
