(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

(** Tests for Help_parser module - CLI help output parsing
    
    Tests cover:
    - Argument kind classification (toggle vs value types)
    - Option name extraction and formatting
    - Value type inference from help text
    - Helper string utilities
*)

open Alcotest
open Octez_manager_lib

(* ============================================================ *)
(* Helper Function Tests *)
(* ============================================================ *)

let test_contains_found () =
  let result = Help_parser.contains ~needle:"test" "this is a test string" in
  check bool "needle found" true result

let test_contains_not_found () =
  let result = Help_parser.contains ~needle:"missing" "this is a test string" in
  check bool "needle not found" false result

let test_contains_empty_needle () =
  let result = Help_parser.contains ~needle:"" "any string" in
  check bool "empty needle always matches" true result

let test_trim_nonempty_with_spaces () =
  let result = Help_parser.trim_nonempty "  text  " in
  check (option string) "trims spaces" (Some "text") result

let test_trim_nonempty_empty_string () =
  let result = Help_parser.trim_nonempty "   " in
  check (option string) "empty returns None" None result

let test_primary_name_prefers_long () =
  let result = Help_parser.primary_name ["-p"; "--port"] in
  check string "prefers --long" "--port" result

let test_primary_name_uses_first_if_no_long () =
  let result = Help_parser.primary_name ["-p"; "-P"] in
  check string "uses first if no long" "-p" result

let test_display_names_filters_to_long () =
  let result = Help_parser.display_names ["-p"; "--port"; "--Port"] in
  check int "only long names" 2 (List.length result)

(* ============================================================ *)
(* Argument Kind Classification Tests *)
(* ============================================================ *)

let test_classify_toggle_no_arg () =
  let kind =
    Help_parser.classify_arg_kind
      ~names:["--verbose"]
      ~arg:None
      ~doc:"Enable verbose mode"
  in
  check bool "toggle has no arg" true (kind = Help_parser.Toggle)

let test_classify_addr_port () =
  let kind =
    Help_parser.classify_arg_kind
      ~names:["--rpc-addr"]
      ~arg:(Some "ADDR:PORT")
      ~doc:"RPC server address"
  in
  match kind with
  | Help_parser.Value Help_parser.Addr_port ->
      check bool "addr:port detected" true true
  | _ -> fail "should detect addr:port"

let test_classify_port () =
  let kind =
    Help_parser.classify_arg_kind
      ~names:["--port"]
      ~arg:(Some "NUM")
      ~doc:"Port number"
  in
  match kind with
  | Help_parser.Value Help_parser.Port -> check bool "port detected" true true
  | _ -> fail "should detect port"

let test_classify_file () =
  let kind =
    Help_parser.classify_arg_kind
      ~names:["--config"]
      ~arg:(Some "FILE")
      ~doc:"Configuration file path"
  in
  match kind with
  | Help_parser.Value Help_parser.File -> check bool "file detected" true true
  | _ -> fail "should detect file"

let test_classify_dir () =
  let kind =
    Help_parser.classify_arg_kind
      ~names:["--data-dir"]
      ~arg:(Some "DIR")
      ~doc:"Data directory path"
  in
  match kind with
  | Help_parser.Value Help_parser.Dir -> check bool "dir detected" true true
  | _ -> fail "should detect dir"

let test_classify_number () =
  let kind =
    Help_parser.classify_arg_kind
      ~names:["--connections"]
      ~arg:(Some "N")
      ~doc:"Number of connections"
  in
  match kind with
  | Help_parser.Value Help_parser.Number ->
      check bool "number detected" true true
  | _ -> fail "should detect number"

let test_classify_text_fallback () =
  let kind =
    Help_parser.classify_arg_kind
      ~names:["--name"]
      ~arg:(Some "STRING")
      ~doc:"Instance name"
  in
  match kind with
  | Help_parser.Value Help_parser.Text -> check bool "text fallback" true true
  | _ -> fail "should default to text"

(* ============================================================ *)
(* Test Suite *)
(* ============================================================ *)

let helper_tests =
  [
    ("contains found", `Quick, test_contains_found);
    ("contains not found", `Quick, test_contains_not_found);
    ("contains empty needle", `Quick, test_contains_empty_needle);
    ("trim nonempty with spaces", `Quick, test_trim_nonempty_with_spaces);
    ("trim nonempty empty", `Quick, test_trim_nonempty_empty_string);
    ("primary name prefers long", `Quick, test_primary_name_prefers_long);
    ( "primary name first if no long",
      `Quick,
      test_primary_name_uses_first_if_no_long );
    ("display names filters long", `Quick, test_display_names_filters_to_long);
  ]

let classification_tests =
  [
    ("classify toggle", `Quick, test_classify_toggle_no_arg);
    ("classify addr:port", `Quick, test_classify_addr_port);
    ("classify port", `Quick, test_classify_port);
    ("classify file", `Quick, test_classify_file);
    ("classify dir", `Quick, test_classify_dir);
    ("classify number", `Quick, test_classify_number);
    ("classify text fallback", `Quick, test_classify_text_fallback);
  ]

let () =
  Alcotest.run
    "Help_parser"
    [("helpers", helper_tests); ("classification", classification_tests)]
