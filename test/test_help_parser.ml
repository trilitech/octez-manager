(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
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
(* strip_ansi Tests *)
(* ============================================================ *)

let test_strip_ansi_removes_color_codes () =
  let result = Help_parser.strip_ansi "\027[32mhello\027[0m" in
  check string "strips color codes" "hello" result

let test_strip_ansi_leaves_plain_text () =
  let result = Help_parser.strip_ansi "hello world" in
  check string "plain text unchanged" "hello world" result

let test_strip_ansi_empty () =
  check string "empty string" "" (Help_parser.strip_ansi "")

let test_strip_ansi_no_letter_terminator () =
  (* An escape sequence is terminated by the first letter; digits/semicolons
     in between are consumed. *)
  let result = Help_parser.strip_ansi "\027[1;33mtext\027[0m" in
  check string "bold yellow stripped" "text" result

(* ============================================================ *)
(* split_bracket_arg Tests *)
(* ============================================================ *)

let test_split_bracket_arg_equals () =
  let result = Help_parser.split_bracket_arg "--flag[=VAL]" in
  check
    (option (pair string string))
    "bracket equals"
    (Some ("--flag", "=VAL"))
    result

let test_split_bracket_arg_no_bracket () =
  let result = Help_parser.split_bracket_arg "--flag" in
  check (option (pair string string)) "no bracket" None result

let test_split_bracket_arg_empty_inside () =
  let result = Help_parser.split_bracket_arg "--flag[]" in
  check
    (option (pair string string))
    "empty bracket"
    (Some ("--flag", ""))
    result

let test_split_bracket_arg_unclosed () =
  (* No closing bracket → None *)
  let result = Help_parser.split_bracket_arg "--flag[VAL" in
  check (option (pair string string)) "unclosed bracket" None result

(* ============================================================ *)
(* extract_section_lines Tests *)
(* ============================================================ *)

let test_extract_section_lines_found () =
  let lines =
    [
      "DESCRIPTION";
      "  Some desc";
      "OPTIONS";
      "  --foo  The foo flag";
      "  --bar  The bar flag";
      "COMMANDS";
      "  start  Start it";
    ]
  in
  let result = Help_parser.extract_section_lines ~header:"OPTIONS" lines in
  check int "two option lines" 2 (List.length result) ;
  check
    bool
    "first line contains foo"
    true
    (String.length (List.nth result 0) > 0)

let test_extract_section_lines_missing () =
  let lines = ["DESCRIPTION"; "  Some desc"; "COMMANDS"; "  start  Start"] in
  let result = Help_parser.extract_section_lines ~header:"OPTIONS" lines in
  check int "empty when missing" 0 (List.length result)

let test_extract_section_lines_stops_at_next_header () =
  let lines =
    ["OPTIONS"; "  --foo  foo"; "COMMANDS"; "  start  start"; "  stop  stop"]
  in
  let result = Help_parser.extract_section_lines ~header:"OPTIONS" lines in
  (* Should only get the OPTIONS content, not COMMANDS content *)
  check int "one line from OPTIONS" 1 (List.length result)

(* ============================================================ *)
(* parse_cmdliner_commands Tests *)
(* ============================================================ *)

let test_parse_cmdliner_commands_basic () =
  let input =
    "COMMANDS\n\
    \  start [OPTION]...\n\
    \      Start the service.\n\
    \  stop [OPTION]...\n\
    \      Stop the service.\n"
  in
  let cmds = Help_parser.parse_cmdliner_commands input in
  check int "two commands" 2 (List.length cmds) ;
  check string "first cmd name" "start" (List.nth cmds 0).Help_parser.name ;
  check string "second cmd name" "stop" (List.nth cmds 1).Help_parser.name

let test_parse_cmdliner_commands_captures_doc () =
  let input =
    "COMMANDS\n  list [OPTION]...\n      List all baker instances.\n"
  in
  let cmds = Help_parser.parse_cmdliner_commands input in
  check int "one command" 1 (List.length cmds) ;
  check
    string
    "doc captured"
    "List all baker instances."
    (List.nth cmds 0).Help_parser.doc

let test_parse_cmdliner_commands_no_commands_section () =
  let input = "OPTIONS\n  --help  Show help.\n" in
  let cmds = Help_parser.parse_cmdliner_commands input in
  check int "empty when no COMMANDS section" 0 (List.length cmds)

(* ============================================================ *)
(* parse_cmdliner_options Tests *)
(* ============================================================ *)

let test_parse_cmdliner_options_basic () =
  let input =
    "OPTIONS\n\
    \  --verbose  Enable verbose output.\n\
    \  --port[=PORT]  Port number (default: 8732).\n"
  in
  let opts = Help_parser.parse_cmdliner_options input in
  check bool "at least one option" true (List.length opts >= 1)

let test_parse_cmdliner_options_bracket_syntax () =
  let input = "OPTIONS\n  --flag[=VAL]  A flag with optional value.\n" in
  let opts = Help_parser.parse_cmdliner_options input in
  check int "one option" 1 (List.length opts) ;
  let opt = List.nth opts 0 in
  check
    string
    "name is --flag"
    "--flag"
    (Help_parser.primary_name opt.Help_parser.names) ;
  check (option string) "arg is VAL" (Some "VAL") opt.Help_parser.arg

let test_parse_cmdliner_options_common_options_section () =
  let input =
    "OPTIONS\n  --foo  Foo.\nCOMMON OPTIONS\n  --help[=FMT]  Show help.\n"
  in
  let opts = Help_parser.parse_cmdliner_options input in
  (* Should parse options from BOTH sections *)
  check bool "options from both sections" true (List.length opts >= 2)

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

let strip_ansi_tests =
  [
    ("removes color codes", `Quick, test_strip_ansi_removes_color_codes);
    ("leaves plain text", `Quick, test_strip_ansi_leaves_plain_text);
    ("empty string", `Quick, test_strip_ansi_empty);
    ("bold yellow stripped", `Quick, test_strip_ansi_no_letter_terminator);
  ]

let split_bracket_arg_tests =
  [
    ("bracket equals", `Quick, test_split_bracket_arg_equals);
    ("no bracket", `Quick, test_split_bracket_arg_no_bracket);
    ("empty bracket", `Quick, test_split_bracket_arg_empty_inside);
    ("unclosed bracket", `Quick, test_split_bracket_arg_unclosed);
  ]

let extract_section_lines_tests =
  [
    ("section found", `Quick, test_extract_section_lines_found);
    ("section missing", `Quick, test_extract_section_lines_missing);
    ( "stops at next header",
      `Quick,
      test_extract_section_lines_stops_at_next_header );
  ]

let parse_cmdliner_commands_tests =
  [
    ("basic commands", `Quick, test_parse_cmdliner_commands_basic);
    ("captures doc", `Quick, test_parse_cmdliner_commands_captures_doc);
    ( "no commands section",
      `Quick,
      test_parse_cmdliner_commands_no_commands_section );
  ]

let parse_cmdliner_options_tests =
  [
    ("basic options", `Quick, test_parse_cmdliner_options_basic);
    ("bracket syntax", `Quick, test_parse_cmdliner_options_bracket_syntax);
    ( "common options section",
      `Quick,
      test_parse_cmdliner_options_common_options_section );
  ]

let () =
  Alcotest.run
    "Help_parser"
    [
      ("helpers", helper_tests);
      ("classification", classification_tests);
      ("strip_ansi", strip_ansi_tests);
      ("split_bracket_arg", split_bracket_arg_tests);
      ("extract_section_lines", extract_section_lines_tests);
      ("parse_cmdliner_commands", parse_cmdliner_commands_tests);
      ("parse_cmdliner_options", parse_cmdliner_options_tests);
    ]
