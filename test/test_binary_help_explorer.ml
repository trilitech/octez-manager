(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Tests for Binary_help_explorer module.

    Covers parse_initial_args and arg_kind_to_string. *)

open Alcotest
module BHE = Octez_manager_ui.Binary_help_explorer

(* ── parse_initial_args ──────────────────────────────────────── *)

let test_parse_empty () =
  check
    (list (pair string (option string)))
    "empty"
    []
    (BHE.For_tests.parse_initial_args "")

let test_parse_single_flag () =
  check
    (list (pair string (option string)))
    "single"
    [("--verbose", None)]
    (BHE.For_tests.parse_initial_args "--verbose")

let test_parse_flag_with_eq_value () =
  check
    (list (pair string (option string)))
    "eq"
    [("--rpc-addr", Some "127.0.0.1:8732")]
    (BHE.For_tests.parse_initial_args "--rpc-addr=127.0.0.1:8732")

let test_parse_flag_with_space_value () =
  check
    (list (pair string (option string)))
    "space"
    [("--rpc-addr", Some "127.0.0.1:8732")]
    (BHE.For_tests.parse_initial_args "--rpc-addr 127.0.0.1:8732")

let test_parse_multiple_flags () =
  let result =
    BHE.For_tests.parse_initial_args
      "--verbose --rpc-addr=localhost:8732 --data-dir /tmp"
  in
  check int "three flags" 3 (List.length result) ;
  check
    (pair string (option string))
    "first"
    ("--verbose", None)
    (List.nth result 0) ;
  check
    (pair string (option string))
    "second"
    ("--rpc-addr", Some "localhost:8732")
    (List.nth result 1) ;
  check
    (pair string (option string))
    "third"
    ("--data-dir", Some "/tmp")
    (List.nth result 2)

let test_parse_short_flag () =
  check
    (list (pair string (option string)))
    "short"
    [("-v", None)]
    (BHE.For_tests.parse_initial_args "-v")

let test_parse_mixed_flags () =
  let result = BHE.For_tests.parse_initial_args "-v --port=8732" in
  check int "two flags" 2 (List.length result) ;
  check (pair string (option string)) "short" ("-v", None) (List.nth result 0) ;
  check
    (pair string (option string))
    "long"
    ("--port", Some "8732")
    (List.nth result 1)

let test_parse_quoted_value () =
  let result =
    BHE.For_tests.parse_initial_args "--data-dir \"/path/with spaces\""
  in
  check int "one flag" 1 (List.length result) ;
  check
    (pair string (option string))
    "quoted"
    ("--data-dir", Some "/path/with spaces")
    (List.nth result 0)

let test_parse_consecutive_flags () =
  let result = BHE.For_tests.parse_initial_args "--verbose --force --dry-run" in
  check int "three flags" 3 (List.length result) ;
  List.iter (fun (_, v) -> check (option string) "no value" None v) result

(* ── arg_kind_to_string ──────────────────────────────────────── *)

let test_kind_toggle () =
  check
    string
    "toggle"
    "toggle"
    (BHE.For_tests.arg_kind_to_string Octez_manager_lib.Help_parser.Toggle)

let test_kind_port () =
  check
    string
    "port"
    "port"
    (BHE.For_tests.arg_kind_to_string
       (Octez_manager_lib.Help_parser.Value Port))

let test_kind_addr () =
  check
    string
    "addr"
    "addr"
    (BHE.For_tests.arg_kind_to_string
       (Octez_manager_lib.Help_parser.Value Addr))

let test_kind_file () =
  check
    string
    "file"
    "file"
    (BHE.For_tests.arg_kind_to_string
       (Octez_manager_lib.Help_parser.Value File))

let test_kind_dir () =
  check
    string
    "dir"
    "dir"
    (BHE.For_tests.arg_kind_to_string (Octez_manager_lib.Help_parser.Value Dir))

let test_kind_number () =
  check
    string
    "number"
    "number"
    (BHE.For_tests.arg_kind_to_string
       (Octez_manager_lib.Help_parser.Value Number))

let test_kind_text () =
  check
    string
    "text"
    "text"
    (BHE.For_tests.arg_kind_to_string
       (Octez_manager_lib.Help_parser.Value Text))

let test_kind_addr_port () =
  check
    string
    "addr_port"
    "addr_port"
    (BHE.For_tests.arg_kind_to_string
       (Octez_manager_lib.Help_parser.Value Addr_port))

(* ── truncate ──────────────────────────────────────────────── *)

let test_truncate_short () =
  check string "short" "hello" (BHE.For_tests.truncate ~max_len:10 "hello")

let test_truncate_exact () =
  check string "exact" "hello" (BHE.For_tests.truncate ~max_len:5 "hello")

let test_truncate_long () =
  let result = BHE.For_tests.truncate ~max_len:5 "hello world" in
  (* The function takes max_len-1 chars and appends "…" (UTF-8: 3 bytes) *)
  check bool "ends with ellipsis" true (String.ends_with ~suffix:"…" result) ;
  check
    bool
    "shorter than original"
    true
    (String.length result < String.length "hello world")

let test_truncate_empty () =
  check string "empty" "" (BHE.For_tests.truncate ~max_len:10 "")

let test_truncate_one () =
  check string "one" "a" (BHE.For_tests.truncate ~max_len:5 "a")

(* ── wrap_text ──────────────────────────────────────────────── *)

let test_wrap_short () =
  let result = BHE.For_tests.wrap_text ~width:80 "hello world" in
  check (list string) "no wrap" ["hello world"] result

let test_wrap_long () =
  let long = String.make 50 'a' ^ " " ^ String.make 50 'b' in
  let result = BHE.For_tests.wrap_text ~width:60 long in
  check bool "wrapped" true (List.length result >= 2)

let test_wrap_newlines () =
  let result = BHE.For_tests.wrap_text ~width:80 "line1\nline2\nline3" in
  check int "three lines" 3 (List.length result)

let test_wrap_empty () =
  let result = BHE.For_tests.wrap_text ~width:80 "" in
  check (list string) "empty" [""] result

(* ── option_label ──────────────────────────────────────────── *)

module HP = Octez_manager_lib.Help_parser

let make_opt ?(names = ["--test"]) ?(arg = None) ?(doc = "") ?(kind = HP.Toggle)
    () : HP.option_entry =
  {names; arg; doc; kind}

let test_option_label_single () =
  let opt = make_opt ~names:["--verbose"] () in
  check string "single" "--verbose" (BHE.For_tests.option_label opt)

let test_option_label_multi () =
  let opt = make_opt ~names:["-v"; "--verbose"] () in
  let label = BHE.For_tests.option_label opt in
  check bool "contains verbose" true (String.length label > 0)

(* ── render_value ──────────────────────────────────────────── *)

let test_render_value_none () =
  check string "none" "" (BHE.For_tests.render_value None)

let test_render_value_some () =
  check string "some" "hello" (BHE.For_tests.render_value (Some "hello"))

(* ── format_tokens ──────────────────────────────────────────── *)

let test_format_tokens_none_selected () =
  let rows = [BHE.For_tests.make_row (make_opt ~names:["--verbose"] ())] in
  check (list string) "empty" [] (BHE.For_tests.format_tokens rows)

let test_format_tokens_selected_toggle () =
  let rows =
    [BHE.For_tests.make_row_selected (make_opt ~names:["--verbose"] ()) None]
  in
  check (list string) "toggle" ["--verbose"] (BHE.For_tests.format_tokens rows)

let test_format_tokens_selected_with_value () =
  let rows =
    [
      BHE.For_tests.make_row_selected
        (make_opt ~names:["--rpc-addr"] ~kind:(HP.Value HP.Addr_port) ())
        (Some "127.0.0.1:8732");
    ]
  in
  check
    (list string)
    "with value"
    ["--rpc-addr"; "127.0.0.1:8732"]
    (BHE.For_tests.format_tokens rows)

let test_format_tokens_mixed () =
  let rows =
    [
      BHE.For_tests.make_row_selected (make_opt ~names:["--verbose"] ()) None;
      BHE.For_tests.make_row (make_opt ~names:["--debug"] ());
      BHE.For_tests.make_row_selected
        (make_opt ~names:["--port"] ~kind:(HP.Value HP.Port) ())
        (Some "8732");
    ]
  in
  check
    (list string)
    "mixed"
    ["--verbose"; "--port"; "8732"]
    (BHE.For_tests.format_tokens rows)

(* ── name_matches_excluded ──────────────────────────────────── *)

let test_name_match_exact () =
  check
    bool
    "exact"
    true
    (BHE.For_tests.name_matches_excluded "--help" "--help")

let test_name_match_prefix () =
  check
    bool
    "prefix"
    true
    (BHE.For_tests.name_matches_excluded "--help=plain" "--help")

let test_name_no_match () =
  check
    bool
    "no match"
    false
    (BHE.For_tests.name_matches_excluded "--verbose" "--help")

let test_name_match_short () =
  check bool "short" false (BHE.For_tests.name_matches_excluded "-h" "--help")

(* ── is_excluded_option ──────────────────────────────────────── *)

let test_excluded_yes () =
  let opt = make_opt ~names:["--help"; "-help"] () in
  check
    bool
    "excluded"
    true
    (BHE.For_tests.is_excluded_option opt ~excluded:["--help"])

let test_excluded_no () =
  let opt = make_opt ~names:["--verbose"] () in
  check
    bool
    "not excluded"
    false
    (BHE.For_tests.is_excluded_option opt ~excluded:["--help"])

let test_excluded_empty_list () =
  let opt = make_opt ~names:["--verbose"] () in
  check
    bool
    "empty excluded"
    false
    (BHE.For_tests.is_excluded_option opt ~excluded:[])

let test_excluded_node_list () =
  let opt = make_opt ~names:["--data-dir"; "-d"] () in
  check
    bool
    "in node excluded"
    true
    (BHE.For_tests.is_excluded_option
       opt
       ~excluded:BHE.For_tests.excluded_node_options)

let test_not_excluded_node_list () =
  let opt = make_opt ~names:["--expected-pow"] () in
  check
    bool
    "not in node excluded"
    false
    (BHE.For_tests.is_excluded_option
       opt
       ~excluded:BHE.For_tests.excluded_node_options)

(* ── PBT: truncate never crashes ──────────────────────────── *)

let test_truncate_no_crash =
  QCheck.Test.make
    ~name:"truncate never crashes"
    ~count:500
    QCheck.(pair (int_range 1 200) string)
    (fun (max_len, s) ->
      let _ = BHE.For_tests.truncate ~max_len s in
      true)

let test_wrap_text_no_crash =
  QCheck.Test.make
    ~name:"wrap_text never crashes"
    ~count:500
    QCheck.(pair (int_range 1 200) string)
    (fun (width, s) ->
      let _ = BHE.For_tests.wrap_text ~width s in
      true)

(* ── Test suite ──────────────────────────────────────────────── *)

let () =
  Alcotest.run
    "Binary_help_explorer"
    [
      ( "parse_initial_args",
        [
          test_case "empty" `Quick test_parse_empty;
          test_case "single flag" `Quick test_parse_single_flag;
          test_case "flag=value" `Quick test_parse_flag_with_eq_value;
          test_case "flag value" `Quick test_parse_flag_with_space_value;
          test_case "multiple flags" `Quick test_parse_multiple_flags;
          test_case "short flag" `Quick test_parse_short_flag;
          test_case "mixed flags" `Quick test_parse_mixed_flags;
          test_case "quoted value" `Quick test_parse_quoted_value;
          test_case "consecutive flags" `Quick test_parse_consecutive_flags;
        ] );
      ( "arg_kind_to_string",
        [
          test_case "toggle" `Quick test_kind_toggle;
          test_case "port" `Quick test_kind_port;
          test_case "addr" `Quick test_kind_addr;
          test_case "file" `Quick test_kind_file;
          test_case "dir" `Quick test_kind_dir;
          test_case "number" `Quick test_kind_number;
          test_case "text" `Quick test_kind_text;
          test_case "addr_port" `Quick test_kind_addr_port;
        ] );
      ( "truncate",
        [
          test_case "short" `Quick test_truncate_short;
          test_case "exact" `Quick test_truncate_exact;
          test_case "long" `Quick test_truncate_long;
          test_case "empty" `Quick test_truncate_empty;
          test_case "one char" `Quick test_truncate_one;
        ] );
      ( "wrap_text",
        [
          test_case "short line" `Quick test_wrap_short;
          test_case "long line" `Quick test_wrap_long;
          test_case "with newlines" `Quick test_wrap_newlines;
          test_case "empty" `Quick test_wrap_empty;
        ] );
      ( "option_label",
        [
          test_case "single name" `Quick test_option_label_single;
          test_case "multi name" `Quick test_option_label_multi;
        ] );
      ( "render_value",
        [
          test_case "none" `Quick test_render_value_none;
          test_case "some" `Quick test_render_value_some;
        ] );
      ( "format_tokens",
        [
          test_case "none selected" `Quick test_format_tokens_none_selected;
          test_case "selected toggle" `Quick test_format_tokens_selected_toggle;
          test_case
            "selected with value"
            `Quick
            test_format_tokens_selected_with_value;
          test_case "mixed" `Quick test_format_tokens_mixed;
        ] );
      ( "name_matches_excluded",
        [
          test_case "exact match" `Quick test_name_match_exact;
          test_case "prefix match" `Quick test_name_match_prefix;
          test_case "no match" `Quick test_name_no_match;
          test_case "short vs long" `Quick test_name_match_short;
        ] );
      ( "is_excluded_option",
        [
          test_case "excluded" `Quick test_excluded_yes;
          test_case "not excluded" `Quick test_excluded_no;
          test_case "empty excluded list" `Quick test_excluded_empty_list;
          test_case "in node excluded" `Quick test_excluded_node_list;
          test_case "not in node excluded" `Quick test_not_excluded_node_list;
        ] );
      ( "PBT",
        List.map
          QCheck_alcotest.to_alcotest
          [test_truncate_no_crash; test_wrap_text_no_crash] );
    ]
