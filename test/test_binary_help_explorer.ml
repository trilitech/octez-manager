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
    ]
