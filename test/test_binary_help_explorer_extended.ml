(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Extended tests for Binary_help_explorer pure functions.

    Covers render_value, truncate, wrap_text, option_label, format_tokens,
    option_matches_flag, name_matches_excluded, is_excluded_option,
    option_hint_markdown, init_rows_from_args, and excluded option lists.

    Includes PBT for truncate, wrap_text, and parse_initial_args. *)

open Alcotest
module BHE = Octez_manager_ui.Binary_help_explorer
module BHE_ft = BHE.For_tests
module HP = Octez_manager_lib.Help_parser

(* ── Helpers ───────────────────────────────────────────────── *)

let make_opt ?(arg = None) ?(doc = "") ?(kind = HP.Toggle) names =
  {HP.names; arg; doc; kind}

let make_row ?(value = None) ?(selected = false) opt =
  {BHE.opt; value; selected}

(* ── render_value ──────────────────────────────────────────── *)

let test_render_value_none () = check string "none" "" (BHE.render_value None)

let test_render_value_some () =
  check string "some" "hello" (BHE.render_value (Some "hello"))

let test_render_value_empty () =
  check string "empty" "" (BHE.render_value (Some ""))

(* ── truncate ──────────────────────────────────────────────── *)

let test_truncate_short () =
  check string "short" "abc" (BHE.truncate ~max_len:10 "abc")

let test_truncate_exact () =
  check string "exact" "abcde" (BHE.truncate ~max_len:5 "abcde")

let test_truncate_long () =
  let result = BHE.truncate ~max_len:5 "abcdef" in
  (* "…" is a 3-byte UTF-8 char; truncate takes max_len-1 ASCII chars + "…" *)
  check string "truncated" "abcd\xe2\x80\xa6" result

let test_truncate_empty () =
  check string "empty" "" (BHE.truncate ~max_len:5 "")

(* ── option_label ──────────────────────────────────────────── *)

let test_option_label_single () =
  let opt = make_opt ["--verbose"] in
  check string "single" "--verbose" (BHE.option_label opt)

let test_option_label_multiple () =
  (* display_names filters to long names only when present *)
  let opt = make_opt ["--verbose"; "-v"] in
  check string "multiple" "--verbose" (BHE.option_label opt)

let test_option_label_empty () =
  let opt = make_opt [] in
  check string "empty" "" (BHE.option_label opt)

(* ── wrap_text ─────────────────────────────────────────────── *)

let test_wrap_text_short () =
  let result = BHE_ft.wrap_text ~width:80 "short line" in
  check (list string) "no wrap" ["short line"] result

let test_wrap_text_long () =
  let input = "this is a line that is longer than the width" in
  let result = BHE_ft.wrap_text ~width:20 input in
  check bool "wrapped" true (List.length result > 1) ;
  List.iter
    (fun line -> check bool "within width" true (String.length line <= 20))
    result

let test_wrap_text_preserves_newlines () =
  let result = BHE_ft.wrap_text ~width:80 "line one\nline two" in
  check (list string) "two lines" ["line one"; "line two"] result

let test_wrap_text_empty () =
  let result = BHE_ft.wrap_text ~width:80 "" in
  check (list string) "empty" [""] result

(* ── format_tokens ─────────────────────────────────────────── *)

let test_format_tokens_none_selected () =
  let rows =
    [make_row (make_opt ["--verbose"]); make_row (make_opt ["--force"])]
  in
  check (list string) "empty" [] (BHE.format_tokens rows)

let test_format_tokens_toggle () =
  let rows = [make_row ~selected:true (make_opt ["--verbose"])] in
  check (list string) "toggle" ["--verbose"] (BHE.format_tokens rows)

let test_format_tokens_with_value () =
  let rows =
    [
      make_row
        ~selected:true
        ~value:(Some "8732")
        (make_opt ~kind:(HP.Value HP.Port) ["--port"]);
    ]
  in
  check (list string) "with value" ["--port"; "8732"] (BHE.format_tokens rows)

let test_format_tokens_mixed () =
  let rows =
    [
      make_row ~selected:true (make_opt ["--verbose"]);
      make_row (make_opt ["--quiet"]);
      make_row
        ~selected:true
        ~value:(Some "/tmp")
        (make_opt ~kind:(HP.Value HP.Dir) ["--data-dir"]);
    ]
  in
  check
    (list string)
    "mixed"
    ["--verbose"; "--data-dir"; "/tmp"]
    (BHE.format_tokens rows)

(* ── option_matches_flag ───────────────────────────────────── *)

let test_option_matches_flag_exact () =
  let opt = make_opt ["--verbose"; "-v"] in
  check bool "matches --verbose" true (BHE.option_matches_flag opt "--verbose") ;
  check bool "matches -v" true (BHE.option_matches_flag opt "-v") ;
  check bool "no match" false (BHE.option_matches_flag opt "--force")

(* ── name_matches_excluded ─────────────────────────────────── *)

let test_name_matches_excluded_exact () =
  check bool "exact" true (BHE.name_matches_excluded "--help" "--help")

let test_name_matches_excluded_prefix () =
  check bool "prefix" true (BHE.name_matches_excluded "--data-dir" "--data-dir")

let test_name_matches_excluded_longer () =
  check
    bool
    "longer name"
    true
    (BHE.name_matches_excluded "--data-dir=/tmp" "--data-dir")

let test_name_matches_excluded_no_match () =
  check bool "no match" false (BHE.name_matches_excluded "--verbose" "--help")

let test_name_matches_excluded_shorter () =
  check bool "shorter" false (BHE.name_matches_excluded "--hel" "--help")

(* ── is_excluded_option ────────────────────────────────────── *)

let test_is_excluded_yes () =
  let opt = make_opt ["--help"; "-h"] in
  check
    bool
    "excluded"
    true
    (BHE.is_excluded_option opt ~excluded:["--help"; "--version"])

let test_is_excluded_no () =
  let opt = make_opt ["--verbose"; "-v"] in
  check
    bool
    "not excluded"
    false
    (BHE.is_excluded_option opt ~excluded:["--help"; "--version"])

let test_is_excluded_prefix_match () =
  let opt = make_opt ["--data-dir"] in
  check
    bool
    "prefix excluded"
    true
    (BHE.is_excluded_option opt ~excluded:["--data-dir"])

(* ── init_rows_from_args ───────────────────────────────────── *)

let test_init_rows_empty_args () =
  let options = [make_opt ["--verbose"]; make_opt ["--force"]] in
  let rows = BHE.init_rows_from_args options "" in
  check int "two rows" 2 (List.length rows) ;
  List.iter (fun r -> check bool "not selected" false r.BHE.selected) rows

let test_init_rows_with_matching_flag () =
  let options =
    [make_opt ["--verbose"]; make_opt ~kind:(HP.Value HP.Port) ["--port"]]
  in
  let rows = BHE.init_rows_from_args options "--verbose --port 8732" in
  check int "two rows" 2 (List.length rows) ;
  let verbose_row = List.nth rows 0 in
  check bool "verbose selected" true verbose_row.BHE.selected ;
  let port_row = List.nth rows 1 in
  check bool "port selected" true port_row.BHE.selected ;
  check (option string) "port value" (Some "8732") port_row.BHE.value

let test_init_rows_unmatched_flag () =
  let options = [make_opt ["--verbose"]] in
  let rows = BHE.init_rows_from_args options "--force" in
  check int "one row" 1 (List.length rows) ;
  check bool "not selected" false (List.nth rows 0).BHE.selected

(* ── option_hint_markdown ──────────────────────────────────── *)

let test_hint_markdown_basic () =
  let opt = make_opt ~doc:"Enable verbose output" ["--verbose"; "-v"] in
  let row = make_row opt in
  let short, long = BHE.option_hint_markdown row in
  check bool "has short" true (Option.is_some short) ;
  check bool "has long" true (Option.is_some long) ;
  let short_text = Option.get short in
  check
    bool
    "short contains flag"
    true
    (try
       ignore (Str.search_forward (Str.regexp_string "--verbose") short_text 0) ;
       true
     with Not_found -> false)

let test_hint_markdown_selected () =
  let opt = make_opt ["--port"] ~kind:(HP.Value HP.Port) in
  let row = make_row ~selected:true ~value:(Some "8732") opt in
  let _short, long = BHE.option_hint_markdown row in
  check bool "has long" true (Option.is_some long) ;
  let long_text = Option.get long in
  check
    bool
    "long contains Selected"
    true
    (try
       ignore (Str.search_forward (Str.regexp_string "Selected") long_text 0) ;
       true
     with Not_found -> false)

let test_hint_markdown_no_doc () =
  let opt = make_opt ~doc:"" ["--quiet"] in
  let row = make_row opt in
  let short, _long = BHE.option_hint_markdown row in
  check bool "no short when no doc" true (Option.is_none short)

let test_hint_markdown_with_arg () =
  let opt =
    make_opt
      ~arg:(Some "PORT")
      ~doc:"Listen port"
      ~kind:(HP.Value HP.Port)
      ["--port"]
  in
  let row = make_row opt in
  let _short, long = BHE.option_hint_markdown row in
  check bool "has long" true (Option.is_some long) ;
  let long_text = Option.get long in
  check
    bool
    "long contains PORT"
    true
    (try
       ignore (Str.search_forward (Str.regexp_string "PORT") long_text 0) ;
       true
     with Not_found -> false)

(* ── excluded option lists ─────────────────────────────────── *)

let test_excluded_node_options_has_help () =
  check bool "has --help" true (List.mem "--help" BHE.excluded_node_options)

let test_excluded_baker_options_has_help () =
  check bool "has --help" true (List.mem "--help" BHE.excluded_baker_options)

let test_excluded_accuser_options_has_help () =
  check bool "has --help" true (List.mem "--help" BHE.excluded_accuser_options)

let test_excluded_dal_options_has_help () =
  check bool "has --help" true (List.mem "--help" BHE.excluded_dal_options)

(* ── PBT ───────────────────────────────────────────────────── *)

let prop_truncate_length =
  QCheck.Test.make
    ~name:"truncate output <= max_len"
    ~count:300
    QCheck.(pair (int_range 1 100) string)
    (fun (max_len, s) ->
      let result = BHE.truncate ~max_len s in
      if String.length s <= max_len then result = s
      else
        (* Truncated: max_len-1 ASCII prefix + 3-byte UTF-8 ellipsis *)
        String.length result = max_len - 1 + 3
        && String.ends_with ~suffix:"\xe2\x80\xa6" result)

let prop_truncate_short_identity =
  QCheck.Test.make
    ~name:"truncate preserves short strings"
    ~count:300
    QCheck.(pair (int_range 1 200) (string_size (Gen.int_range 0 10)))
    (fun (max_len, s) ->
      if String.length s <= max_len then BHE.truncate ~max_len s = s else true)

let prop_wrap_text_no_crash =
  QCheck.Test.make
    ~name:"wrap_text never crashes"
    ~count:300
    QCheck.(pair (int_range 1 200) string)
    (fun (width, s) ->
      ignore (BHE_ft.wrap_text ~width s) ;
      true)

let prop_wrap_text_preserves_content =
  QCheck.Test.make
    ~name:"wrap_text preserves all non-space content"
    ~count:200
    QCheck.(pair (int_range 5 80) (string_size (Gen.int_range 0 200)))
    (fun (width, s) ->
      let lines = BHE_ft.wrap_text ~width s in
      let rejoined = String.concat " " lines in
      (* All non-whitespace chars from original should appear in result *)
      let orig_chars =
        String.to_seq s
        |> Seq.filter (fun c -> c <> ' ' && c <> '\n')
        |> String.of_seq
      in
      let result_chars =
        String.to_seq rejoined
        |> Seq.filter (fun c -> c <> ' ' && c <> '\n')
        |> String.of_seq
      in
      String.equal orig_chars result_chars)

let prop_parse_initial_args_no_crash =
  QCheck.Test.make
    ~name:"parse_initial_args never crashes"
    ~count:500
    QCheck.string
    (fun s ->
      ignore (BHE.For_tests.parse_initial_args s) ;
      true)

let prop_parse_initial_args_flags_start_with_dash =
  QCheck.Test.make
    ~name:"parse_initial_args flags start with -"
    ~count:300
    QCheck.string
    (fun s ->
      let result = BHE.For_tests.parse_initial_args s in
      List.for_all
        (fun (flag, _) -> String.length flag > 0 && flag.[0] = '-')
        result)

let prop_name_matches_excluded_reflexive =
  QCheck.Test.make
    ~name:"name_matches_excluded is reflexive"
    ~count:300
    QCheck.string
    (fun s -> BHE.name_matches_excluded s s)

(* ── Test Suite ────────────────────────────────────────────── *)

let () =
  Alcotest.run
    "Binary_help_explorer_extended"
    [
      ( "render_value",
        [
          test_case "none" `Quick test_render_value_none;
          test_case "some" `Quick test_render_value_some;
          test_case "empty" `Quick test_render_value_empty;
        ] );
      ( "truncate",
        [
          test_case "short" `Quick test_truncate_short;
          test_case "exact" `Quick test_truncate_exact;
          test_case "long" `Quick test_truncate_long;
          test_case "empty" `Quick test_truncate_empty;
        ] );
      ( "option_label",
        [
          test_case "single" `Quick test_option_label_single;
          test_case "multiple" `Quick test_option_label_multiple;
          test_case "empty" `Quick test_option_label_empty;
        ] );
      ( "wrap_text",
        [
          test_case "short" `Quick test_wrap_text_short;
          test_case "long" `Quick test_wrap_text_long;
          test_case
            "preserves newlines"
            `Quick
            test_wrap_text_preserves_newlines;
          test_case "empty" `Quick test_wrap_text_empty;
        ] );
      ( "format_tokens",
        [
          test_case "none selected" `Quick test_format_tokens_none_selected;
          test_case "toggle" `Quick test_format_tokens_toggle;
          test_case "with value" `Quick test_format_tokens_with_value;
          test_case "mixed" `Quick test_format_tokens_mixed;
        ] );
      ( "option_matches_flag",
        [test_case "exact" `Quick test_option_matches_flag_exact] );
      ( "name_matches_excluded",
        [
          test_case "exact" `Quick test_name_matches_excluded_exact;
          test_case "prefix" `Quick test_name_matches_excluded_prefix;
          test_case "longer" `Quick test_name_matches_excluded_longer;
          test_case "no match" `Quick test_name_matches_excluded_no_match;
          test_case "shorter" `Quick test_name_matches_excluded_shorter;
        ] );
      ( "is_excluded_option",
        [
          test_case "excluded" `Quick test_is_excluded_yes;
          test_case "not excluded" `Quick test_is_excluded_no;
          test_case "prefix match" `Quick test_is_excluded_prefix_match;
        ] );
      ( "init_rows_from_args",
        [
          test_case "empty args" `Quick test_init_rows_empty_args;
          test_case "matching flag" `Quick test_init_rows_with_matching_flag;
          test_case "unmatched flag" `Quick test_init_rows_unmatched_flag;
        ] );
      ( "option_hint_markdown",
        [
          test_case "basic" `Quick test_hint_markdown_basic;
          test_case "selected" `Quick test_hint_markdown_selected;
          test_case "no doc" `Quick test_hint_markdown_no_doc;
          test_case "with arg" `Quick test_hint_markdown_with_arg;
        ] );
      ( "excluded_lists",
        [
          test_case "node has --help" `Quick test_excluded_node_options_has_help;
          test_case
            "baker has --help"
            `Quick
            test_excluded_baker_options_has_help;
          test_case
            "accuser has --help"
            `Quick
            test_excluded_accuser_options_has_help;
          test_case "dal has --help" `Quick test_excluded_dal_options_has_help;
        ] );
      ( "pbt",
        List.map
          QCheck_alcotest.to_alcotest
          [
            prop_truncate_length;
            prop_truncate_short_identity;
            prop_wrap_text_no_crash;
            prop_wrap_text_preserves_content;
            prop_parse_initial_args_no_crash;
            prop_parse_initial_args_flags_start_with_dash;
            prop_name_matches_excluded_reflexive;
          ] );
    ]
