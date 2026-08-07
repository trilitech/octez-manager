(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Property-based tests for Env_file_parser module.

    Properties tested:
    - No-crash: parse_string/expand_vars never raise on random input
    - Comment/blank invariant: lines starting with '#' or blank never produce pairs
    - expand_vars without env is identity for strings without '$'
    - expand_vars termination: always returns on arbitrary input
*)

open Octez_manager_lib

(* ============================================================ *)
(* No-crash properties *)
(* ============================================================ *)

let prop_parse_string_no_crash =
  QCheck.Test.make
    ~name:"parse_string never crashes on random input"
    ~count:300
    QCheck.string
    (fun s ->
      let _result = Env_file_parser.parse_string s in
      true)

let prop_expand_vars_no_crash =
  QCheck.Test.make
    ~name:"expand_vars never crashes on random input"
    ~count:300
    QCheck.(pair (list (pair string string)) string)
    (fun (env, s) ->
      let _result = Env_file_parser.expand_vars ~env s in
      true)

(* ============================================================ *)
(* Invariant properties *)
(* ============================================================ *)

let prop_comment_lines_skipped =
  QCheck.Test.make
    ~name:"lines starting with # never produce key-value pairs"
    ~count:300
    QCheck.string
    (fun s ->
      (* Remove newlines to test a single comment line *)
      let single_line = String.concat "" (String.split_on_char '\n' s) in
      let line = "# " ^ single_line in
      let result = Env_file_parser.parse_string line in
      List.length result = 0)

let prop_blank_lines_skipped =
  QCheck.Test.make
    ~name:"blank lines never produce key-value pairs"
    ~count:100
    QCheck.(int_range 0 10)
    (fun n ->
      let blanks = String.make n ' ' in
      let result = Env_file_parser.parse_string blanks in
      List.length result = 0)

let prop_expand_vars_no_dollar_identity =
  QCheck.Test.make
    ~name:"expand_vars is identity when input has no '$'"
    ~count:300
    QCheck.string
    (fun s ->
      if not (String.contains s '$') then
        let result = Env_file_parser.expand_vars ~env:[] s in
        String.equal result s
      else true)

let prop_expand_vars_known_var_replaces =
  QCheck.Test.make
    ~name:"expand_vars replaces known ${VAR} in output"
    ~count:300
    QCheck.(pair string string)
    (fun (key, value) ->
      (* Only test with valid variable names *)
      let is_valid_var_char c =
        (c >= 'A' && c <= 'Z')
        || (c >= 'a' && c <= 'z')
        || (c >= '0' && c <= '9')
        || c = '_'
      in
      if
        String.length key > 0
        && String.for_all is_valid_var_char key
        && (not (String.contains value '$'))
        && not (String.contains value '}')
      then
        let env = [(key, value)] in
        let input = "${" ^ key ^ "}" in
        let result = Env_file_parser.expand_vars ~env input in
        String.equal result value
      else true)

let prop_parse_string_line_count =
  QCheck.Test.make
    ~name:
      "parse_string returns at most as many pairs as non-comment non-blank \
       lines"
    ~count:300
    QCheck.string
    (fun s ->
      let result = Env_file_parser.parse_string s in
      let lines = String.split_on_char '\n' s in
      let content_lines =
        List.filter
          (fun line ->
            let t = String.trim line in
            t <> "" && not (String.starts_with ~prefix:"#" t))
          lines
      in
      List.length result <= List.length content_lines)

(* ============================================================ *)
(* Test Suite *)
(* ============================================================ *)

let props =
  List.map
    QCheck_alcotest.to_alcotest
    [
      prop_parse_string_no_crash;
      prop_expand_vars_no_crash;
      prop_comment_lines_skipped;
      prop_blank_lines_skipped;
      prop_expand_vars_no_dollar_identity;
      prop_expand_vars_known_var_replaces;
      prop_parse_string_line_count;
    ]

let () = Alcotest.run "Env_file_parser_props" [("properties", props)]
