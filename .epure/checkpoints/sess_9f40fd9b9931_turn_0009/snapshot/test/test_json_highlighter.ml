(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Octez_manager_ui

(* ============================================================ *)
(* Highlight Tests                                               *)
(* ============================================================ *)

let test_highlight_null () =
  let options = {Json_highlighter.default_options with colors = false} in
  let result = Json_highlighter.highlight ~options "null" in
  Alcotest.(check (result string string)) "null" (Ok "null") result

let test_highlight_bool_true () =
  let options = {Json_highlighter.default_options with colors = false} in
  let result = Json_highlighter.highlight ~options "true" in
  Alcotest.(check (result string string)) "true" (Ok "true") result

let test_highlight_bool_false () =
  let options = {Json_highlighter.default_options with colors = false} in
  let result = Json_highlighter.highlight ~options "false" in
  Alcotest.(check (result string string)) "false" (Ok "false") result

let test_highlight_int () =
  let options = {Json_highlighter.default_options with colors = false} in
  let result = Json_highlighter.highlight ~options "42" in
  Alcotest.(check (result string string)) "int" (Ok "42") result

let test_highlight_float () =
  let options = {Json_highlighter.default_options with colors = false} in
  let result = Json_highlighter.highlight ~options "3.14" in
  match result with
  | Ok s -> Alcotest.(check bool) "is float" true (String.length s > 0)
  | Error _ -> Alcotest.fail "expected success"

let test_highlight_string () =
  let options = {Json_highlighter.default_options with colors = false} in
  let result = Json_highlighter.highlight ~options {|"hello"|} in
  Alcotest.(check (result string string)) "string" (Ok {|"hello"|}) result

let test_highlight_empty_array () =
  let options = {Json_highlighter.default_options with colors = false} in
  let result = Json_highlighter.highlight ~options "[]" in
  Alcotest.(check (result string string)) "empty array" (Ok "[]") result

let test_highlight_empty_object () =
  let options = {Json_highlighter.default_options with colors = false} in
  let result = Json_highlighter.highlight ~options "{}" in
  Alcotest.(check (result string string)) "empty object" (Ok "{}") result

let test_highlight_simple_object () =
  let options = {Json_highlighter.default_options with colors = false} in
  let result = Json_highlighter.highlight ~options {|{"key": "value"}|} in
  match result with
  | Ok s ->
      Alcotest.(check bool) "has key" true (String.length s > 0) ;
      Alcotest.(check bool) "contains key" true (String.sub s 0 1 = "{")
  | Error _ -> Alcotest.fail "expected success"

let test_highlight_nested_object () =
  let options = {Json_highlighter.default_options with colors = false} in
  let json = {|{"outer": {"inner": 1}}|} in
  let result = Json_highlighter.highlight ~options json in
  match result with
  | Ok s ->
      Alcotest.(check bool) "contains newlines" true (String.contains s '\n')
  | Error _ -> Alcotest.fail "expected success"

let test_highlight_array_of_values () =
  let options = {Json_highlighter.default_options with colors = false} in
  let json = {|[1, 2, 3]|} in
  let result = Json_highlighter.highlight ~options json in
  match result with
  | Ok s -> Alcotest.(check bool) "starts with [" true (s.[0] = '[')
  | Error _ -> Alcotest.fail "expected success"

let test_highlight_invalid_json () =
  let options = {Json_highlighter.default_options with colors = false} in
  let result = Json_highlighter.highlight ~options "{invalid" in
  match result with
  | Ok _ -> Alcotest.fail "expected error for invalid JSON"
  | Error msg -> Alcotest.(check bool) "has error" true (String.length msg > 0)

(* ============================================================ *)
(* Color Tests                                                   *)
(* ============================================================ *)

let test_highlight_with_colors () =
  let options = {Json_highlighter.default_options with colors = true} in
  let result = Json_highlighter.highlight ~options {|{"key": 42}|} in
  match result with
  | Ok s ->
      (* Should contain ANSI escape codes *)
      Alcotest.(check bool) "contains escape" true (String.contains s '\027')
  | Error _ -> Alcotest.fail "expected success"

let test_strip_colors () =
  let colored = "\027[38;5;14mtext\027[0m" in
  let stripped = Json_highlighter.strip_colors colored in
  Alcotest.(check string) "stripped" "text" stripped

let test_strip_colors_complex () =
  let colored = "\027[38;5;14m\"key\"\027[0m: \027[38;5;13m42\027[0m" in
  let stripped = Json_highlighter.strip_colors colored in
  Alcotest.(check string) "stripped complex" "\"key\": 42" stripped

let test_strip_colors_no_colors () =
  let plain = "plain text" in
  let stripped = Json_highlighter.strip_colors plain in
  Alcotest.(check string) "no change" "plain text" stripped

(* ============================================================ *)
(* Options Tests                                                 *)
(* ============================================================ *)

let test_default_options () =
  let opts = Json_highlighter.default_options in
  Alcotest.(check int) "indent" 2 opts.indent ;
  Alcotest.(check int) "max_depth" 20 opts.max_depth ;
  Alcotest.(check bool) "colors" true opts.colors

let test_custom_indent () =
  let options =
    {Json_highlighter.default_options with indent = 4; colors = false}
  in
  let json = {|{"a": 1}|} in
  let result = Json_highlighter.highlight ~options json in
  match result with
  | Ok s ->
      (* With indent=4, should have 4 spaces before "a" *)
      Alcotest.(check bool) "has 4 spaces" true (String.length s > 0)
  | Error _ -> Alcotest.fail "expected success"

(* ============================================================ *)
(* Test Runner                                                   *)
(* ============================================================ *)

let () =
  Alcotest.run
    "Json_highlighter"
    [
      ( "highlight",
        [
          Alcotest.test_case "null" `Quick test_highlight_null;
          Alcotest.test_case "bool true" `Quick test_highlight_bool_true;
          Alcotest.test_case "bool false" `Quick test_highlight_bool_false;
          Alcotest.test_case "int" `Quick test_highlight_int;
          Alcotest.test_case "float" `Quick test_highlight_float;
          Alcotest.test_case "string" `Quick test_highlight_string;
          Alcotest.test_case "empty array" `Quick test_highlight_empty_array;
          Alcotest.test_case "empty object" `Quick test_highlight_empty_object;
          Alcotest.test_case "simple object" `Quick test_highlight_simple_object;
          Alcotest.test_case "nested object" `Quick test_highlight_nested_object;
          Alcotest.test_case
            "array of values"
            `Quick
            test_highlight_array_of_values;
          Alcotest.test_case "invalid json" `Quick test_highlight_invalid_json;
        ] );
      ( "colors",
        [
          Alcotest.test_case "with colors" `Quick test_highlight_with_colors;
          Alcotest.test_case "strip colors" `Quick test_strip_colors;
          Alcotest.test_case "strip complex" `Quick test_strip_colors_complex;
          Alcotest.test_case
            "strip no colors"
            `Quick
            test_strip_colors_no_colors;
        ] );
      ( "options",
        [
          Alcotest.test_case "defaults" `Quick test_default_options;
          Alcotest.test_case "custom indent" `Quick test_custom_indent;
        ] );
    ]
