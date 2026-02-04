(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Tests for Foldable_json module.

    Covers of_string, of_json, render, fold/unfold operations,
    line_count, toggle_fold_at_line, and is_foldable_line. *)

open Alcotest
module FJ = Octez_manager_ui.Foldable_json

let contains s sub =
  try
    ignore (Str.search_forward (Str.regexp_string sub) s 0) ;
    true
  with Not_found -> false

(* ── of_string ──────────────────────────────────────────────── *)

let test_of_string_valid_object () =
  check bool "parses" true (Option.is_some (FJ.of_string {|{"key": "value"}|}))

let test_of_string_valid_array () =
  check bool "parses" true (Option.is_some (FJ.of_string "[1, 2, 3]"))

let test_of_string_empty_object () =
  check bool "parses" true (Option.is_some (FJ.of_string "{}"))

let test_of_string_empty_array () =
  check bool "parses" true (Option.is_some (FJ.of_string "[]"))

let test_of_string_invalid () =
  check bool "None" true (Option.is_none (FJ.of_string "not json {{"))

let test_of_string_scalar () =
  check bool "parses scalar" true (Option.is_some (FJ.of_string "42"))

let test_of_string_null () =
  check bool "parses null" true (Option.is_some (FJ.of_string "null"))

(* ── render ─────────────────────────────────────────────────── *)

let test_render_simple_object () =
  let t = Option.get (FJ.of_string {|{"name": "alice", "age": 30}|}) in
  let rendered = FJ.render t in
  check bool "has name key" true (contains rendered "name") ;
  check bool "has alice value" true (contains rendered "alice")

let test_render_nested_default_folded () =
  let t = Option.get (FJ.of_string {|{"outer": {"inner": 42}}|}) in
  let rendered = FJ.render t in
  (* Inner object is folded by default *)
  check bool "has fold indicator" true (contains rendered "{...}")

let test_render_empty_object () =
  let t = Option.get (FJ.of_string "{}") in
  let rendered = FJ.render t in
  check bool "has braces" true (contains rendered "{}")

let test_render_empty_array () =
  let t = Option.get (FJ.of_string "[]") in
  let rendered = FJ.render t in
  check bool "has brackets" true (contains rendered "[]")

let test_render_scalar () =
  let t = Option.get (FJ.of_string "42") in
  let rendered = FJ.render t in
  check bool "has number" true (contains rendered "42")

(* ── raw ────────────────────────────────────────────────────── *)

let test_raw_returns_json () =
  let t = Option.get (FJ.of_string {|{"x": 1}|}) in
  let raw = FJ.raw t in
  check bool "contains x" true (contains raw "x") ;
  check bool "contains 1" true (contains raw "1")

(* ── unfold_all / fold_all ──────────────────────────────────── *)

let test_unfold_all () =
  let t = Option.get (FJ.of_string {|{"a": {"b": {"c": 1}}}|}) in
  let t = FJ.unfold_all t in
  let rendered = FJ.render t in
  (* After unfolding all, no fold indicators should remain *)
  check bool "no object fold" false (contains rendered "{...}") ;
  check bool "has value" true (contains rendered "1")

let test_fold_all () =
  let t = Option.get (FJ.of_string {|{"a": 1, "b": {"c": 2}}|}) in
  let t = FJ.unfold_all t in
  let t = FJ.fold_all t in
  let rendered = FJ.render t in
  (* Root is unfolded, but nested object is folded *)
  check bool "nested folded" true (contains rendered "{...}")

let test_fold_all_array () =
  let t = Option.get (FJ.of_string {|{"items": [1, 2, 3]}|}) in
  let t = FJ.unfold_all t in
  let t = FJ.fold_all t in
  let rendered = FJ.render t in
  check bool "array folded" true (contains rendered "[...]")

(* ── line_count ─────────────────────────────────────────────── *)

let test_line_count_scalar () =
  let t = Option.get (FJ.of_string "42") in
  let count = FJ.line_count t in
  check bool "at least 1 line" true (count >= 1)

let test_line_count_increases_on_unfold () =
  let t = Option.get (FJ.of_string {|{"x": [1, 2, 3, 4, 5]}|}) in
  let folded_count = FJ.line_count t in
  let t = FJ.unfold_all t in
  let unfolded_count = FJ.line_count t in
  check bool "more lines" true (unfolded_count > folded_count)

(* ── toggle_fold_at_line ────────────────────────────────────── *)

let test_toggle_fold () =
  let t = Option.get (FJ.of_string {|{"items": [1, 2, 3]}|}) in
  let rendered_before = FJ.render t in
  (* Find the line with the fold indicator *)
  let lines = String.split_on_char '\n' rendered_before in
  let fold_line =
    List.mapi (fun i l -> (i, l)) lines
    |> List.find_opt (fun (_, l) -> contains l "[...]")
  in
  match fold_line with
  | Some (i, _) ->
      let t = FJ.toggle_fold_at_line t ~line:i in
      let rendered_after = FJ.render t in
      check
        bool
        "content changed"
        true
        (String.length rendered_after > String.length rendered_before)
  | None ->
      (* Array might be folded on a different indicator *)
      check bool "has fold" true (contains rendered_before "[...]")

let test_toggle_fold_noop () =
  (* Toggle on a non-foldable line should be identity *)
  let t = Option.get (FJ.of_string {|{"a": 1}|}) in
  let rendered_before = FJ.render t in
  let t = FJ.toggle_fold_at_line t ~line:999 in
  let rendered_after = FJ.render t in
  check string "unchanged" rendered_before rendered_after

(* ── is_foldable_line ───────────────────────────────────────── *)

let test_is_foldable_line_root () =
  let t = Option.get (FJ.of_string {|{"a": {"b": 1}}|}) in
  ignore (FJ.render t) ;
  (* Line 0 should be foldable (root object) *)
  check bool "root foldable" true (FJ.is_foldable_line t ~line:0)

let test_is_foldable_line_out_of_range () =
  let t = Option.get (FJ.of_string {|{"a": 1}|}) in
  ignore (FJ.render t) ;
  check bool "out of range" false (FJ.is_foldable_line t ~line:999)

(* ── Test suite ──────────────────────────────────────────────── *)

let () =
  Alcotest.run
    "Foldable_json"
    [
      ( "of_string",
        [
          test_case "valid object" `Quick test_of_string_valid_object;
          test_case "valid array" `Quick test_of_string_valid_array;
          test_case "empty object" `Quick test_of_string_empty_object;
          test_case "empty array" `Quick test_of_string_empty_array;
          test_case "invalid" `Quick test_of_string_invalid;
          test_case "scalar" `Quick test_of_string_scalar;
          test_case "null" `Quick test_of_string_null;
        ] );
      ( "render",
        [
          test_case "simple object" `Quick test_render_simple_object;
          test_case "nested folded" `Quick test_render_nested_default_folded;
          test_case "empty object" `Quick test_render_empty_object;
          test_case "empty array" `Quick test_render_empty_array;
          test_case "scalar" `Quick test_render_scalar;
        ] );
      ("raw", [test_case "returns json" `Quick test_raw_returns_json]);
      ( "fold/unfold",
        [
          test_case "unfold all" `Quick test_unfold_all;
          test_case "fold all" `Quick test_fold_all;
          test_case "fold all array" `Quick test_fold_all_array;
        ] );
      ( "line_count",
        [
          test_case "scalar" `Quick test_line_count_scalar;
          test_case
            "increases on unfold"
            `Quick
            test_line_count_increases_on_unfold;
        ] );
      ( "toggle_fold_at_line",
        [
          test_case "toggle" `Quick test_toggle_fold;
          test_case "noop" `Quick test_toggle_fold_noop;
        ] );
      ( "is_foldable_line",
        [
          test_case "root" `Quick test_is_foldable_line_root;
          test_case "out of range" `Quick test_is_foldable_line_out_of_range;
        ] );
    ]
