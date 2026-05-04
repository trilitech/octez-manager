(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Octez_manager_ui
module State = Rpc_browser_state
module Render = Rpc_browser_render_result

(* ============================================================ *)
(* Pager Header Tests                                           *)
(* ============================================================ *)

let make_pager_slot ?(id = 0) ?(request = "") ?(body = "") ?(raw_body = "") () =
  State.create_empty_pager id |> fun slot ->
  {slot with State.request; body; raw_body}

let test_render_pager_header_focused () =
  let slot = make_pager_slot ~id:0 ~request:"/chains/main/blocks/head" () in
  let result =
    Render.render_pager_header ~slot ~is_focused:true ~is_target:false
  in
  Alcotest.(check bool) "has content" true (String.length result > 0)

let test_render_pager_header_unfocused () =
  let slot = make_pager_slot ~id:1 ~request:"/version" () in
  let result =
    Render.render_pager_header ~slot ~is_focused:false ~is_target:false
  in
  Alcotest.(check bool) "has content" true (String.length result > 0)

let test_render_pager_header_target () =
  let slot = make_pager_slot ~id:1 ~request:"/version" () in
  let result =
    Render.render_pager_header ~slot ~is_focused:false ~is_target:true
  in
  Alcotest.(check bool) "has content" true (String.length result > 0)

let test_render_pager_header_empty () =
  let slot = make_pager_slot ~id:2 () in
  let result =
    Render.render_pager_header ~slot ~is_focused:true ~is_target:false
  in
  Alcotest.(check bool) "has content" true (String.length result > 0)

(* ============================================================ *)
(* Loading and Error Tests                                       *)
(* ============================================================ *)

let test_render_loading () =
  let result = Render.render_loading () in
  Alcotest.(check bool) "has content" true (String.length result > 0)

let test_render_error () =
  let result = Render.render_error "Connection failed" in
  Alcotest.(check bool) "has content" true (String.length result > 0)

(* ============================================================ *)
(* Help Line Tests                                               *)
(* ============================================================ *)

let test_render_help_single () =
  let result = Render.render_help ~num_pagers:1 in
  Alcotest.(check bool) "has content" true (String.length result > 0)

let test_render_help_multiple () =
  let result = Render.render_help ~num_pagers:5 in
  Alcotest.(check bool) "has content" true (String.length result > 0)

(* ============================================================ *)
(* Hidden Indicator Tests                                        *)
(* ============================================================ *)

let test_hidden_indicator_none () =
  let result =
    Render.render_hidden_indicator ~hidden_left:[] ~hidden_right:[]
  in
  Alcotest.(check string) "empty" "" result

let test_hidden_indicator_left () =
  let result =
    Render.render_hidden_indicator ~hidden_left:[0; 1] ~hidden_right:[]
  in
  Alcotest.(check bool) "has content" true (String.length result > 0)

let test_hidden_indicator_right () =
  let result =
    Render.render_hidden_indicator ~hidden_left:[] ~hidden_right:[3; 4]
  in
  Alcotest.(check bool) "has content" true (String.length result > 0)

let test_hidden_indicator_both () =
  let result =
    Render.render_hidden_indicator ~hidden_left:[0] ~hidden_right:[5]
  in
  Alcotest.(check bool) "has content" true (String.length result > 0)

(* ============================================================ *)
(* Layout Calculation Tests                                      *)
(* ============================================================ *)

let test_calculate_layout_single () =
  let Render.{grid_cols; grid_rows}, max_visible =
    Render.calculate_layout ~cols:80 ~rows:24 ~num_pagers:1
  in
  Alcotest.(check int) "max_visible" 1 max_visible ;
  Alcotest.(check int) "grid_cols" 1 grid_cols ;
  Alcotest.(check int) "grid_rows" 1 grid_rows

let test_calculate_layout_prefers_horizontal_when_wide () =
  (* Wide terminal: 200 cols, 24 rows, 2 pagers
     Horizontal 2x1: each gets 100 cols x 24 rows = 2400 area
     Vertical 1x2: each gets 200 cols x 12 rows = 2400 area - but 12 < min_pager_rows
     Should use horizontal *)
  let Render.{grid_cols; grid_rows}, max_visible =
    Render.calculate_layout ~cols:200 ~rows:24 ~num_pagers:2
  in
  Alcotest.(check int) "max_visible" 2 max_visible ;
  Alcotest.(check int) "grid_cols for wide" 2 grid_cols ;
  Alcotest.(check int) "grid_rows for wide" 1 grid_rows

let test_calculate_layout_prefers_vertical_when_tall () =
  (* Narrow but tall: 80 cols, 60 rows, 2 pagers
     Horizontal 2x1: each gets 40 cols - 40 < min_pager_cols (80), not viable
     Vertical 1x2: each gets 80 cols x 30 rows, viable *)
  let Render.{grid_cols; grid_rows}, max_visible =
    Render.calculate_layout ~cols:80 ~rows:60 ~num_pagers:2
  in
  Alcotest.(check int) "max_visible" 2 max_visible ;
  Alcotest.(check int) "grid_cols for tall" 1 grid_cols ;
  Alcotest.(check int) "grid_rows for tall" 2 grid_rows

let test_calculate_layout_grid () =
  (* Large terminal: 320 cols, 72 rows, 5 pagers
     Should arrange in a 2x3 grid (capacity 6, shows all 5) *)
  let Render.{grid_cols; grid_rows}, max_visible =
    Render.calculate_layout ~cols:320 ~rows:72 ~num_pagers:5
  in
  Alcotest.(check int) "max_visible for grid" 5 max_visible ;
  (* Grid should accommodate all 5: either 2x3 or 3x2 depending on which gives more area *)
  Alcotest.(check bool) "fits all pagers" true (grid_cols * grid_rows >= 5)

(* ============================================================ *)
(* Visible Pagers Tests                                          *)
(* ============================================================ *)

let test_get_visible_pagers_all_fit () =
  let pagers = [make_pager_slot ~id:0 (); make_pager_slot ~id:1 ()] in
  let visible, left, right =
    Render.get_visible_pagers ~pagers ~focused_id:0 ~max_visible:5
  in
  Alcotest.(check int) "all visible" 2 (List.length visible) ;
  Alcotest.(check int) "none left" 0 (List.length left) ;
  Alcotest.(check int) "none right" 0 (List.length right)

let test_get_visible_pagers_overflow () =
  let pagers = List.init 5 (fun i -> make_pager_slot ~id:i ()) in
  let visible, left, right =
    Render.get_visible_pagers ~pagers ~focused_id:2 ~max_visible:2
  in
  Alcotest.(check int) "2 visible" 2 (List.length visible) ;
  Alcotest.(check bool)
    "some hidden"
    true
    (List.length left + List.length right > 0)

(* ============================================================ *)
(* Pager Tabs Tests                                              *)
(* ============================================================ *)

let test_render_pager_tabs () =
  let pagers =
    [
      make_pager_slot ~id:0 ();
      make_pager_slot ~id:1 ();
      make_pager_slot ~id:2 ();
    ]
  in
  let result = Render.render_pager_tabs ~pagers ~focused_id:1 in
  Alcotest.(check bool) "has content" true (String.length result > 0)

(* ============================================================ *)
(* Full Render Tests                                             *)
(* ============================================================ *)

let test_render_list_mode () =
  let state = State.init ~instances:[] in
  let result = Render.render ~state ~cols:80 ~rows:24 ~focus:true in
  Alcotest.(check bool) "has content" true (String.length result >= 1)

let test_render_result_mode () =
  let state = State.init ~instances:[] in
  let state = State.execute_get ~url:"http://localhost/version" state in
  let state = State.set_result ~raw_body:"{}" state in
  let result = Render.render ~state ~cols:80 ~rows:24 ~focus:true in
  Alcotest.(check bool) "has content" true (String.length result > 2)

let test_render_result_with_error () =
  let state = State.init ~instances:[] in
  let state = State.execute_get ~url:"http://localhost/error" state in
  let state = State.set_result ~raw_body:"error data" state in
  let state = State.set_error "Parse error" state in
  let result = Render.render ~state ~cols:80 ~rows:24 ~focus:true in
  Alcotest.(check bool) "has content" true (String.length result > 0)

let test_render_multi_pager () =
  let state = State.init ~instances:[] in
  let state = State.execute_get ~url:"http://localhost/v1" state in
  let state = State.set_result ~raw_body:"{}" state in
  let state = match State.add_pager state with Some s -> s | None -> state in
  let result = Render.render ~state ~cols:200 ~rows:50 ~focus:true in
  Alcotest.(check bool) "has content" true (String.length result > 0)

(* ============================================================ *)
(* visible_length Tests                                          *)
(* ============================================================ *)

module FT = Render.For_tests

let test_visible_length_plain () =
  Alcotest.(check int) "plain ASCII" 5 (FT.visible_length "hello")

let test_visible_length_empty () =
  Alcotest.(check int) "empty" 0 (FT.visible_length "")

let test_visible_length_ansi () =
  (* \027[31m = red, \027[0m = reset *)
  let s = "\027[31mhello\027[0m" in
  Alcotest.(check int) "ANSI stripped" 5 (FT.visible_length s)

let test_visible_length_multiple_ansi () =
  let s = "\027[1m\027[31mhi\027[0m" in
  Alcotest.(check int) "multiple ANSI" 2 (FT.visible_length s)

let test_visible_length_utf8 () =
  (* "café" = 4 visible chars, but 5 bytes (é is 2 bytes) *)
  Alcotest.(check int) "UTF-8" 4 (FT.visible_length "caf\xc3\xa9")

let test_visible_length_utf8_emoji () =
  (* 😀 = 4 bytes, 1 visible char *)
  Alcotest.(check int) "emoji" 1 (FT.visible_length "\xf0\x9f\x98\x80")

let test_visible_length_mixed () =
  (* ANSI + UTF-8 *)
  let s = "\027[32mcaf\xc3\xa9\027[0m" in
  Alcotest.(check int) "mixed ANSI+UTF8" 4 (FT.visible_length s)

(* ============================================================ *)
(* truncate_to_width Tests                                       *)
(* ============================================================ *)

let test_truncate_short () =
  let result = FT.truncate_to_width "hi" ~width:10 in
  Alcotest.(check string) "not truncated" "hi" result

let test_truncate_exact () =
  let result = FT.truncate_to_width "hello" ~width:5 in
  Alcotest.(check string) "exact fit" "hello" result

let test_truncate_long () =
  let result = FT.truncate_to_width "hello world" ~width:5 in
  Alcotest.(check int) "truncated length" 5 (FT.visible_length result)

let test_truncate_ansi () =
  let s = "\027[31mhello world\027[0m" in
  let result = FT.truncate_to_width s ~width:5 in
  Alcotest.(check int) "truncated ANSI" 5 (FT.visible_length result)

let test_truncate_zero_width () =
  let result = FT.truncate_to_width "hello" ~width:0 in
  Alcotest.(check int) "zero width" 0 (FT.visible_length result)

(* ============================================================ *)
(* split_lines_padded Tests                                      *)
(* ============================================================ *)

let test_split_lines_exact () =
  let lines = FT.split_lines_padded "a\nb\nc" ~target_lines:3 ~width:5 in
  Alcotest.(check int) "3 lines" 3 (List.length lines) ;
  List.iter
    (fun line ->
      Alcotest.(check int) "each line width 5" 5 (FT.visible_length line))
    lines

let test_split_lines_too_few () =
  let lines = FT.split_lines_padded "a" ~target_lines:3 ~width:5 in
  Alcotest.(check int) "padded to 3" 3 (List.length lines)

let test_split_lines_too_many () =
  let lines = FT.split_lines_padded "a\nb\nc\nd\ne" ~target_lines:2 ~width:5 in
  Alcotest.(check int) "trimmed to 2" 2 (List.length lines)

let test_split_lines_empty () =
  let lines = FT.split_lines_padded "" ~target_lines:3 ~width:5 in
  Alcotest.(check int) "empty padded" 3 (List.length lines)

(* ============================================================ *)
(* PBT: visible_length invariants                                *)
(* ============================================================ *)

let test_visible_length_non_negative =
  QCheck.Test.make
    ~name:"visible_length is always non-negative"
    ~count:500
    QCheck.string
    (fun s -> FT.visible_length s >= 0)

let test_truncate_respects_width =
  QCheck.Test.make
    ~name:"truncate_to_width respects width"
    ~count:500
    QCheck.(pair string (int_range 0 100))
    (fun (s, width) ->
      let result = FT.truncate_to_width s ~width in
      FT.visible_length result <= width)

(* ============================================================ *)
(* Test Runner                                                   *)
(* ============================================================ *)

let () =
  Alcotest.run
    "Rpc_browser_render_result"
    [
      ( "pager_header",
        [
          Alcotest.test_case "focused" `Quick test_render_pager_header_focused;
          Alcotest.test_case
            "unfocused"
            `Quick
            test_render_pager_header_unfocused;
          Alcotest.test_case "target" `Quick test_render_pager_header_target;
          Alcotest.test_case "empty" `Quick test_render_pager_header_empty;
        ] );
      ( "loading_error",
        [
          Alcotest.test_case "loading" `Quick test_render_loading;
          Alcotest.test_case "error" `Quick test_render_error;
        ] );
      ( "help",
        [
          Alcotest.test_case "single" `Quick test_render_help_single;
          Alcotest.test_case "multiple" `Quick test_render_help_multiple;
        ] );
      ( "hidden_indicator",
        [
          Alcotest.test_case "none" `Quick test_hidden_indicator_none;
          Alcotest.test_case "left" `Quick test_hidden_indicator_left;
          Alcotest.test_case "right" `Quick test_hidden_indicator_right;
          Alcotest.test_case "both" `Quick test_hidden_indicator_both;
        ] );
      ( "layout",
        [
          Alcotest.test_case "single" `Quick test_calculate_layout_single;
          Alcotest.test_case
            "horizontal when wide"
            `Quick
            test_calculate_layout_prefers_horizontal_when_wide;
          Alcotest.test_case
            "vertical when tall"
            `Quick
            test_calculate_layout_prefers_vertical_when_tall;
          Alcotest.test_case "grid" `Quick test_calculate_layout_grid;
        ] );
      ( "visible_pagers",
        [
          Alcotest.test_case "all fit" `Quick test_get_visible_pagers_all_fit;
          Alcotest.test_case "overflow" `Quick test_get_visible_pagers_overflow;
        ] );
      ("pager_tabs", [Alcotest.test_case "render" `Quick test_render_pager_tabs]);
      ( "render",
        [
          Alcotest.test_case "list mode" `Quick test_render_list_mode;
          Alcotest.test_case "result mode" `Quick test_render_result_mode;
          Alcotest.test_case "with error" `Quick test_render_result_with_error;
          Alcotest.test_case "multi pager" `Quick test_render_multi_pager;
        ] );
      ( "visible_length",
        [
          Alcotest.test_case "plain" `Quick test_visible_length_plain;
          Alcotest.test_case "empty" `Quick test_visible_length_empty;
          Alcotest.test_case "ANSI" `Quick test_visible_length_ansi;
          Alcotest.test_case
            "multiple ANSI"
            `Quick
            test_visible_length_multiple_ansi;
          Alcotest.test_case "UTF-8" `Quick test_visible_length_utf8;
          Alcotest.test_case "emoji" `Quick test_visible_length_utf8_emoji;
          Alcotest.test_case "mixed" `Quick test_visible_length_mixed;
        ] );
      ( "truncate_to_width",
        [
          Alcotest.test_case "short" `Quick test_truncate_short;
          Alcotest.test_case "exact" `Quick test_truncate_exact;
          Alcotest.test_case "long" `Quick test_truncate_long;
          Alcotest.test_case "ANSI" `Quick test_truncate_ansi;
          Alcotest.test_case "zero width" `Quick test_truncate_zero_width;
        ] );
      ( "split_lines_padded",
        [
          Alcotest.test_case "exact" `Quick test_split_lines_exact;
          Alcotest.test_case "too few" `Quick test_split_lines_too_few;
          Alcotest.test_case "too many" `Quick test_split_lines_too_many;
          Alcotest.test_case "empty" `Quick test_split_lines_empty;
        ] );
      ( "PBT",
        List.map
          QCheck_alcotest.to_alcotest
          [test_visible_length_non_negative; test_truncate_respects_width] );
    ]
