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
  let result = Render.render_pager_header ~slot ~is_focused:true in
  Alcotest.(check bool) "has content" true (String.length result > 0)

let test_render_pager_header_unfocused () =
  let slot = make_pager_slot ~id:1 ~request:"/version" () in
  let result = Render.render_pager_header ~slot ~is_focused:false in
  Alcotest.(check bool) "has content" true (String.length result > 0)

let test_render_pager_header_empty () =
  let slot = make_pager_slot ~id:2 () in
  let result = Render.render_pager_header ~slot ~is_focused:true in
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
  let layout, visible_count, max_visible =
    Render.calculate_layout ~cols:80 ~rows:24 ~num_pagers:1
  in
  Alcotest.(check int) "visible_count" 1 visible_count ;
  Alcotest.(check int) "max_visible" 1 max_visible ;
  (* Single pager always uses vertical layout *)
  Alcotest.(check bool) "vertical layout" true (layout = Render.Vertical)

let test_calculate_layout_prefers_horizontal_when_wide () =
  (* Wide terminal: 200 cols, 24 rows, 2 pagers
     Horizontal: each gets 100 cols x 24 rows = 2400 area
     Vertical: each gets 200 cols x 12 rows = 2400 area
     Should prefer horizontal when equal or better *)
  let layout, visible_count, _max_visible =
    Render.calculate_layout ~cols:200 ~rows:24 ~num_pagers:2
  in
  Alcotest.(check int) "visible_count" 2 visible_count ;
  Alcotest.(check bool)
    "horizontal layout for wide terminal"
    true
    (layout = Render.Horizontal)

let test_calculate_layout_prefers_vertical_when_tall () =
  (* Narrow but tall: 80 cols, 60 rows, 2 pagers
     Horizontal: each gets 40 cols x 60 rows - 40 < min_pager_cols (80), not viable
     Vertical: each gets 80 cols x 30 rows = 2400 area, viable *)
  let layout, visible_count, _max_visible =
    Render.calculate_layout ~cols:80 ~rows:60 ~num_pagers:2
  in
  Alcotest.(check int) "visible_count" 2 visible_count ;
  Alcotest.(check bool)
    "vertical layout for tall terminal"
    true
    (layout = Render.Vertical)

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
  let state =
    State.set_result ~body:"{\"version\": \"1.0\"}" ~raw_body:"{}" state
  in
  let result = Render.render ~state ~cols:80 ~rows:24 ~focus:true in
  Alcotest.(check bool) "has content" true (String.length result > 2)

let test_render_result_with_error () =
  let state = State.init ~instances:[] in
  let state = State.execute_get ~url:"http://localhost/error" state in
  let state =
    State.set_result ~body:"error data" ~raw_body:"error data" state
  in
  let state = State.set_error "Parse error" state in
  let result = Render.render ~state ~cols:80 ~rows:24 ~focus:true in
  Alcotest.(check bool) "has content" true (String.length result > 0)

let test_render_multi_pager () =
  let state = State.init ~instances:[] in
  let state = State.execute_get ~url:"http://localhost/v1" state in
  let state = State.set_result ~body:"{}" ~raw_body:"{}" state in
  let state = match State.add_pager state with Some s -> s | None -> state in
  let result = Render.render ~state ~cols:200 ~rows:50 ~focus:true in
  Alcotest.(check bool) "has content" true (String.length result > 0)

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
    ]
