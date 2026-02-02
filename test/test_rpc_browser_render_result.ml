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
(* Header Tests                                                  *)
(* ============================================================ *)

let test_render_header () =
  let result =
    Render.render_header
      ~request:"/chains/main/blocks/head"
      ~response_time_ms:None
      ~response_size:None
  in
  Alcotest.(check bool) "has content" true (String.length result > 0)

let test_render_header_with_time () =
  let result =
    Render.render_header
      ~request:"/version"
      ~response_time_ms:(Some 42.0)
      ~response_size:(Some 1234)
  in
  Alcotest.(check bool) "has content" true (String.length result > 0)

(* ============================================================ *)
(* Body Rendering Tests                                          *)
(* ============================================================ *)

let test_render_body_short () =
  let body = "line1\nline2\nline3" in
  let lines = Render.render_body ~body ~scroll_offset:0 ~visible_height:10 in
  Alcotest.(check int) "3 lines" 3 (List.length lines)

let test_render_body_scrolled () =
  let body = "line1\nline2\nline3\nline4\nline5" in
  let lines = Render.render_body ~body ~scroll_offset:2 ~visible_height:2 in
  Alcotest.(check int) "2 visible" 2 (List.length lines) ;
  Alcotest.(check string) "starts at line3" "line3" (List.hd lines)

let test_render_body_overflow () =
  let body = "line1\nline2\nline3" in
  let lines = Render.render_body ~body ~scroll_offset:10 ~visible_height:2 in
  (* Should clamp to end *)
  Alcotest.(check int) "clamped" 2 (List.length lines)

let test_render_body_empty () =
  let body = "" in
  let lines = Render.render_body ~body ~scroll_offset:0 ~visible_height:10 in
  Alcotest.(check int) "1 empty line" 1 (List.length lines)

(* ============================================================ *)
(* Scroll Indicator Tests                                        *)
(* ============================================================ *)

let test_scroll_indicator_single () =
  let result = Render.render_scroll_indicator ~current:0 ~total:1 in
  Alcotest.(check string) "empty" "" result

let test_scroll_indicator_multiple () =
  let result = Render.render_scroll_indicator ~current:5 ~total:100 in
  Alcotest.(check bool) "has content" true (String.length result > 0)

let test_scroll_indicator_end () =
  let result = Render.render_scroll_indicator ~current:99 ~total:100 in
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

let test_render_help () =
  let result = Render.render_help () in
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

let test_render_result_with_scroll () =
  let state = State.init ~instances:[] in
  let state = State.execute_get ~url:"http://localhost/data" state in
  let long_body =
    String.concat "\n" (List.init 100 (fun i -> Printf.sprintf "line %d" i))
  in
  let state = State.set_result ~body:long_body ~raw_body:long_body state in
  let state = State.scroll 10 state in
  let result = Render.render ~state ~cols:80 ~rows:24 ~focus:true in
  Alcotest.(check bool) "has content" true (String.length result > 0)

let test_render_result_with_error () =
  let state = State.init ~instances:[] in
  let state = State.execute_get ~url:"http://localhost/error" state in
  let state =
    State.set_result ~body:"error data" ~raw_body:"error data" state
  in
  let state = State.set_error "Parse error" state in
  let result = Render.render ~state ~cols:80 ~rows:24 ~focus:true in
  Alcotest.(check bool) "has content" true (String.length result > 0)

(* ============================================================ *)
(* Test Runner                                                   *)
(* ============================================================ *)

let () =
  Alcotest.run
    "Rpc_browser_render_result"
    [
      ( "header",
        [
          Alcotest.test_case "render" `Quick test_render_header;
          Alcotest.test_case "with time" `Quick test_render_header_with_time;
        ] );
      ( "body",
        [
          Alcotest.test_case "short" `Quick test_render_body_short;
          Alcotest.test_case "scrolled" `Quick test_render_body_scrolled;
          Alcotest.test_case "overflow" `Quick test_render_body_overflow;
          Alcotest.test_case "empty" `Quick test_render_body_empty;
        ] );
      ( "scroll_indicator",
        [
          Alcotest.test_case "single" `Quick test_scroll_indicator_single;
          Alcotest.test_case "multiple" `Quick test_scroll_indicator_multiple;
          Alcotest.test_case "end" `Quick test_scroll_indicator_end;
        ] );
      ( "loading_error",
        [
          Alcotest.test_case "loading" `Quick test_render_loading;
          Alcotest.test_case "error" `Quick test_render_error;
        ] );
      ("help", [Alcotest.test_case "render" `Quick test_render_help]);
      ( "render",
        [
          Alcotest.test_case "list mode" `Quick test_render_list_mode;
          Alcotest.test_case "result mode" `Quick test_render_result_mode;
          Alcotest.test_case "with scroll" `Quick test_render_result_with_scroll;
          Alcotest.test_case "with error" `Quick test_render_result_with_error;
        ] );
    ]
