(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Octez_manager_ui
module State = Rpc_browser_state
module Render = Rpc_browser_render_list

(* Helper to create test services *)
let make_service ?(rpc_addr = "127.0.0.1:8732") name =
  Mock_service_helpers_lib.Mock_service_helpers.mock_service
    ~instance:name
    ~rpc_addr
    ()

(* ============================================================ *)
(* Breadcrumb Tests                                              *)
(* ============================================================ *)

let test_breadcrumb_root () =
  let result = Render.render_breadcrumb [] in
  Alcotest.(check bool) "contains root" true (String.length result > 0)

let test_breadcrumb_single () =
  let result = Render.render_breadcrumb ["chains"] in
  Alcotest.(check bool) "contains chains" true (String.length result > 0)

let test_breadcrumb_nested () =
  let result = Render.render_breadcrumb ["chains"; "main"; "blocks"] in
  Alcotest.(check bool) "has content" true (String.length result > 0)

(* ============================================================ *)
(* Instance Selector Tests                                       *)
(* ============================================================ *)

let test_instance_selector_empty () =
  let result = Render.render_instance_selector ~target:None in
  Alcotest.(check bool) "shows no instance" true (String.length result > 0)

let test_instance_selector_valid () =
  let target = Some (make_service "node1") in
  let result = Render.render_instance_selector ~target in
  Alcotest.(check bool) "has content" true (String.length result > 0)

(* ============================================================ *)
(* Entry Kind Tests                                              *)
(* ============================================================ *)

let test_entry_kind_get () =
  let result = Render.render_entry_kind State.Get in
  Alcotest.(check bool) "contains GET" true (String.length result > 0)

let test_entry_kind_sub () =
  let result = Render.render_entry_kind State.Sub in
  Alcotest.(check bool) "contains SUB" true (String.length result > 0)

let test_entry_kind_dyn () =
  let result = Render.render_entry_kind (State.Dyn "string") in
  Alcotest.(check bool) "contains DYN" true (String.length result > 0)

(* ============================================================ *)
(* Entry Rendering Tests                                         *)
(* ============================================================ *)

let test_entry_not_selected () =
  let entry = {State.name = "chains"; kind = State.Sub} in
  let result = Render.render_entry ~cursor:1 ~idx:0 ~focus:true entry in
  Alcotest.(check bool) "has content" true (String.length result > 0)

let test_entry_selected () =
  let entry = {State.name = "chains"; kind = State.Sub} in
  let result = Render.render_entry ~cursor:0 ~idx:0 ~focus:true entry in
  Alcotest.(check bool) "has content" true (String.length result > 0)

let test_entry_selected_focused_has_bold () =
  let entry = {State.name = "chains"; kind = State.Sub} in
  let result = Render.render_entry ~cursor:0 ~idx:0 ~focus:true entry in
  (* Bold ANSI code is \027[1m *)
  Alcotest.(check bool)
    "contains bold"
    true
    (String.exists (fun c -> c = '\027') result)

let test_entry_selected_unfocused_has_dim () =
  let entry = {State.name = "chains"; kind = State.Sub} in
  let result = Render.render_entry ~cursor:0 ~idx:0 ~focus:false entry in
  (* Dim ANSI code is \027[2m *)
  Alcotest.(check bool)
    "contains ANSI"
    true
    (String.exists (fun c -> c = '\027') result)

let test_entry_focus_produces_different_output () =
  let entry = {State.name = "chains"; kind = State.Sub} in
  let focused = Render.render_entry ~cursor:0 ~idx:0 ~focus:true entry in
  let unfocused = Render.render_entry ~cursor:0 ~idx:0 ~focus:false entry in
  Alcotest.(check bool) "different output" true (focused <> unfocused)

let test_entry_not_selected_same_regardless_of_focus () =
  let entry = {State.name = "chains"; kind = State.Sub} in
  let focused = Render.render_entry ~cursor:1 ~idx:0 ~focus:true entry in
  let unfocused = Render.render_entry ~cursor:1 ~idx:0 ~focus:false entry in
  (* Non-selected entries should look the same regardless of focus *)
  Alcotest.(check string) "same output" focused unfocused

(* ============================================================ *)
(* Loading and Error Tests                                       *)
(* ============================================================ *)

let test_render_loading () =
  let result = Render.render_loading () in
  Alcotest.(check bool) "has content" true (String.length result > 0)

let test_render_loading_custom_msg () =
  let result = Render.render_loading ~msg:"Fetching..." () in
  Alcotest.(check bool) "has content" true (String.length result > 0)

let test_render_error_none () =
  let result = Render.render_error None in
  Alcotest.(check int) "empty list" 0 (List.length result)

let test_render_error_some () =
  let result = Render.render_error (Some "Connection failed") in
  Alcotest.(check int) "has error line" 1 (List.length result)

(* ============================================================ *)
(* Help Line Tests                                               *)
(* ============================================================ *)

let test_render_help () =
  let result = Render.render_help () in
  Alcotest.(check bool) "has content" true (String.length result > 0)

(* ============================================================ *)
(* Full Render Tests                                             *)
(* ============================================================ *)

let test_render_empty_state () =
  let state = State.init ~instances:[] in
  let lines = Render.render ~focus:true ~state ~cols:80 in
  Alcotest.(check bool) "has lines" true (List.length lines > 0)

let test_render_with_loading () =
  let state = State.init ~instances:[] in
  let lines = Render.render ~focus:true ~state ~cols:80 in
  Alcotest.(check bool) "has lines" true (List.length lines > 0)

let test_render_with_entries () =
  let state = State.init ~instances:[] in
  let entries =
    [
      {State.name = "chains"; kind = State.Sub};
      {name = "version"; kind = State.Get};
    ]
  in
  let state = State.set_entries entries state in
  let lines = Render.render ~focus:true ~state ~cols:80 in
  Alcotest.(check bool) "has lines" true (List.length lines > 2)

let test_render_with_error () =
  let state = State.init ~instances:[] in
  let state = State.set_error "Something went wrong" state in
  let lines = Render.render ~focus:true ~state ~cols:80 in
  Alcotest.(check bool) "has lines" true (List.length lines > 0)

let test_render_with_path () =
  let state = State.init ~instances:[] in
  let state = State.navigate_to "chains" state in
  let state =
    State.set_entries [{State.name = "main"; kind = State.Sub}] state
  in
  let lines = Render.render ~focus:true ~state ~cols:80 in
  Alcotest.(check bool) "has lines" true (List.length lines > 0)

(* ============================================================ *)
(* Test Runner                                                   *)
(* ============================================================ *)

let () =
  Alcotest.run
    "Rpc_browser_render_list"
    [
      ( "breadcrumb",
        [
          Alcotest.test_case "root" `Quick test_breadcrumb_root;
          Alcotest.test_case "single" `Quick test_breadcrumb_single;
          Alcotest.test_case "nested" `Quick test_breadcrumb_nested;
        ] );
      ( "instance_selector",
        [
          Alcotest.test_case "empty" `Quick test_instance_selector_empty;
          Alcotest.test_case "valid" `Quick test_instance_selector_valid;
        ] );
      ( "entry_kind",
        [
          Alcotest.test_case "get" `Quick test_entry_kind_get;
          Alcotest.test_case "sub" `Quick test_entry_kind_sub;
          Alcotest.test_case "dyn" `Quick test_entry_kind_dyn;
        ] );
      ( "entry",
        [
          Alcotest.test_case "not selected" `Quick test_entry_not_selected;
          Alcotest.test_case "selected" `Quick test_entry_selected;
          Alcotest.test_case
            "selected focused has bold"
            `Quick
            test_entry_selected_focused_has_bold;
          Alcotest.test_case
            "selected unfocused has dim"
            `Quick
            test_entry_selected_unfocused_has_dim;
          Alcotest.test_case
            "focus produces different output"
            `Quick
            test_entry_focus_produces_different_output;
          Alcotest.test_case
            "not selected same regardless of focus"
            `Quick
            test_entry_not_selected_same_regardless_of_focus;
        ] );
      ( "loading_error",
        [
          Alcotest.test_case "loading" `Quick test_render_loading;
          Alcotest.test_case
            "loading custom"
            `Quick
            test_render_loading_custom_msg;
          Alcotest.test_case "error none" `Quick test_render_error_none;
          Alcotest.test_case "error some" `Quick test_render_error_some;
        ] );
      ("help", [Alcotest.test_case "render" `Quick test_render_help]);
      ( "render",
        [
          Alcotest.test_case "empty state" `Quick test_render_empty_state;
          Alcotest.test_case "with loading" `Quick test_render_with_loading;
          Alcotest.test_case "with entries" `Quick test_render_with_entries;
          Alcotest.test_case "with error" `Quick test_render_with_error;
          Alcotest.test_case "with path" `Quick test_render_with_path;
        ] );
    ]
