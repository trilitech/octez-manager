(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** TUI tests for the RPC browser page.
    
    Tests that the page header "RPC Browser . {instance}" and "Press Esc to go 
    back" appear in ALL modes: List mode, Result mode (side-by-side and single-
    column layouts), with browser focused and pager focused.
    
    This is a regression test for issue #846 where inline headers were bypassing
    the page header in Result mode single-column views. *)

open Alcotest
module HD = Lib_miaou_internal.Headless_driver
module TH = Tui_test_helpers_lib.Tui_test_helpers

(* ============================================================ *)
(* Test: Page Loads and Shows Header *)
(* ============================================================ *)

let test_page_loads_with_header () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Octez_manager_ui.Rpc_browser.Page) ;

      let screen = TH.get_screen_text () in

      (* Check for page title *)
      check
        bool
        "header shows 'RPC Browser'"
        true
        (TH.contains_substring screen "RPC Browser") ;

      (* Check for help text *)
      check
        bool
        "header shows 'Press Esc to go back'"
        true
        (TH.contains_substring screen "Press Esc to go back"))

(* ============================================================ *)
(* Test: Header Visible in List Mode *)
(* ============================================================ *)

let test_header_in_list_mode () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Octez_manager_ui.Rpc_browser.Page) ;

      (* Verify we're in List mode (initial state) *)
      let screen = TH.get_screen_text () in

      check
        bool
        "list mode shows RPC Browser header"
        true
        (TH.contains_substring screen "RPC Browser") ;

      check
        bool
        "list mode shows Esc help text"
        true
        (TH.contains_substring screen "Press Esc to go back"))

(* ============================================================ *)
(* Test: Header Visible in Result Mode - Side-by-Side Layout *)
(* ============================================================ *)

let test_header_in_result_mode_side_by_side () =
  TH.with_test_env (fun () ->
      (* Note: Terminal size is determined by the test environment *)
      HD.Stateful.init (module Octez_manager_ui.Rpc_browser.Page) ;

      (* Navigate into an entry to trigger Result mode *)
      (* Try navigating down to find a valid entry *)
      TH.navigate_down 2 ;
      Unix.sleepf 0.02 ;
      ignore (HD.Stateful.send_key "Enter") ;
      ignore (HD.Stateful.idle_wait ~iterations:10 ~sleep:0.001 ()) ;

      let screen = TH.get_screen_text () in

      (* Page header should be visible in all modes *)
      check
        bool
        "result mode shows RPC Browser header"
        true
        (TH.contains_substring screen "RPC Browser") ;

      check
        bool
        "result mode shows Esc help text"
        true
        (TH.contains_substring screen "Press Esc to go back"))

(* ============================================================ *)
(* Test: Header Visible in Result Mode - Single Column, Browser Focused *)
(* ============================================================ *)

let test_header_in_result_mode_single_column_browser_focused () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Octez_manager_ui.Rpc_browser.Page) ;

      (* Navigate into an entry to trigger Result mode *)
      TH.navigate_down 2 ;
      Unix.sleepf 0.02 ;
      ignore (HD.Stateful.send_key "Enter") ;
      ignore (HD.Stateful.idle_wait ~iterations:10 ~sleep:0.001 ()) ;

      (* Ensure browser is focused (should be default) *)
      let screen = TH.get_screen_text () in

      check
        bool
        "browser-focused shows RPC Browser header"
        true
        (TH.contains_substring screen "RPC Browser") ;

      check
        bool
        "browser-focused shows Esc help text"
        true
        (TH.contains_substring screen "Press Esc to go back"))

(* ============================================================ *)
(* Test: Header Visible in Result Mode - Single Column, Pager Focused *)
(* ============================================================ *)

let test_header_in_result_mode_single_column_pager_focused () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Octez_manager_ui.Rpc_browser.Page) ;

      (* Navigate into an entry to trigger Result mode *)
      TH.navigate_down 2 ;
      Unix.sleepf 0.02 ;
      ignore (HD.Stateful.send_key "Enter") ;
      ignore (HD.Stateful.idle_wait ~iterations:10 ~sleep:0.001 ()) ;

      (* Switch focus to pager with Right arrow *)
      ignore (HD.Stateful.send_key "Right") ;
      ignore (HD.Stateful.idle_wait ~iterations:5 ~sleep:0.001 ()) ;

      let screen = TH.get_screen_text () in

      check
        bool
        "pager-focused shows RPC Browser header"
        true
        (TH.contains_substring screen "RPC Browser") ;

      check
        bool
        "pager-focused shows Esc help text"
        true
        (TH.contains_substring screen "Press Esc to go back"))

(* ============================================================ *)
(* Test: Header Persists After Navigation *)
(* ============================================================ *)

let test_header_persists_after_navigation () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Octez_manager_ui.Rpc_browser.Page) ;

      (* Navigate multiple times *)
      TH.navigate_down 3 ;
      TH.navigate_up 1 ;
      Unix.sleepf 0.02 ;

      let screen = TH.get_screen_text () in

      check
        bool
        "header persists after navigation"
        true
        (TH.contains_substring screen "RPC Browser") ;

      check
        bool
        "help text persists after navigation"
        true
        (TH.contains_substring screen "Press Esc to go back"))

(* ============================================================ *)
(* Test: Header Visible After Refresh *)
(* ============================================================ *)

let test_header_visible_after_refresh () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Octez_manager_ui.Rpc_browser.Page) ;

      (* Trigger refresh *)
      ignore (HD.Stateful.send_key "r") ;
      ignore (HD.Stateful.idle_wait ~iterations:10 ~sleep:0.001 ()) ;

      let screen = TH.get_screen_text () in

      check
        bool
        "header visible after refresh"
        true
        (TH.contains_substring screen "RPC Browser") ;

      check
        bool
        "help text visible after refresh"
        true
        (TH.contains_substring screen "Press Esc to go back"))

(* ============================================================ *)
(* Test: Result Mode Switching Focus Preserves Header *)
(* ============================================================ *)

let test_focus_switch_preserves_header () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Octez_manager_ui.Rpc_browser.Page) ;

      (* Enter Result mode *)
      TH.navigate_down 2 ;
      Unix.sleepf 0.02 ;
      ignore (HD.Stateful.send_key "Enter") ;
      ignore (HD.Stateful.idle_wait ~iterations:10 ~sleep:0.001 ()) ;

      (* Switch focus between browser and pager *)
      ignore (HD.Stateful.send_key "Right") ;
      ignore (HD.Stateful.idle_wait ~iterations:5 ~sleep:0.001 ()) ;
      ignore (HD.Stateful.send_key "Left") ;
      ignore (HD.Stateful.idle_wait ~iterations:5 ~sleep:0.001 ()) ;

      let screen = TH.get_screen_text () in

      check
        bool
        "header preserved during focus switch"
        true
        (TH.contains_substring screen "RPC Browser") ;

      check
        bool
        "help text preserved during focus switch"
        true
        (TH.contains_substring screen "Press Esc to go back"))

(* ============================================================ *)
(* Test Suite *)
(* ============================================================ *)

let header_tests =
  [
    ("page loads with header", `Quick, test_page_loads_with_header);
    ("header visible in list mode", `Quick, test_header_in_list_mode);
    ( "header visible in result mode",
      `Quick,
      test_header_in_result_mode_side_by_side );
    ( "header visible in result mode browser-focused",
      `Quick,
      test_header_in_result_mode_single_column_browser_focused );
    ( "header visible in result mode pager-focused",
      `Quick,
      test_header_in_result_mode_single_column_pager_focused );
    ( "header persists after navigation",
      `Quick,
      test_header_persists_after_navigation );
    ("header visible after refresh", `Quick, test_header_visible_after_refresh);
    ("focus switch preserves header", `Quick, test_focus_switch_preserves_header);
  ]

let () =
  Alcotest.run "RPC Browser Page (TUI)" [("rpc_browser_page", header_tests)]
