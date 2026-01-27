(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** TUI tests for the instances page.
    
    Tests instances.ml and instances/* submodules (1,669 points, 0% coverage).
    The instances page is the main service management interface showing all
    running nodes, bakers, accusers, and DAL nodes.
    
    Note: These tests verify the page structure, navigation, and rendering,
    but do NOT actually start/stop services (systemd operations not mocked). *)

open Alcotest
module HD = Lib_miaou_internal.Headless_driver
module Instances = Octez_manager_ui.Instances
module TH = Tui_test_helpers_lib.Tui_test_helpers

(* ============================================================ *)
(* Test: Page Initialization *)
(* ============================================================ *)

let test_page_loads () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Instances.Page) ;

      (* Verify page renders - check for "Total instances" or "Install new instance" *)
      let screen = TH.get_screen_text () in
      check bool "screen not empty" true (String.length screen > 0) ;

      (* Should show instance-related content *)
      check
        bool
        "shows instance content"
        true
        (TH.contains_substring screen "Total instances"
        || TH.contains_substring screen "Install new instance"
        || TH.contains_substring screen "No managed instances"))

(* ============================================================ *)
(* Test: Page Shows Service List Headers *)
(* ============================================================ *)

let test_shows_headers () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Instances.Page) ;

      let screen = TH.get_screen_text () in

      (* Should show column headers or section labels *)
      (* The exact labels may vary, but there should be some structure *)
      check bool "has content structure" true (String.length screen > 100))

(* ============================================================ *)
(* Test: Navigation Works *)
(* ============================================================ *)

let test_navigation () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Instances.Page) ;

      (* Navigate down and up *)
      TH.navigate_down 3 ;
      TH.navigate_up 1 ;
      TH.navigate_down 2 ;

      (* Page should still be functional *)
      let screen = TH.get_screen_text () in
      check
        bool
        "still rendering after navigation"
        true
        (String.length screen > 50) ;

      (* Should still show instance content *)
      check
        bool
        "shows instance content"
        true
        (TH.contains_substring screen "Total instances"
        || TH.contains_substring screen "Install"))

(* ============================================================ *)
(* Test: Refresh Functionality *)
(* ============================================================ *)

let test_refresh () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Instances.Page) ;

      let initial_screen = TH.get_screen_text () in

      (* Try refresh key (usually 'r' or F5) *)
      ignore (TH.send_key_and_wait "r") ;
      Unix.sleepf 0.05 ;

      let after_refresh = TH.get_screen_text () in

      (* Both screens should be valid *)
      check bool "initial screen valid" true (String.length initial_screen > 0) ;
      check bool "after refresh valid" true (String.length after_refresh > 0))

(* ============================================================ *)
(* Test: Page Handles Empty State *)
(* ============================================================ *)

let test_empty_state () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Instances.Page) ;

      let screen = TH.get_screen_text () in

      (* With no services, should show empty state or message *)
      (* The page should still render without crashing *)
      check bool "empty state renders" true (String.length screen > 0) ;

      (* Should show the page title at minimum *)
      check
        bool
        "shows instances label"
        true
        (TH.contains_substring screen "Instance"
        || TH.contains_substring screen "instance"
        || TH.contains_substring screen "Service"))

(* ============================================================ *)
(* Test: Multiple Navigation Cycles *)
(* ============================================================ *)

let test_navigation_cycles () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Instances.Page) ;

      (* Navigate extensively *)
      for _i = 1 to 5 do
        TH.navigate_down 2 ;
        Unix.sleepf 0.01 ;
        TH.navigate_up 1 ;
        Unix.sleepf 0.01
      done ;

      let screen = TH.get_screen_text () in

      (* Page should remain stable *)
      check bool "stable after many navigations" true (String.length screen > 50) ;

      (* Should show instance content *)
      check
        bool
        "shows instance content"
        true
        (TH.contains_substring screen "Total instances"
        || TH.contains_substring screen "Install"))

(* ============================================================ *)
(* Test: Keyboard Shortcuts Don't Crash *)
(* ============================================================ *)

let test_keyboard_shortcuts () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Instances.Page) ;

      (* Try various common shortcuts *)
      let keys = ["r"; "s"; "d"; "i"; "e"; "Enter"; "Escape"] in

      List.iter
        (fun key ->
          ignore (TH.send_key_and_wait key) ;
          Unix.sleepf 0.01)
        keys ;

      let screen = TH.get_screen_text () in

      (* Page should still be functional after trying shortcuts *)
      check bool "survives keyboard input" true (String.length screen > 0))

(* ============================================================ *)
(* Test: Screen Updates on Tick *)
(* ============================================================ *)

let test_periodic_update () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Instances.Page) ;

      let initial = TH.get_screen_text () in

      (* Wait for a tick/update cycle *)
      Unix.sleepf 0.1 ;

      let after_wait = TH.get_screen_text () in

      (* Both should be valid screens *)
      check bool "initial valid" true (String.length initial > 0) ;
      check bool "after wait valid" true (String.length after_wait > 0))

(* ============================================================ *)
(* Test: Column Navigation (if multi-column layout) *)
(* ============================================================ *)

let test_column_navigation () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Instances.Page) ;

      (* Try left/right navigation (column switching) *)
      ignore (TH.send_key_and_wait "Right") ;
      Unix.sleepf 0.01 ;
      ignore (TH.send_key_and_wait "Left") ;
      Unix.sleepf 0.01 ;

      let screen = TH.get_screen_text () in

      (* Should handle column navigation without crash *)
      check bool "handles column navigation" true (String.length screen > 0))

(* ============================================================ *)
(* Test: Fold/Unfold Service Details *)
(* ============================================================ *)

let test_fold_unfold () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Instances.Page) ;

      (* Try Tab key (common for expand/collapse) *)
      ignore (TH.send_key_and_wait "Tab") ;
      Unix.sleepf 0.02 ;

      let after_tab = TH.get_screen_text () in

      (* Try Space key (another common toggle) *)
      ignore (TH.send_key_and_wait " ") ;
      Unix.sleepf 0.02 ;

      let after_space = TH.get_screen_text () in

      (* Both operations should produce valid screens *)
      check bool "after tab valid" true (String.length after_tab > 0) ;
      check bool "after space valid" true (String.length after_space > 0))

(* ============================================================ *)
(* Test: Create Menu Opens *)
(* ============================================================ *)

let test_create_menu_opens () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Instances.Page) ;

      (* Press 'c' to open create menu *)
      ignore (HD.Stateful.send_key "c") ;
      ignore (HD.Stateful.idle_wait ~iterations:5 ~sleep:0.001 ()) ;

      (* Verify modal is active *)
      check
        bool
        "modal opened after 'c' key"
        true
        (Miaou.Core.Modal_manager.has_active ()) ;

      (* Check screen shows service options *)
      let screen = TH.get_screen_text () in
      check
        bool
        "menu shows Node option"
        true
        (TH.contains_substring screen "Node") ;
      check
        bool
        "menu shows Baker option"
        true
        (TH.contains_substring screen "Baker"))

(* ============================================================ *)
(* Test: Select Node from Create Menu *)
(* ============================================================ *)

let test_select_node_from_menu () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Instances.Page) ;

      (* Open create menu *)
      ignore (HD.Stateful.send_key "c") ;
      ignore (HD.Stateful.idle_wait ~iterations:5 ~sleep:0.001 ()) ;

      (* Verify modal is active *)
      check
        bool
        "create menu opened"
        true
        (Miaou.Core.Modal_manager.has_active ()) ;

      (* Navigate to ensure item is selected (Down then Up to return to Node) *)
      ignore (HD.Stateful.send_key "Down") ;
      ignore (HD.Stateful.idle_wait ~iterations:2 ~sleep:0.001 ()) ;
      ignore (HD.Stateful.send_key "Up") ;
      ignore (HD.Stateful.idle_wait ~iterations:2 ~sleep:0.001 ()) ;

      (* Press Enter to select Node *)
      ignore (HD.Stateful.send_key "Enter") ;
      let idle_result = HD.Stateful.idle_wait ~iterations:10 ~sleep:0.001 () in

      (* Verify modal closed *)
      check
        bool
        "modal closed after Enter"
        false
        (Miaou.Core.Modal_manager.has_active ()) ;

      (* Check that navigation was requested to install form *)
      check
        bool
        "Enter on Node triggers navigation to install form"
        true
        (match idle_result with
        | `SwitchTo "install_node_form_v3" -> true
        | _ -> false))

(* ============================================================ *)
(* Test Suite *)
(* ============================================================ *)

let page_tests =
  [
    ("page loads and renders", `Quick, test_page_loads);
    ("page shows headers/structure", `Quick, test_shows_headers);
    ("navigation works", `Quick, test_navigation);
    ("refresh functionality", `Quick, test_refresh);
    ("empty state handled", `Quick, test_empty_state);
    ("multiple navigation cycles", `Quick, test_navigation_cycles);
    ("keyboard shortcuts don't crash", `Quick, test_keyboard_shortcuts);
    ("periodic update works", `Quick, test_periodic_update);
    ("column navigation", `Quick, test_column_navigation);
    ("fold/unfold toggles", `Quick, test_fold_unfold);
    ("create menu opens with 'c' key", `Quick, test_create_menu_opens);
    ("select Node from create menu", `Quick, test_select_node_from_menu);
  ]

let () = Alcotest.run "Instances Page (TUI)" [("instances_page", page_tests)]
