(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** TUI tests for the log viewer page header.
    
    Tests that the page header "Log Viewer . {instance}" and "Press Esc to go 
    back" appear consistently. This is a regression test for issue #846 to 
    ensure the log viewer doesn't have the same header bypass problem. *)

open Alcotest
module HD = Lib_miaou_internal.Headless_driver
module TH = Tui_test_helpers_lib.Tui_test_helpers

(* ============================================================ *)
(* Test: Page Shows Header *)
(* ============================================================ *)

let test_page_shows_header () =
  TH.with_test_env (fun () ->
      (* Set a pending instance so log viewer has content *)
      Octez_manager_ui.Context.set_pending_instance_detail "test-instance" ;

      HD.Stateful.init (module Octez_manager_ui.Log_viewer_page.Page) ;

      let screen = TH.get_screen_text () in

      (* Check for page title *)
      check
        bool
        "header shows 'Log Viewer'"
        true
        (TH.contains_substring screen "Log Viewer") ;

      (* Check for help text *)
      check
        bool
        "header shows 'Press Esc to go back'"
        true
        (TH.contains_substring screen "Press Esc to go back"))

(* ============================================================ *)
(* Test: Header Persists After Refresh *)
(* ============================================================ *)

let test_header_persists_after_refresh () =
  TH.with_test_env (fun () ->
      Octez_manager_ui.Context.set_pending_instance_detail "test-instance" ;
      HD.Stateful.init (module Octez_manager_ui.Log_viewer_page.Page) ;

      (* Trigger refresh *)
      ignore (HD.Stateful.send_key "r") ;
      ignore (HD.Stateful.idle_wait ~iterations:10 ~sleep:0.001 ()) ;

      let screen = TH.get_screen_text () in

      check
        bool
        "header visible after refresh"
        true
        (TH.contains_substring screen "Log Viewer") ;

      check
        bool
        "help text visible after refresh"
        true
        (TH.contains_substring screen "Press Esc to go back"))

(* ============================================================ *)
(* Test: Header Visible After Source Toggle *)
(* ============================================================ *)

let test_header_visible_after_source_toggle () =
  TH.with_test_env (fun () ->
      Octez_manager_ui.Context.set_pending_instance_detail "test-instance" ;
      HD.Stateful.init (module Octez_manager_ui.Log_viewer_page.Page) ;

      (* Toggle source (journald <-> daily logs) *)
      ignore (HD.Stateful.send_key "t") ;
      ignore (HD.Stateful.idle_wait ~iterations:10 ~sleep:0.001 ()) ;

      let screen = TH.get_screen_text () in

      check
        bool
        "header visible after source toggle"
        true
        (TH.contains_substring screen "Log Viewer") ;

      check
        bool
        "help text visible after source toggle"
        true
        (TH.contains_substring screen "Press Esc to go back"))

(* ============================================================ *)
(* Test: Header Contains Instance Name *)
(* ============================================================ *)

let test_header_contains_instance_name () =
  TH.with_test_env (fun () ->
      Octez_manager_ui.Context.set_pending_instance_detail "my-node" ;
      HD.Stateful.init (module Octez_manager_ui.Log_viewer_page.Page) ;

      let screen = TH.get_screen_text () in

      (* Check that instance name appears in header *)
      check
        bool
        "header shows instance name"
        true
        (TH.contains_substring screen "my-node"
        || TH.contains_substring screen "My-node"))

(* ============================================================ *)
(* Test Suite *)
(* ============================================================ *)

let header_tests =
  [
    ("page shows header", `Quick, test_page_shows_header);
    ("header persists after refresh", `Quick, test_header_persists_after_refresh);
    ( "header visible after source toggle",
      `Quick,
      test_header_visible_after_source_toggle );
    ("header contains instance name", `Quick, test_header_contains_instance_name);
  ]

let () =
  Alcotest.run
    "Log Viewer Page Header (TUI)"
    [("log_viewer_header", header_tests)]
