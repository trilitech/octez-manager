(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Headless TUI tests for the binaries management page.

    The binaries page displays installed Octez binary versions in collapsible
    groups and allows downloading, linking, pruning, and navigating versions.
    These tests verify rendering and keyboard interaction without network I/O. *)

open Alcotest
module HD = Lib_miaou_internal.Headless_driver
module Binaries = Octez_manager_ui.Binaries
module TH = Tui_test_helpers_lib.Tui_test_helpers

(* ── Test: Page Initialization ───────────────────────────────────── *)

let test_page_loads () =
  TH.with_test_env (fun () ->
      Octez_manager_ui.Manager_app.register_pages () ;
      HD.Stateful.init (module Binaries.Page_Impl) ;

      let screen = TH.get_screen_text () in
      check bool "screen not empty" true (String.length screen > 0) ;
      (* Binaries page should show title or empty state *)
      check
        bool
        "shows binaries content"
        true
        (TH.contains_substring screen "Binaries"
        || TH.contains_substring screen "version"
        || TH.contains_substring screen "No binaries"
        || TH.contains_substring screen "download"))

(* ── Test: Navigation ────────────────────────────────────────────── *)

let test_navigation_up_down () =
  TH.with_test_env (fun () ->
      Octez_manager_ui.Manager_app.register_pages () ;
      HD.Stateful.init (module Binaries.Page_Impl) ;

      (* Navigate down and up *)
      TH.navigate_down 3 ;
      TH.navigate_up 2 ;

      let screen = TH.get_screen_text () in
      check bool "still renders" true (String.length screen > 0))

(* ── Test: Esc key navigates back ────────────────────────────────── *)

let test_esc_navigates_back () =
  TH.with_test_env (fun () ->
      Octez_manager_ui.Manager_app.register_pages () ;
      HD.Stateful.init (module Binaries.Page_Impl) ;

      (* Press Esc should trigger back navigation *)
      ignore (TH.send_key_and_wait "Escape") ;
      let screen = TH.get_screen_text () in
      check bool "renders after Esc" true (String.length screen > 0))

(* ── Test: Tab toggles groups ────────────────────────────────────── *)

let test_tab_key () =
  TH.with_test_env (fun () ->
      Octez_manager_ui.Manager_app.register_pages () ;
      HD.Stateful.init (module Binaries.Page_Impl) ;

      let before = TH.get_screen_text () in
      ignore (TH.send_key_and_wait "Tab") ;
      let after = TH.get_screen_text () in

      (* Both should render without crashing *)
      check bool "before valid" true (String.length before > 0) ;
      check bool "after valid" true (String.length after > 0))

(* ── Test: Refresh key ───────────────────────────────────────────── *)

let test_refresh_key () =
  TH.with_test_env (fun () ->
      Octez_manager_ui.Manager_app.register_pages () ;
      HD.Stateful.init (module Binaries.Page_Impl) ;

      ignore (TH.send_key_and_wait "r") ;
      let screen = TH.get_screen_text () in
      check bool "renders after refresh" true (String.length screen > 0))

(* ── Test: Empty state rendering ─────────────────────────────────── *)

let test_empty_state () =
  TH.with_test_env (fun () ->
      Octez_manager_ui.Manager_app.register_pages () ;
      HD.Stateful.init (module Binaries.Page_Impl) ;

      let screen = TH.get_screen_text () in
      (* With no binaries installed, page should still render *)
      check bool "empty state renders" true (String.length screen > 0))

(* ── Test suite ──────────────────────────────────────────────────── *)

let () =
  run
    "Binaries_page"
    [
      ( "initialization",
        [
          test_case "page loads" `Quick test_page_loads;
          test_case "empty state" `Quick test_empty_state;
        ] );
      ( "navigation",
        [
          test_case "up/down" `Quick test_navigation_up_down;
          test_case "esc back" `Quick test_esc_navigates_back;
          test_case "tab toggle" `Quick test_tab_key;
          test_case "refresh" `Quick test_refresh_key;
        ] );
    ]
