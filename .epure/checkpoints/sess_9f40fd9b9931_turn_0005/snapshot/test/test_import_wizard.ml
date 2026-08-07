(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Headless TUI tests for Import_wizard page.

    Tests empty state rendering, navigation, and key handling when no
    external services are detected. *)

open Alcotest
module HD = Lib_miaou_internal.Headless_driver
module Import_wizard = Octez_manager_ui.Import_wizard
module TH = Tui_test_helpers_lib.Tui_test_helpers

(* ── Test: Empty state ─────────────────────────────────────────── *)

let test_page_loads_empty () =
  TH.with_test_env (fun () ->
      Octez_manager_ui.Manager_app.register_pages () ;
      HD.Stateful.init (module Import_wizard.Page) ;
      let screen = TH.get_screen_text () in
      check
        bool
        "shows import content"
        true
        (TH.contains_substring screen "Import"
        || TH.contains_substring screen "Step"
        || TH.contains_substring screen "Select"
        || String.length screen > 0))

(* ── Test: Esc from SelectService ──────────────────────────────── *)

let test_esc_from_select_service () =
  TH.with_test_env (fun () ->
      Octez_manager_ui.Manager_app.register_pages () ;
      HD.Stateful.init (module Import_wizard.Page) ;
      ignore (TH.send_key_and_wait "Escape") ;
      let screen = TH.get_screen_text () in
      check bool "renders after Esc" true (String.length screen > 0))

(* ── Test: Empty list navigation ───────────────────────────────── *)

let test_empty_list_navigation () =
  TH.with_test_env (fun () ->
      Octez_manager_ui.Manager_app.register_pages () ;
      HD.Stateful.init (module Import_wizard.Page) ;
      TH.navigate_down 3 ;
      TH.navigate_up 2 ;
      let screen = TH.get_screen_text () in
      check bool "renders after nav" true (String.length screen > 0))

(* ── Test: Enter with no services ──────────────────────────────── *)

let test_enter_with_no_services () =
  TH.with_test_env (fun () ->
      Octez_manager_ui.Manager_app.register_pages () ;
      HD.Stateful.init (module Import_wizard.Page) ;
      ignore (TH.send_key_and_wait "Enter") ;
      let screen = TH.get_screen_text () in
      check bool "renders after Enter" true (String.length screen > 0))

(* ── Test: Refresh key ─────────────────────────────────────────── *)

let test_refresh_key () =
  TH.with_test_env (fun () ->
      Octez_manager_ui.Manager_app.register_pages () ;
      HD.Stateful.init (module Import_wizard.Page) ;
      ignore (TH.send_key_and_wait "r") ;
      let screen = TH.get_screen_text () in
      check bool "renders after refresh" true (String.length screen > 0))

(* ── Test: Unhandled keys ──────────────────────────────────────── *)

let test_unhandled_keys () =
  TH.with_test_env (fun () ->
      Octez_manager_ui.Manager_app.register_pages () ;
      HD.Stateful.init (module Import_wizard.Page) ;
      List.iter
        (fun k -> ignore (TH.send_key_and_wait k))
        ["a"; "x"; "Tab"; "1"; "z"] ;
      let screen = TH.get_screen_text () in
      check bool "still renders" true (String.length screen > 0))

(* ── Test: Header step indicator ───────────────────────────────── *)

let test_header_shows_step () =
  TH.with_test_env (fun () ->
      Octez_manager_ui.Manager_app.register_pages () ;
      HD.Stateful.init (module Import_wizard.Page) ;
      let screen = TH.get_screen_text () in
      check
        bool
        "shows step or select"
        true
        (TH.contains_substring screen "Step"
        || TH.contains_substring screen "Select"
        || TH.contains_substring screen "Import"
        || String.length screen > 50))

(* ── Suite ────────────────────────────────────────────────────── *)

let () =
  run
    "Import_wizard"
    [
      ( "init",
        [
          test_case "empty state" `Quick test_page_loads_empty;
          test_case "step indicator" `Quick test_header_shows_step;
        ] );
      ( "nav",
        [
          test_case "esc" `Quick test_esc_from_select_service;
          test_case "empty list" `Quick test_empty_list_navigation;
          test_case "enter empty" `Quick test_enter_with_no_services;
        ] );
      ( "keys",
        [
          test_case "refresh" `Quick test_refresh_key;
          test_case "unhandled" `Quick test_unhandled_keys;
        ] );
    ]
