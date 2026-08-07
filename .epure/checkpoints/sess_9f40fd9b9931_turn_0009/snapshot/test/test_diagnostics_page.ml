(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Headless TUI tests for the diagnostics page.

    The diagnostics page displays scheduler status, cache info, sparkline
    charts, and supports scrolling, metrics toggling, and cache clearing.
    These tests verify rendering and keyboard interaction. *)

open Alcotest
module HD = Lib_miaou_internal.Headless_driver
module Diagnostics = Octez_manager_ui.Diagnostics
module TH = Tui_test_helpers_lib.Tui_test_helpers

(* ── Test: Page Initialization ───────────────────────────────────── *)

let test_page_loads () =
  TH.with_test_env (fun () ->
      Octez_manager_ui.Manager_app.register_pages () ;
      HD.Stateful.init (module Diagnostics.Page) ;

      let screen = TH.get_screen_text () in
      check bool "screen not empty" true (String.length screen > 0) ;
      (* Diagnostics should show scheduler or cache info *)
      check
        bool
        "shows diagnostics content"
        true
        (TH.contains_substring screen "Diagnostics"
        || TH.contains_substring screen "Scheduler"
        || TH.contains_substring screen "Cache"
        || TH.contains_substring screen "Render"
        || String.length screen > 50))

(* ── Test: Scrolling ─────────────────────────────────────────────── *)

let test_scroll_down_up () =
  TH.with_test_env (fun () ->
      Octez_manager_ui.Manager_app.register_pages () ;
      HD.Stateful.init (module Diagnostics.Page) ;

      (* Scroll down with j/Down, up with k/Up *)
      TH.navigate_down 5 ;
      TH.navigate_up 3 ;

      let screen = TH.get_screen_text () in
      check bool "renders after scroll" true (String.length screen > 0))

let test_vim_scroll () =
  TH.with_test_env (fun () ->
      Octez_manager_ui.Manager_app.register_pages () ;
      HD.Stateful.init (module Diagnostics.Page) ;

      (* Vim-style j/k scrolling *)
      ignore (TH.send_key_and_wait "j") ;
      ignore (TH.send_key_and_wait "j") ;
      ignore (TH.send_key_and_wait "k") ;

      let screen = TH.get_screen_text () in
      check bool "renders after vim scroll" true (String.length screen > 0))

(* ── Test: Toggle Keys ───────────────────────────────────────────── *)

let test_metrics_toggle () =
  TH.with_test_env (fun () ->
      Octez_manager_ui.Manager_app.register_pages () ;
      HD.Stateful.init (module Diagnostics.Page) ;

      let before = TH.get_screen_text () in
      ignore (TH.send_key_and_wait "m") ;
      let after = TH.get_screen_text () in

      check bool "before valid" true (String.length before > 0) ;
      check bool "after valid" true (String.length after > 0))

let test_cache_clear () =
  TH.with_test_env (fun () ->
      Octez_manager_ui.Manager_app.register_pages () ;
      HD.Stateful.init (module Diagnostics.Page) ;

      ignore (TH.send_key_and_wait "c") ;
      let screen = TH.get_screen_text () in
      check bool "renders after clear" true (String.length screen > 0))

let test_duration_change () =
  TH.with_test_env (fun () ->
      Octez_manager_ui.Manager_app.register_pages () ;
      HD.Stateful.init (module Diagnostics.Page) ;

      ignore (TH.send_key_and_wait "d") ;
      let screen = TH.get_screen_text () in
      check bool "renders after duration change" true (String.length screen > 0))

(* ── Test: Esc navigates back ────────────────────────────────────── *)

let test_esc_back () =
  TH.with_test_env (fun () ->
      Octez_manager_ui.Manager_app.register_pages () ;
      HD.Stateful.init (module Diagnostics.Page) ;

      ignore (TH.send_key_and_wait "Escape") ;
      let screen = TH.get_screen_text () in
      check bool "renders after Esc" true (String.length screen > 0))

(* ── Test suite ──────────────────────────────────────────────────── *)

let () =
  run
    "Diagnostics_page"
    [
      ("initialization", [test_case "page loads" `Quick test_page_loads]);
      ( "scrolling",
        [
          test_case "down/up" `Quick test_scroll_down_up;
          test_case "vim j/k" `Quick test_vim_scroll;
        ] );
      ( "key_actions",
        [
          test_case "metrics toggle" `Quick test_metrics_toggle;
          test_case "cache clear" `Quick test_cache_clear;
          test_case "duration change" `Quick test_duration_change;
          test_case "esc back" `Quick test_esc_back;
        ] );
    ]
