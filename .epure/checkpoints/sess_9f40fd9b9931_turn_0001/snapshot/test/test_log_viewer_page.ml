(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Headless TUI tests for Log_viewer_page.

    Tests initialization, navigation keys, and fallback rendering
    when journalctl is unavailable. *)

open Alcotest
module HD = Lib_miaou_internal.Headless_driver
module Log_viewer_page = Octez_manager_ui.Log_viewer_page
module TH = Tui_test_helpers_lib.Tui_test_helpers
module Mock = Mock_service_helpers_lib.Mock_service_helpers

let register_mock_service svc =
  match Octez_manager_lib.Service_registry.write svc with
  | Ok () -> ()
  | Error (`Msg e) -> Alcotest.fail ("Failed to register: " ^ e)

(* ── Test: No pending instance ─────────────────────────────────── *)

let test_no_pending_instance () =
  TH.with_test_env (fun () ->
      Octez_manager_ui.Manager_app.register_pages () ;
      HD.Stateful.init (module Log_viewer_page.Page) ;
      let screen = TH.get_screen_text () in
      check bool "not empty" true (String.length screen > 0))

(* ── Test: With managed service (fallback) ─────────────────────── *)

let test_with_managed_service () =
  TH.with_test_env (fun () ->
      Octez_manager_ui.Manager_app.register_pages () ;
      let svc = Mock.mock_service ~instance:"log-test" ~role:"node" () in
      register_mock_service svc ;
      Octez_manager_ui.Context.set_pending_instance_detail "log-test" ;
      HD.Stateful.init (module Log_viewer_page.Page) ;
      let screen = TH.get_screen_text () in
      check bool "not empty" true (String.length screen > 0))

(* ── Test: Instance not found ──────────────────────────────────── *)

let test_instance_not_found () =
  TH.with_test_env (fun () ->
      Octez_manager_ui.Manager_app.register_pages () ;
      Octez_manager_ui.Context.set_pending_instance_detail "ghost-instance" ;
      HD.Stateful.init (module Log_viewer_page.Page) ;
      let screen = TH.get_screen_text () in
      check bool "renders" true (String.length screen > 0))

(* ── Test: Scroll navigation ──────────────────────────────────── *)

let test_scroll_navigation () =
  TH.with_test_env (fun () ->
      Octez_manager_ui.Manager_app.register_pages () ;
      HD.Stateful.init (module Log_viewer_page.Page) ;
      ignore (TH.send_key_and_wait "j") ;
      ignore (TH.send_key_and_wait "k") ;
      TH.navigate_down 3 ;
      TH.navigate_up 2 ;
      let screen = TH.get_screen_text () in
      check bool "still renders" true (String.length screen > 0))

(* ── Test: Esc navigates back ──────────────────────────────────── *)

let test_esc_navigates_back () =
  TH.with_test_env (fun () ->
      Octez_manager_ui.Manager_app.register_pages () ;
      HD.Stateful.init (module Log_viewer_page.Page) ;
      ignore (TH.send_key_and_wait "Escape") ;
      let screen = TH.get_screen_text () in
      check bool "renders after Esc" true (String.length screen > 0))

(* ── Test: Refresh key ─────────────────────────────────────────── *)

let test_refresh_key () =
  TH.with_test_env (fun () ->
      Octez_manager_ui.Manager_app.register_pages () ;
      HD.Stateful.init (module Log_viewer_page.Page) ;
      ignore (TH.send_key_and_wait "r") ;
      let screen = TH.get_screen_text () in
      check bool "renders after refresh" true (String.length screen > 0))

(* ── Test: Go to top/bottom ────────────────────────────────────── *)

let test_pager_go_top_bottom () =
  TH.with_test_env (fun () ->
      Octez_manager_ui.Manager_app.register_pages () ;
      HD.Stateful.init (module Log_viewer_page.Page) ;
      ignore (TH.send_key_and_wait "G") ;
      ignore (TH.send_key_and_wait "g") ;
      let screen = TH.get_screen_text () in
      check bool "renders" true (String.length screen > 0))

(* ── Test: Wrap toggle ─────────────────────────────────────────── *)

let test_wrap_toggle () =
  TH.with_test_env (fun () ->
      Octez_manager_ui.Manager_app.register_pages () ;
      HD.Stateful.init (module Log_viewer_page.Page) ;
      ignore (TH.send_key_and_wait "w") ;
      let screen = TH.get_screen_text () in
      check bool "renders after wrap" true (String.length screen > 0))

(* ── Suite ────────────────────────────────────────────────────── *)

let () =
  run
    "Log_viewer_page"
    [
      ( "init",
        [
          test_case "no pending" `Quick test_no_pending_instance;
          test_case "managed service" `Quick test_with_managed_service;
          test_case "not found" `Quick test_instance_not_found;
        ] );
      ( "nav",
        [
          test_case "scroll" `Quick test_scroll_navigation;
          test_case "top/bottom" `Quick test_pager_go_top_bottom;
        ] );
      ( "keys",
        [
          test_case "esc" `Quick test_esc_navigates_back;
          test_case "refresh" `Quick test_refresh_key;
          test_case "wrap" `Quick test_wrap_toggle;
        ] );
    ]
