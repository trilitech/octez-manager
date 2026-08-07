(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Headless TUI tests for Instance_details page.

    Tests initialization with/without pending instance, rendering of
    node/baker fields, and key handling. *)

open Alcotest
module HD = Lib_miaou_internal.Headless_driver
module Instance_details = Octez_manager_ui.Instance_details
module TH = Tui_test_helpers_lib.Tui_test_helpers
module Mock = Mock_service_helpers_lib.Mock_service_helpers

let register_mock_service svc =
  match Octez_manager_lib.Service_registry.write svc with
  | Ok () -> ()
  | Error (`Msg e) -> Alcotest.fail ("Failed to register: " ^ e)

(* ── Test: No pending instance shows error/empty ───────────────── *)

let test_no_pending_instance () =
  TH.with_test_env (fun () ->
      Octez_manager_ui.Manager_app.register_pages () ;
      HD.Stateful.init (module Instance_details.Page) ;
      let screen = TH.get_screen_text () in
      check bool "screen not empty" true (String.length screen > 0))

(* ── Test: With mocked node service ────────────────────────────── *)

let test_with_mocked_node_service () =
  TH.with_test_env (fun () ->
      Octez_manager_ui.Manager_app.register_pages () ;
      let svc =
        Mock.mock_service
          ~instance:"test-details-node"
          ~role:"node"
          ~network:"mainnet"
          ~rpc_addr:"127.0.0.1:8732"
          ~net_addr:"0.0.0.0:9732"
          ()
      in
      register_mock_service svc ;
      Octez_manager_ui.Context.set_pending_instance_detail "test-details-node" ;
      HD.Stateful.init (module Instance_details.Page) ;
      let screen = TH.get_screen_text () in
      check
        bool
        "shows instance name"
        true
        (TH.contains_substring screen "test-details-node"))

(* ── Test: Node role renders correct fields ────────────────────── *)

let test_node_role_fields () =
  TH.with_test_env (fun () ->
      Octez_manager_ui.Manager_app.register_pages () ;
      let svc =
        Mock.mock_service
          ~instance:"node-fields-test"
          ~role:"node"
          ~network:"shadownet"
          ~rpc_addr:"127.0.0.1:8733"
          ()
      in
      register_mock_service svc ;
      Octez_manager_ui.Context.set_pending_instance_detail "node-fields-test" ;
      HD.Stateful.init (module Instance_details.Page) ;
      let screen = TH.get_screen_text () in
      check
        bool
        "shows role or network"
        true
        (TH.contains_substring screen "shadownet"
        || TH.contains_substring screen "Role"
        || TH.contains_substring screen "Network"))

(* ── Test: Baker role fields ───────────────────────────────────── *)

let test_baker_role_fields () =
  TH.with_test_env (fun () ->
      Octez_manager_ui.Manager_app.register_pages () ;
      let svc =
        Mock.mock_service
          ~instance:"baker-fields-test"
          ~role:"baker"
          ~network:"mainnet"
          ()
      in
      register_mock_service svc ;
      Octez_manager_ui.Context.set_pending_instance_detail "baker-fields-test" ;
      HD.Stateful.init (module Instance_details.Page) ;
      let screen = TH.get_screen_text () in
      check bool "shows baker" true (TH.contains_substring screen "baker"))

(* ── Test: Instance not found ──────────────────────────────────── *)

let test_instance_not_found () =
  TH.with_test_env (fun () ->
      Octez_manager_ui.Manager_app.register_pages () ;
      Octez_manager_ui.Context.set_pending_instance_detail "nonexistent-node" ;
      HD.Stateful.init (module Instance_details.Page) ;
      let screen = TH.get_screen_text () in
      check bool "renders something" true (String.length screen > 0))

(* ── Test: Esc navigates back ──────────────────────────────────── *)

let test_esc_navigates_back () =
  TH.with_test_env (fun () ->
      Octez_manager_ui.Manager_app.register_pages () ;
      HD.Stateful.init (module Instance_details.Page) ;
      ignore (TH.send_key_and_wait "Escape") ;
      let screen = TH.get_screen_text () in
      check bool "renders after Esc" true (String.length screen > 0))

(* ── Test: Unhandled keys ignored ──────────────────────────────── *)

let test_unhandled_keys () =
  TH.with_test_env (fun () ->
      Octez_manager_ui.Manager_app.register_pages () ;
      let svc = Mock.mock_service ~instance:"key-test" ~role:"node" () in
      register_mock_service svc ;
      Octez_manager_ui.Context.set_pending_instance_detail "key-test" ;
      HD.Stateful.init (module Instance_details.Page) ;
      List.iter
        (fun k -> ignore (TH.send_key_and_wait k))
        ["a"; "b"; "x"; "1"; "Space"; "Tab"] ;
      let screen = TH.get_screen_text () in
      check bool "still renders" true (String.length screen > 0))

(* ── Test: Render fields formatting ────────────────────────────── *)

let test_render_fields_formatting () =
  TH.with_test_env (fun () ->
      Octez_manager_ui.Manager_app.register_pages () ;
      let svc =
        Mock.mock_service
          ~instance:"format-test"
          ~role:"node"
          ~network:"mainnet"
          ~rpc_addr:"127.0.0.1:8734"
          ~service_user:"tezos"
          ()
      in
      register_mock_service svc ;
      Octez_manager_ui.Context.set_pending_instance_detail "format-test" ;
      HD.Stateful.init (module Instance_details.Page) ;
      let screen = TH.get_screen_text () in
      check
        bool
        "shows Instance"
        true
        (TH.contains_substring screen "Instance"
        || TH.contains_substring screen "format-test"))

(* ── Suite ────────────────────────────────────────────────────── *)

let () =
  run
    "Instance_details"
    [
      ( "init",
        [
          test_case "no pending" `Quick test_no_pending_instance;
          test_case "mocked node" `Quick test_with_mocked_node_service;
          test_case "not found" `Quick test_instance_not_found;
        ] );
      ( "rendering",
        [
          test_case "node fields" `Quick test_node_role_fields;
          test_case "baker fields" `Quick test_baker_role_fields;
          test_case "formatting" `Quick test_render_fields_formatting;
        ] );
      ( "keys",
        [
          test_case "esc" `Quick test_esc_navigates_back;
          test_case "unhandled" `Quick test_unhandled_keys;
        ] );
    ]
