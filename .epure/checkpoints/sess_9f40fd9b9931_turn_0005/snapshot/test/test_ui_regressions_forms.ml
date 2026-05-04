(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** UI Regression Tests for Installation Forms
    
    Tests that capture pixel-perfect screenshots of all installation forms
    and detect any visual regressions.
*)

module URF = Ui_regression_framework_lib.Ui_regression_framework
module DM = Ui_regression_framework_lib.Deterministic_mocks
module HD = Lib_miaou_internal.Headless_driver
module TH = Tui_test_helpers_lib.Tui_test_helpers
module Install_node_form = Octez_manager_ui.Install_node_form_v3

(* ============================================================ *)
(* Install Node Form Tests *)
(* ============================================================ *)

(** Test install node form - initial empty state *)
let test_install_node_empty () =
  TH.with_test_env (fun () ->
      DM.setup_deterministic_env () ;

      (* Initialize the form - this renders it automatically *)
      HD.Stateful.init (module Install_node_form.Page) ;

      (* Capture the initial state *)
      URF.assert_ui_regression "install_node_empty")

(** Test install node form - after typing instance name *)
let test_install_node_name_filled () =
  TH.with_test_env (fun () ->
      DM.setup_deterministic_env () ;

      HD.Stateful.init (module Install_node_form.Page) ;

      (* Type instance name *)
      TH.type_string "test-node" ;

      URF.assert_ui_regression "install_node_name_filled")

(** Test install node form - navigated to network field *)
let test_install_node_network_focus () =
  TH.with_test_env (fun () ->
      DM.setup_deterministic_env () ;

      HD.Stateful.init (module Install_node_form.Page) ;

      (* Navigate to network field (Tab) *)
      ignore (TH.send_key_and_wait "Tab") ;

      URF.assert_ui_regression "install_node_network_focus")

(** Test install node form - network dropdown opened *)
let test_install_node_network_dropdown () =
  TH.with_test_env (fun () ->
      DM.setup_deterministic_env () ;

      HD.Stateful.init (module Install_node_form.Page) ;

      (* Navigate to network and open dropdown *)
      ignore (TH.send_key_and_wait "Tab") ;
      ignore (TH.send_key_and_wait "Enter") ;

      URF.assert_ui_regression "install_node_network_dropdown")

(* ============================================================ *)
(* Test Suite *)
(* ============================================================ *)

let install_node_tests =
  [
    ("empty_form", `Quick, test_install_node_empty);
    ("name_filled", `Quick, test_install_node_name_filled);
    ("network_focus", `Quick, test_install_node_network_focus);
    ("network_dropdown", `Quick, test_install_node_network_dropdown);
  ]

let () =
  Alcotest.run "UI_Regressions_Forms" [("install_node", install_node_tests)]
