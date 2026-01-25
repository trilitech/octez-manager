(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Comprehensive UI Regression Tests for All Installation Forms
    
    This file contains extensive regression tests covering all states
    of all installation forms to maximize UI code coverage.
*)

module URF = Ui_regression_framework_lib.Ui_regression_framework
module DM = Ui_regression_framework_lib.Deterministic_mocks
module HD = Lib_miaou_internal.Headless_driver
module TH = Tui_test_helpers_lib.Tui_test_helpers
module Install_node_form = Octez_manager_ui.Install_node_form_v3
module Install_baker_form = Octez_manager_ui.Install_baker_form_v3
module Install_accuser_form = Octez_manager_ui.Install_accuser_form_v3
module Install_dal_node_form = Octez_manager_ui.Install_dal_node_form_v3

(* ============================================================ *)
(* Install Node Form - Comprehensive Tests *)
(* ============================================================ *)

(** Test: Instance name field - various inputs *)
let test_node_instance_name_variations () =
  TH.with_test_env (fun () ->
      DM.setup_deterministic_env () ;

      HD.Stateful.init (module Install_node_form.Page) ;
      TH.type_string "my" ;
      URF.assert_ui_regression "node_instance_name_short")

let test_node_instance_name_long () =
  TH.with_test_env (fun () ->
      DM.setup_deterministic_env () ;

      HD.Stateful.init (module Install_node_form.Page) ;
      TH.type_string "mainnet-production-node-01" ;
      URF.assert_ui_regression "node_instance_name_long")

let test_node_instance_name_with_numbers () =
  TH.with_test_env (fun () ->
      DM.setup_deterministic_env () ;

      HD.Stateful.init (module Install_node_form.Page) ;
      TH.type_string "node-123" ;
      URF.assert_ui_regression "node_instance_name_numbers")

(** Test: Network field - different selections *)
let test_node_network_mainnet () =
  TH.with_test_env (fun () ->
      DM.setup_deterministic_env () ;

      HD.Stateful.init (module Install_node_form.Page) ;
      ignore (TH.send_key_and_wait "Tab") ;
      ignore (TH.send_key_and_wait "Enter") ;
      ignore (TH.send_key_and_wait "Down") ;
      ignore (TH.send_key_and_wait "Enter") ;
      URF.assert_ui_regression "node_network_mainnet")

let test_node_network_ghostnet () =
  TH.with_test_env (fun () ->
      DM.setup_deterministic_env () ;

      HD.Stateful.init (module Install_node_form.Page) ;
      ignore (TH.send_key_and_wait "Tab") ;
      ignore (TH.send_key_and_wait "Enter") ;
      ignore (TH.send_key_and_wait "Down") ;
      ignore (TH.send_key_and_wait "Down") ;
      ignore (TH.send_key_and_wait "Enter") ;
      URF.assert_ui_regression "node_network_ghostnet")

(** Test: History mode field *)
let test_node_history_mode_focus () =
  TH.with_test_env (fun () ->
      DM.setup_deterministic_env () ;

      HD.Stateful.init (module Install_node_form.Page) ;
      ignore (TH.send_key_and_wait "Tab") ;
      ignore (TH.send_key_and_wait "Tab") ;
      URF.assert_ui_regression "node_history_mode_focus")

let test_node_history_mode_full () =
  TH.with_test_env (fun () ->
      DM.setup_deterministic_env () ;

      HD.Stateful.init (module Install_node_form.Page) ;
      ignore (TH.send_key_and_wait "Tab") ;
      ignore (TH.send_key_and_wait "Tab") ;
      ignore (TH.send_key_and_wait "Enter") ;
      ignore (TH.send_key_and_wait "Down") ;
      ignore (TH.send_key_and_wait "Enter") ;
      URF.assert_ui_regression "node_history_mode_full")

let test_node_history_mode_archive () =
  TH.with_test_env (fun () ->
      DM.setup_deterministic_env () ;

      HD.Stateful.init (module Install_node_form.Page) ;
      ignore (TH.send_key_and_wait "Tab") ;
      ignore (TH.send_key_and_wait "Tab") ;
      ignore (TH.send_key_and_wait "Enter") ;
      ignore (TH.send_key_and_wait "Down") ;
      ignore (TH.send_key_and_wait "Down") ;
      ignore (TH.send_key_and_wait "Enter") ;
      URF.assert_ui_regression "node_history_mode_archive")

(** Test: Snapshot import field *)
let test_node_snapshot_focus () =
  TH.with_test_env (fun () ->
      DM.setup_deterministic_env () ;

      HD.Stateful.init (module Install_node_form.Page) ;
      ignore (TH.send_key_and_wait "Tab") ;
      ignore (TH.send_key_and_wait "Tab") ;
      ignore (TH.send_key_and_wait "Tab") ;
      URF.assert_ui_regression "node_snapshot_focus")

let test_node_snapshot_disabled () =
  TH.with_test_env (fun () ->
      DM.setup_deterministic_env () ;

      HD.Stateful.init (module Install_node_form.Page) ;
      ignore (TH.send_key_and_wait "Tab") ;
      ignore (TH.send_key_and_wait "Tab") ;
      ignore (TH.send_key_and_wait "Tab") ;
      ignore (TH.send_key_and_wait "Enter") ;
      ignore (TH.send_key_and_wait "Down") ;
      ignore (TH.send_key_and_wait "Enter") ;
      URF.assert_ui_regression "node_snapshot_disabled")

(** Test: Data directory field *)
let test_node_data_dir_focus () =
  TH.with_test_env (fun () ->
      DM.setup_deterministic_env () ;

      HD.Stateful.init (module Install_node_form.Page) ;
      for _ = 1 to 4 do
        ignore (TH.send_key_and_wait "Tab")
      done ;
      URF.assert_ui_regression "node_data_dir_focus")

let test_node_data_dir_custom () =
  TH.with_test_env (fun () ->
      DM.setup_deterministic_env () ;

      HD.Stateful.init (module Install_node_form.Page) ;
      for _ = 1 to 4 do
        ignore (TH.send_key_and_wait "Tab")
      done ;
      ignore (TH.send_key_and_wait "Enter") ;
      TH.type_string "/data/custom-node" ;
      ignore (TH.send_key_and_wait "Enter") ;
      URF.assert_ui_regression "node_data_dir_custom")

(** Test: RPC address field *)
let test_node_rpc_addr_focus () =
  TH.with_test_env (fun () ->
      DM.setup_deterministic_env () ;

      HD.Stateful.init (module Install_node_form.Page) ;
      for _ = 1 to 5 do
        ignore (TH.send_key_and_wait "Tab")
      done ;
      URF.assert_ui_regression "node_rpc_addr_focus")

let test_node_rpc_addr_custom () =
  TH.with_test_env (fun () ->
      DM.setup_deterministic_env () ;

      HD.Stateful.init (module Install_node_form.Page) ;
      for _ = 1 to 5 do
        ignore (TH.send_key_and_wait "Tab")
      done ;
      ignore (TH.send_key_and_wait "Enter") ;
      TH.type_string "0.0.0.0:9732" ;
      ignore (TH.send_key_and_wait "Enter") ;
      URF.assert_ui_regression "node_rpc_addr_custom")

(** Test: P2P address field *)
let test_node_p2p_addr_focus () =
  TH.with_test_env (fun () ->
      DM.setup_deterministic_env () ;

      HD.Stateful.init (module Install_node_form.Page) ;
      for _ = 1 to 6 do
        ignore (TH.send_key_and_wait "Tab")
      done ;
      URF.assert_ui_regression "node_p2p_addr_focus")

(** Test: Service user field *)
let test_node_service_user_focus () =
  TH.with_test_env (fun () ->
      DM.setup_deterministic_env () ;

      HD.Stateful.init (module Install_node_form.Page) ;
      for _ = 1 to 7 do
        ignore (TH.send_key_and_wait "Tab")
      done ;
      URF.assert_ui_regression "node_service_user_focus")

(** Test: Bootstrap threshold field *)
let test_node_bootstrap_threshold_focus () =
  TH.with_test_env (fun () ->
      DM.setup_deterministic_env () ;

      HD.Stateful.init (module Install_node_form.Page) ;
      for _ = 1 to 8 do
        ignore (TH.send_key_and_wait "Tab")
      done ;
      URF.assert_ui_regression "node_bootstrap_threshold_focus")

(** Test: Enable service toggle *)
let test_node_enable_service_focus () =
  TH.with_test_env (fun () ->
      DM.setup_deterministic_env () ;

      HD.Stateful.init (module Install_node_form.Page) ;
      for _ = 1 to 9 do
        ignore (TH.send_key_and_wait "Tab")
      done ;
      URF.assert_ui_regression "node_enable_service_focus")

(** Test: Navigation through all fields *)
let test_node_complete_navigation () =
  TH.with_test_env (fun () ->
      DM.setup_deterministic_env () ;

      HD.Stateful.init (module Install_node_form.Page) ;
      for _ = 1 to 10 do
        ignore (TH.send_key_and_wait "Tab")
      done ;
      URF.assert_ui_regression "node_complete_navigation")

(* ============================================================ *)
(* Install Baker Form - Comprehensive Tests *)
(* ============================================================ *)

let test_baker_empty () =
  TH.with_test_env (fun () ->
      DM.setup_deterministic_env () ;

      HD.Stateful.init (module Install_baker_form.Page) ;
      URF.assert_ui_regression "baker_empty")

let test_baker_instance_name_filled () =
  TH.with_test_env (fun () ->
      DM.setup_deterministic_env () ;

      HD.Stateful.init (module Install_baker_form.Page) ;
      TH.type_string "my-baker" ;
      URF.assert_ui_regression "baker_instance_name_filled")

let test_baker_node_instance_focus () =
  TH.with_test_env (fun () ->
      DM.setup_deterministic_env () ;

      HD.Stateful.init (module Install_baker_form.Page) ;
      ignore (TH.send_key_and_wait "Tab") ;
      URF.assert_ui_regression "baker_node_instance_focus")

let test_baker_protocol_focus () =
  TH.with_test_env (fun () ->
      DM.setup_deterministic_env () ;

      HD.Stateful.init (module Install_baker_form.Page) ;
      ignore (TH.send_key_and_wait "Tab") ;
      ignore (TH.send_key_and_wait "Tab") ;
      URF.assert_ui_regression "baker_protocol_focus")

let test_baker_delegate_focus () =
  TH.with_test_env (fun () ->
      DM.setup_deterministic_env () ;

      HD.Stateful.init (module Install_baker_form.Page) ;
      for _ = 1 to 3 do
        ignore (TH.send_key_and_wait "Tab")
      done ;
      URF.assert_ui_regression "baker_delegate_focus")

let test_baker_liquidity_toggle_focus () =
  TH.with_test_env (fun () ->
      DM.setup_deterministic_env () ;

      HD.Stateful.init (module Install_baker_form.Page) ;
      for _ = 1 to 4 do
        ignore (TH.send_key_and_wait "Tab")
      done ;
      URF.assert_ui_regression "baker_liquidity_toggle_focus")

let test_baker_dal_node_focus () =
  TH.with_test_env (fun () ->
      DM.setup_deterministic_env () ;

      HD.Stateful.init (module Install_baker_form.Page) ;
      for _ = 1 to 5 do
        ignore (TH.send_key_and_wait "Tab")
      done ;
      URF.assert_ui_regression "baker_dal_node_focus")

let test_baker_service_user_focus () =
  TH.with_test_env (fun () ->
      DM.setup_deterministic_env () ;

      HD.Stateful.init (module Install_baker_form.Page) ;
      for _ = 1 to 6 do
        ignore (TH.send_key_and_wait "Tab")
      done ;
      URF.assert_ui_regression "baker_service_user_focus")

let test_baker_enable_service_focus () =
  TH.with_test_env (fun () ->
      DM.setup_deterministic_env () ;

      HD.Stateful.init (module Install_baker_form.Page) ;
      for _ = 1 to 7 do
        ignore (TH.send_key_and_wait "Tab")
      done ;
      URF.assert_ui_regression "baker_enable_service_focus")

(* ============================================================ *)
(* Install Accuser Form - Comprehensive Tests *)
(* ============================================================ *)

let test_accuser_empty () =
  TH.with_test_env (fun () ->
      DM.setup_deterministic_env () ;

      HD.Stateful.init (module Install_accuser_form.Page) ;
      URF.assert_ui_regression "accuser_empty")

let test_accuser_instance_name_filled () =
  TH.with_test_env (fun () ->
      DM.setup_deterministic_env () ;

      HD.Stateful.init (module Install_accuser_form.Page) ;
      TH.type_string "my-accuser" ;
      URF.assert_ui_regression "accuser_instance_name_filled")

let test_accuser_node_instance_focus () =
  TH.with_test_env (fun () ->
      DM.setup_deterministic_env () ;

      HD.Stateful.init (module Install_accuser_form.Page) ;
      ignore (TH.send_key_and_wait "Tab") ;
      URF.assert_ui_regression "accuser_node_instance_focus")

let test_accuser_protocol_focus () =
  TH.with_test_env (fun () ->
      DM.setup_deterministic_env () ;

      HD.Stateful.init (module Install_accuser_form.Page) ;
      ignore (TH.send_key_and_wait "Tab") ;
      ignore (TH.send_key_and_wait "Tab") ;
      URF.assert_ui_regression "accuser_protocol_focus")

let test_accuser_service_user_focus () =
  TH.with_test_env (fun () ->
      DM.setup_deterministic_env () ;

      HD.Stateful.init (module Install_accuser_form.Page) ;
      for _ = 1 to 3 do
        ignore (TH.send_key_and_wait "Tab")
      done ;
      URF.assert_ui_regression "accuser_service_user_focus")

let test_accuser_enable_service_focus () =
  TH.with_test_env (fun () ->
      DM.setup_deterministic_env () ;

      HD.Stateful.init (module Install_accuser_form.Page) ;
      for _ = 1 to 4 do
        ignore (TH.send_key_and_wait "Tab")
      done ;
      URF.assert_ui_regression "accuser_enable_service_focus")

(* ============================================================ *)
(* Install DAL Node Form - Comprehensive Tests *)
(* ============================================================ *)

let test_dal_empty () =
  TH.with_test_env (fun () ->
      DM.setup_deterministic_env () ;

      HD.Stateful.init (module Install_dal_node_form.Page) ;
      URF.assert_ui_regression "dal_empty")

let test_dal_instance_name_filled () =
  TH.with_test_env (fun () ->
      DM.setup_deterministic_env () ;

      HD.Stateful.init (module Install_dal_node_form.Page) ;
      TH.type_string "my-dal" ;
      URF.assert_ui_regression "dal_instance_name_filled")

let test_dal_node_instance_focus () =
  TH.with_test_env (fun () ->
      DM.setup_deterministic_env () ;

      HD.Stateful.init (module Install_dal_node_form.Page) ;
      ignore (TH.send_key_and_wait "Tab") ;
      URF.assert_ui_regression "dal_node_instance_focus")

let test_dal_network_focus () =
  TH.with_test_env (fun () ->
      DM.setup_deterministic_env () ;

      HD.Stateful.init (module Install_dal_node_form.Page) ;
      ignore (TH.send_key_and_wait "Tab") ;
      ignore (TH.send_key_and_wait "Tab") ;
      URF.assert_ui_regression "dal_network_focus")

let test_dal_profile_focus () =
  TH.with_test_env (fun () ->
      DM.setup_deterministic_env () ;

      HD.Stateful.init (module Install_dal_node_form.Page) ;
      for _ = 1 to 3 do
        ignore (TH.send_key_and_wait "Tab")
      done ;
      URF.assert_ui_regression "dal_profile_focus")

let test_dal_rpc_addr_focus () =
  TH.with_test_env (fun () ->
      DM.setup_deterministic_env () ;

      HD.Stateful.init (module Install_dal_node_form.Page) ;
      for _ = 1 to 4 do
        ignore (TH.send_key_and_wait "Tab")
      done ;
      URF.assert_ui_regression "dal_rpc_addr_focus")

let test_dal_service_user_focus () =
  TH.with_test_env (fun () ->
      DM.setup_deterministic_env () ;

      HD.Stateful.init (module Install_dal_node_form.Page) ;
      for _ = 1 to 5 do
        ignore (TH.send_key_and_wait "Tab")
      done ;
      URF.assert_ui_regression "dal_service_user_focus")

let test_dal_enable_service_focus () =
  TH.with_test_env (fun () ->
      DM.setup_deterministic_env () ;

      HD.Stateful.init (module Install_dal_node_form.Page) ;
      for _ = 1 to 6 do
        ignore (TH.send_key_and_wait "Tab")
      done ;
      URF.assert_ui_regression "dal_enable_service_focus")

(* ============================================================ *)
(* Test Suites *)
(* ============================================================ *)

let node_form_tests =
  [
    ("instance_name_short", `Quick, test_node_instance_name_variations);
    ("instance_name_long", `Quick, test_node_instance_name_long);
    ("instance_name_numbers", `Quick, test_node_instance_name_with_numbers);
    ("network_mainnet", `Quick, test_node_network_mainnet);
    ("network_ghostnet", `Quick, test_node_network_ghostnet);
    ("history_mode_focus", `Quick, test_node_history_mode_focus);
    ("history_mode_full", `Quick, test_node_history_mode_full);
    ("history_mode_archive", `Quick, test_node_history_mode_archive);
    ("snapshot_focus", `Quick, test_node_snapshot_focus);
    ("snapshot_disabled", `Quick, test_node_snapshot_disabled);
    ("data_dir_focus", `Quick, test_node_data_dir_focus);
    ("data_dir_custom", `Quick, test_node_data_dir_custom);
    ("rpc_addr_focus", `Quick, test_node_rpc_addr_focus);
    ("rpc_addr_custom", `Quick, test_node_rpc_addr_custom);
    ("p2p_addr_focus", `Quick, test_node_p2p_addr_focus);
    ("service_user_focus", `Quick, test_node_service_user_focus);
    ("bootstrap_threshold_focus", `Quick, test_node_bootstrap_threshold_focus);
    ("enable_service_focus", `Quick, test_node_enable_service_focus);
    ("complete_navigation", `Quick, test_node_complete_navigation);
  ]

let baker_form_tests =
  [
    ("empty", `Quick, test_baker_empty);
    ("instance_name_filled", `Quick, test_baker_instance_name_filled);
    ("node_instance_focus", `Quick, test_baker_node_instance_focus);
    ("protocol_focus", `Quick, test_baker_protocol_focus);
    ("delegate_focus", `Quick, test_baker_delegate_focus);
    ("liquidity_toggle_focus", `Quick, test_baker_liquidity_toggle_focus);
    ("dal_node_focus", `Quick, test_baker_dal_node_focus);
    ("service_user_focus", `Quick, test_baker_service_user_focus);
    ("enable_service_focus", `Quick, test_baker_enable_service_focus);
  ]

let accuser_form_tests =
  [
    ("empty", `Quick, test_accuser_empty);
    ("instance_name_filled", `Quick, test_accuser_instance_name_filled);
    ("node_instance_focus", `Quick, test_accuser_node_instance_focus);
    ("protocol_focus", `Quick, test_accuser_protocol_focus);
    ("service_user_focus", `Quick, test_accuser_service_user_focus);
    ("enable_service_focus", `Quick, test_accuser_enable_service_focus);
  ]

let dal_form_tests =
  [
    ("empty", `Quick, test_dal_empty);
    ("instance_name_filled", `Quick, test_dal_instance_name_filled);
    ("node_instance_focus", `Quick, test_dal_node_instance_focus);
    ("network_focus", `Quick, test_dal_network_focus);
    ("profile_focus", `Quick, test_dal_profile_focus);
    ("rpc_addr_focus", `Quick, test_dal_rpc_addr_focus);
    ("service_user_focus", `Quick, test_dal_service_user_focus);
    ("enable_service_focus", `Quick, test_dal_enable_service_focus);
  ]

let () =
  Alcotest.run
    "UI_Regressions_Forms_Comprehensive"
    [
      ("install_node", node_form_tests);
      ("install_baker", baker_form_tests);
      ("install_accuser", accuser_form_tests);
      ("install_dal", dal_form_tests);
    ]
