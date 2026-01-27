(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Golden Path TUI Navigation Test
    
    Tests that the TUI can navigate through the service creation workflow:
    1. Open instances page
    2. Open create menu  
    3. Navigate to install forms
    4. Verify forms load correctly
    
    NOTE: This does NOT test actual service creation, which requires:
    - Actual octez binaries
    - Systemd available and working
    - Proper permissions
    
    Full service creation is tested in test/integration/tui-e2e tests that
    run in CI with all dependencies available. *)

module HD = Lib_miaou_internal.Headless_driver
module TH = Tui_test_helpers_lib.Tui_test_helpers
module Instances = Octez_manager_ui.Instances

(* ============================================================ *)
(* Navigation Test *)
(* ============================================================ *)

let test_navigation_workflow () =
  TH.with_test_env (fun () ->
      Printf.eprintf "\n=== TUI Navigation Test ===\n%!" ;

      (* Step 1: Load instances page *)
      Printf.eprintf "[1] Loading instances page...\n%!" ;
      HD.Stateful.init (module Instances.Page) ;
      let screen = TH.get_screen_text () in
      Alcotest.(check bool)
        "instances page loads"
        true
        (TH.contains_substring screen "Total instances") ;
      Printf.eprintf "✓ Instances page loaded\n%!" ;

      (* Step 2: Open create menu *)
      Printf.eprintf "[2] Opening create menu...\n%!" ;
      ignore (HD.Stateful.send_key "c") ;
      ignore (HD.Stateful.idle_wait ~iterations:10 ~sleep:0.001 ()) ;
      Alcotest.(check bool)
        "create menu opens"
        true
        (Miaou.Core.Modal_manager.has_active ()) ;
      let modal_screen = TH.get_screen_text () in
      Alcotest.(check bool)
        "menu shows Node option"
        true
        (TH.contains_substring modal_screen "Node") ;
      Printf.eprintf "✓ Create menu opened\n%!" ;

      (* Step 3: Select Node and navigate to install form *)
      Printf.eprintf "[3] Navigating to node install form...\n%!" ;
      ignore (HD.Stateful.send_key "Enter") ;
      let nav_result = HD.Stateful.idle_wait ~iterations:20 ~sleep:0.001 () in
      (match nav_result with
      | `SwitchTo "install_node_form_v3" ->
          Printf.eprintf "✓ Navigated to install_node_form_v3\n%!"
      | `SwitchTo other ->
          Alcotest.fail
            (Printf.sprintf "Expected install_node_form_v3, got %s" other)
      | `Continue -> Alcotest.fail "Navigation did not occur"
      | `Quit -> Alcotest.fail "Unexpected quit") ;

      (* Step 4: Verify form loads *)
      Printf.eprintf "[4] Verifying node install form loads...\n%!" ;
      HD.Stateful.init (module Octez_manager_ui.Install_node_form_v3.Page) ;
      ignore (HD.Stateful.idle_wait ~iterations:10 ~sleep:0.001 ()) ;
      let form_screen = TH.get_screen_text () in
      Alcotest.(check bool)
        "form title present"
        true
        (TH.contains_substring form_screen "Install Node") ;
      Alcotest.(check bool)
        "network field present"
        true
        (TH.contains_substring form_screen "Network") ;
      Alcotest.(check bool)
        "instance name field present"
        true
        (TH.contains_substring form_screen "Instance Name") ;
      Printf.eprintf "✓ Form loaded with expected fields\n%!" ;

      Printf.eprintf "\n=== Navigation Test PASSED ===\n%!")

let () =
  Alcotest.run
    "Golden Path (TUI)"
    [
      ("navigation", [("workflow navigation", `Quick, test_navigation_workflow)]);
    ]
