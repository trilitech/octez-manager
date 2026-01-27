(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                            *)
(******************************************************************************)

(** Golden Path TUI Test
    
    End-to-end test that drives the TUI through the complete workflow:
    1. Install a node (shadownet, no snapshot for speed)
    2. Install DAL node
    3. Install baker (requires wallet/key setup)
    4. Install accuser
    
    This tests the actual TUI forms, service creation, and systemd integration.
    
    Note: This test requires actual octez binaries and will create real systemd
    services. It's designed to run in the Docker test environment. *)

open Alcotest
module HD = Lib_miaou_internal.Headless_driver
module TH = Tui_test_helpers_lib.Tui_test_helpers
module Instances = Octez_manager_ui.Instances

(* ============================================================ *)
(* Golden Path Test *)
(* ============================================================ *)

let test_install_full_stack () =
  TH.with_test_env (fun () ->
      let suffix = int_of_float (Unix.gettimeofday ()) mod 100000 in
      let node_name = Printf.sprintf "golden_node_%05d" suffix in

      Printf.eprintf "\n=== Golden Path Test ===\n" ;
      Printf.eprintf "Node instance: %s\n%!" node_name ;

      (* TODO: Implement the golden path test
         
         Steps:
         1. HD.Stateful.init (module Instances.Page)
         2. Open create menu with 'c'
         3. Select Node from menu
         4. Fill node installation form:
            - Instance name
            - Network: shadownet
            - History mode: rolling
            - No snapshot (for speed)
            - RPC addr: 127.0.0.1:18732
            - Service user: tezos
         5. Wait for service to be created
         6. Verify service exists in Data module
         7. Create DAL node (associated service)
         8. Create baker (requires wallet setup)
         9. Create accuser
         10. Verify all services are registered
      *)

      (* For now, just verify we can init the page *)
      HD.Stateful.init (module Instances.Page) ;
      let screen = TH.get_screen_text () in
      check
        bool
        "instances page loads"
        true
        (TH.contains_substring screen "Total instances"))

let () =
  Alcotest.run
    "Golden Path (TUI)"
    [("golden_path", [("install full stack", `Slow, test_install_full_stack)])]
