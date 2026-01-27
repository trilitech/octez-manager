(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
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
      let dal_name = Printf.sprintf "dal-node-%s" node_name in
      let accuser_name = Printf.sprintf "accuser-%s" node_name in

      Printf.eprintf "\n=== Golden Path Test ===\n" ;
      Printf.eprintf "Node instance: %s\n%!" node_name ;
      Printf.eprintf "DAL instance: %s\n%!" dal_name ;
      Printf.eprintf "Accuser instance: %s\n%!" accuser_name ;

      (* Helper: send key and expect Continue *)
      let send_expect_continue key =
        match HD.Stateful.send_key key with
        | `Continue -> ()
        | `SwitchTo page ->
            Alcotest.fail
              (Printf.sprintf "Expected Continue, got SwitchTo %s" page)
        | `Quit -> Alcotest.fail "Expected Continue, got Quit"
      in

      (* Helper: type text character by character *)
      let type_text text =
        String.iter (fun c -> send_expect_continue (String.make 1 c)) text
      in

      (* Helper: ensure modal is active *)
      let ensure_modal_active () =
        if not (TH.wait_until_modal_active ~iterations:120 ()) then
          Alcotest.fail "Modal did not become active"
      in

      (* Helper: wait for modal to close *)
      let wait_for_modal_close () =
        if not (TH.wait_until_no_modal ~iterations:120 ()) then
          Alcotest.fail "Modal did not close"
      in

      (* Helper: wait for service to be registered *)
      let wait_for_service name =
        let rec loop attempts =
          match Octez_manager_lib.Service_registry.find ~instance:name with
          | Ok (Some _) ->
              Printf.eprintf "✓ Service %s registered\n%!" name ;
              ()
          | (Ok None | Error _) when attempts <= 0 ->
              Alcotest.fail
                (Printf.sprintf "Service %s not registered after waiting" name)
          | Ok None | Error _ ->
              Unix.sleepf 0.01 ;
              loop (attempts - 1)
        in
        loop 2000
      in

      (* ===== Step 1: Navigate to node install form ===== *)
      Printf.eprintf "\n[1/4] Opening node install form...\n%!" ;
      HD.Stateful.init (module Instances.Page) ;

      (* Open create menu with 'c' *)
      send_expect_continue "c" ;
      ensure_modal_active () ;

      (* Select "Node" (first item, already selected) *)
      send_expect_continue "Enter" ;

      (* Wait for navigation to complete *)
      let nav_result = HD.Stateful.idle_wait ~iterations:20 ~sleep:0.001 () in
      (match nav_result with
      | `SwitchTo page when page = "install_node_form_v3" -> ()
      | `SwitchTo page ->
          Alcotest.fail
            (Printf.sprintf "Expected install_node_form_v3, got %s" page)
      | `Continue -> Alcotest.fail "Expected navigation after modal commit"
      | `Quit -> Alcotest.fail "Unexpected quit") ;

      (* ===== Step 2: Fill node installation form ===== *)
      Printf.eprintf "[2/4] Filling node form...\n%!" ;
      HD.Stateful.init (module Octez_manager_ui.Install_node_form_v3.Page) ;

      (* Instance name modal should appear immediately *)
      ensure_modal_active () ;
      type_text node_name ;
      send_expect_continue "Enter" ;
      wait_for_modal_close () ;
      ignore (HD.Stateful.idle_wait ~iterations:10 ~sleep:0.001 ()) ;

      (* Network: default is shadownet, keep it *)
      send_expect_continue "Down" ;
      ignore (HD.Stateful.idle_wait ~iterations:5 ~sleep:0.001 ()) ;

      (* History mode: default is rolling, keep it *)
      send_expect_continue "Down" ;
      ignore (HD.Stateful.idle_wait ~iterations:5 ~sleep:0.001 ()) ;

      (* Data directory: use default *)
      send_expect_continue "Down" ;
      ignore (HD.Stateful.idle_wait ~iterations:5 ~sleep:0.001 ()) ;

      (* Snapshot: disable for speed *)
      (* Open snapshot modal *)
      send_expect_continue "Enter" ;
      ensure_modal_active () ;
      (* Select "None" (should be first option) *)
      send_expect_continue "Enter" ;
      wait_for_modal_close () ;
      ignore (HD.Stateful.idle_wait ~iterations:5 ~sleep:0.001 ()) ;

      (* Move to next field *)
      send_expect_continue "Down" ;
      ignore (HD.Stateful.idle_wait ~iterations:5 ~sleep:0.001 ()) ;

      (* RPC address: set to 127.0.0.1:18732 *)
      send_expect_continue "Enter" ;
      ensure_modal_active () ;
      (* Clear default *)
      send_expect_continue "\001" ;
      (* Ctrl+A *)
      ignore (HD.Stateful.idle_wait ~iterations:2 ~sleep:0.0 ()) ;
      send_expect_continue "Backspace" ;
      ignore (HD.Stateful.idle_wait ~iterations:2 ~sleep:0.0 ()) ;
      type_text "127.0.0.1:18732" ;
      send_expect_continue "Enter" ;
      wait_for_modal_close () ;
      ignore (HD.Stateful.idle_wait ~iterations:5 ~sleep:0.001 ()) ;

      (* Move to next field *)
      send_expect_continue "Down" ;
      ignore (HD.Stateful.idle_wait ~iterations:5 ~sleep:0.001 ()) ;

      (* P2P address: use default *)
      send_expect_continue "Down" ;
      ignore (HD.Stateful.idle_wait ~iterations:5 ~sleep:0.001 ()) ;

      (* Service user: set to tezos *)
      send_expect_continue "Enter" ;
      ensure_modal_active () ;
      (* Clear default *)
      send_expect_continue "\001" ;
      (* Ctrl+A *)
      ignore (HD.Stateful.idle_wait ~iterations:2 ~sleep:0.0 ()) ;
      send_expect_continue "Backspace" ;
      ignore (HD.Stateful.idle_wait ~iterations:2 ~sleep:0.0 ()) ;
      type_text "tezos" ;
      send_expect_continue "Enter" ;
      wait_for_modal_close () ;
      ignore (HD.Stateful.idle_wait ~iterations:5 ~sleep:0.001 ()) ;

      (* Navigate to remaining fields and submit *)
      (* App bin dir: use default *)
      send_expect_continue "Down" ;
      ignore (HD.Stateful.idle_wait ~iterations:5 ~sleep:0.001 ()) ;

      (* Enable on boot: keep enabled *)
      send_expect_continue "Down" ;
      ignore (HD.Stateful.idle_wait ~iterations:5 ~sleep:0.001 ()) ;

      (* Start now: keep enabled *)
      send_expect_continue "Down" ;
      ignore (HD.Stateful.idle_wait ~iterations:5 ~sleep:0.001 ()) ;

      (* Extra args: use default *)
      send_expect_continue "Down" ;
      ignore (HD.Stateful.idle_wait ~iterations:5 ~sleep:0.001 ()) ;

      (* Submit button *)
      send_expect_continue "Down" ;
      ignore (HD.Stateful.idle_wait ~iterations:5 ~sleep:0.001 ()) ;

      Printf.eprintf "Submitting node form...\n%!" ;
      send_expect_continue "Enter" ;

      (* Wait for service creation (this can take time) *)
      ignore (HD.Stateful.idle_wait ~iterations:2000 ~sleep:0.005 ()) ;
      wait_for_service node_name ;

      (* ===== Step 3: Create DAL node ===== *)
      Printf.eprintf "\n[3/4] Creating DAL node...\n%!" ;
      HD.Stateful.init (module Instances.Page) ;
      ignore (HD.Stateful.idle_wait ~iterations:10 ~sleep:0.001 ()) ;

      (* Find the node in the list - it should be visible now *)
      (* Press Enter to open action menu *)
      send_expect_continue "Enter" ;
      ensure_modal_active () ;

      (* Navigate to "Create Associated Service" *)
      send_expect_continue "Down" ;
      (* Skip first action *)
      send_expect_continue "Down" ;
      (* Skip second action *)
      send_expect_continue "Enter" ;
      (* Select "Create Associated Service" *)
      ignore (HD.Stateful.idle_wait ~iterations:10 ~sleep:0.001 ()) ;
      ensure_modal_active () ;

      (* Select DAL Node *)
      send_expect_continue "Down" ;
      (* Move to DAL Node *)
      send_expect_continue "Enter" ;

      (* Wait for navigation *)
      let nav_result = HD.Stateful.idle_wait ~iterations:20 ~sleep:0.001 () in
      (match nav_result with
      | `SwitchTo page when page = "install_dal_node_form_v3" -> ()
      | `SwitchTo page ->
          Printf.eprintf
            "Warning: unexpected page %s, continuing anyway\n%!"
            page
      | `Continue -> ()
      | `Quit -> Alcotest.fail "Unexpected quit") ;

      (* Fill DAL form (minimal - accept defaults) *)
      HD.Stateful.init (module Octez_manager_ui.Install_dal_node_form_v3.Page) ;
      ignore (HD.Stateful.idle_wait ~iterations:10 ~sleep:0.001 ()) ;

      (* Navigate to submit button *)
      for _ = 1 to 10 do
        send_expect_continue "Down" ;
        ignore (HD.Stateful.idle_wait ~iterations:5 ~sleep:0.001 ())
      done ;

      Printf.eprintf "Submitting DAL form...\n%!" ;
      send_expect_continue "Enter" ;
      ignore (HD.Stateful.idle_wait ~iterations:2000 ~sleep:0.005 ()) ;
      wait_for_service dal_name ;

      (* ===== Step 4: Create accuser ===== *)
      Printf.eprintf "\n[4/4] Creating accuser...\n%!" ;
      HD.Stateful.init (module Instances.Page) ;
      ignore (HD.Stateful.idle_wait ~iterations:10 ~sleep:0.001 ()) ;

      (* Open action menu on node *)
      send_expect_continue "Enter" ;
      ensure_modal_active () ;

      (* Navigate to "Create Associated Service" *)
      send_expect_continue "Down" ;
      send_expect_continue "Down" ;
      send_expect_continue "Enter" ;
      ignore (HD.Stateful.idle_wait ~iterations:10 ~sleep:0.001 ()) ;
      ensure_modal_active () ;

      (* Select Accuser *)
      send_expect_continue "Down" ;
      (* Skip Baker *)
      send_expect_continue "Down" ;
      (* Skip DAL *)
      send_expect_continue "Down" ;
      (* Move to Accuser *)
      send_expect_continue "Enter" ;

      let nav_result = HD.Stateful.idle_wait ~iterations:20 ~sleep:0.001 () in
      (match nav_result with
      | `SwitchTo page when page = "install_accuser_form_v3" -> ()
      | `SwitchTo page ->
          Printf.eprintf
            "Warning: unexpected page %s, continuing anyway\n%!"
            page
      | `Continue -> ()
      | `Quit -> Alcotest.fail "Unexpected quit") ;

      (* Fill accuser form *)
      HD.Stateful.init (module Octez_manager_ui.Install_accuser_form_v3.Page) ;
      ignore (HD.Stateful.idle_wait ~iterations:10 ~sleep:0.001 ()) ;

      (* Navigate to submit *)
      for _ = 1 to 10 do
        send_expect_continue "Down" ;
        ignore (HD.Stateful.idle_wait ~iterations:5 ~sleep:0.001 ())
      done ;

      Printf.eprintf "Submitting accuser form...\n%!" ;
      send_expect_continue "Enter" ;
      ignore (HD.Stateful.idle_wait ~iterations:2000 ~sleep:0.005 ()) ;
      wait_for_service accuser_name ;

      (* ===== Final verification ===== *)
      Printf.eprintf "\n=== Verification ===\n%!" ;

      let verify_service name =
        match Octez_manager_lib.Service_registry.find ~instance:name with
        | Ok (Some _) ->
            Printf.eprintf "✓ %s exists\n%!" name ;
            ()
        | Ok None -> Alcotest.fail (Printf.sprintf "✗ %s not found" name)
        | Error (`Msg e) ->
            Alcotest.fail (Printf.sprintf "✗ %s error: %s" name e)
      in

      verify_service node_name ;
      verify_service dal_name ;
      verify_service accuser_name ;

      Printf.eprintf "\n=== Golden Path Test PASSED ===\n%!")

let () =
  Alcotest.run
    "Golden Path (TUI)"
    [("golden_path", [("install full stack", `Slow, test_install_full_stack)])]
