(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Golden Path TUI Test
    
    End-to-end test that uses the headless driver to:
    1. Navigate to node install form
    2. Fill form fields (instance name, disable snapshot for speed)
    3. Submit form and create a node service
    4. Verify service was registered
    
    SAFETY: This test ONLY runs in CI (Docker containers with systemd).
    It will skip when run locally to avoid creating services on your system.
    
    CI Environment Detection:
    - Checks for CI=true or GITHUB_ACTIONS=true environment variable
    - Binaries expected at /usr/local/bin (pre-installed in Docker image)
    
    STATUS: EXPERIMENTAL
    This test makes assumptions about form field positions and validation
    behavior that were discovered during local "die and retry" development.
    It may need adjustments when running in actual CI:
    - Field navigation (Down/Up counts) may differ
    - Validation modals may appear when editing fields
    - Form submission timing may vary
    
    If this test fails in CI, use the "die and retry" approach:
    1. Add Printf.eprintf statements to show screen content at each step
    2. Check what modals actually appear
    3. Adjust navigation and field editing logic accordingly
    
    This test proves the TUI can actually create services programmatically
    using the headless driver, without requiring tmux. *)

module HD = Lib_miaou_internal.Headless_driver
module TH = Tui_test_helpers_lib.Tui_test_helpers
module Instances = Octez_manager_ui.Instances

(* ============================================================ *)
(* Navigation Test *)
(* ============================================================ *)

let test_create_node_service () =
  (* SAFETY: Only run in CI environment *)
  let is_ci =
    match Sys.getenv_opt "CI" with
    | Some "true" -> true
    | _ -> (
        match Sys.getenv_opt "GITHUB_ACTIONS" with
        | Some "true" -> true
        | _ -> false)
  in

  if not is_ci then (
    Printf.eprintf
      "\n\
       ⚠ Skipping golden path test - not in CI environment.\n\
       This test creates actual services and should only run in CI containers.\n\
       Set CI=true environment variable to run locally (at your own risk!).\n\
       %!" ;
    Alcotest.skip ()) ;

  (* Save real HOME before test_env overrides it *)
  let real_home = try Sys.getenv "HOME" with Not_found -> "/root" in

  TH.with_test_env (fun () ->
      let suffix = int_of_float (Unix.gettimeofday ()) mod 100000 in
      let instance = Printf.sprintf "gpnode_%05d" suffix in

      Printf.eprintf "\n=== Golden Path TUI Test (CI Mode) ===\n" ;
      Printf.eprintf "Instance: %s\n%!" instance ;

      (* Check for binaries - CI uses /usr/local/bin, local can use downloaded *)
      Printf.eprintf "\n[Setup] Checking for Octez binaries...\n%!" ;
      let bin_dir =
        let ci_path = "/usr/local/bin" in
        let local_path =
          Filename.concat real_home ".local/share/octez-manager/binaries/v24.1"
        in
        (* Try CI path first *)
        if Sys.file_exists (Filename.concat ci_path "octez-node") then (
          Printf.eprintf "✓ Using CI binaries at: %s\n%!" ci_path ;
          ci_path (* Fall back to local downloaded binaries *))
        else if Sys.file_exists (Filename.concat local_path "octez-node") then (
          Printf.eprintf
            "✓ Using local binaries at: %s\n\
             (Downloaded via: octez-manager binaries download 24.1)\n\
             %!"
            local_path ;
          local_path)
        else (
          Printf.eprintf
            "⚠ Octez binaries not found at:\n\
            \  - %s\n\
            \  - %s\n\
             Try: dune exec -- octez-manager binaries download --no-verify 24.1\n\
             Skipping this test.\n\
             %!"
            ci_path
            local_path ;
          Alcotest.skip ())
      in

      (* Helper functions *)
      let send_continue key =
        match HD.Stateful.send_key key with
        | `Continue -> ()
        | other ->
            Alcotest.fail
              (Printf.sprintf
                 "Expected Continue, got %s"
                 (match other with
                 | `SwitchTo p -> "SwitchTo " ^ p
                 | `Quit -> "Quit"
                 | `Continue -> "Continue"))
      in

      let type_text text =
        String.iter (fun c -> send_continue (String.make 1 c)) text
      in

      (* Step 1: Navigate to install form *)
      Printf.eprintf "\n[1/3] Navigating to node install form...\n%!" ;
      HD.Stateful.init (module Instances.Page) ;
      send_continue "c" ;
      (* open create menu *)
      ignore (HD.Stateful.idle_wait ~iterations:10 ~sleep:0.001 ()) ;
      send_continue "Enter" ;
      (* select Node *)
      let nav = HD.Stateful.idle_wait ~iterations:20 ~sleep:0.001 () in
      (match nav with
      | `SwitchTo "install_node_form_v3" -> ()
      | _ -> Alcotest.fail "Failed to navigate to install form") ;
      Printf.eprintf "✓ Opened install form\n%!" ;

      (* Step 2: Fill and submit form *)
      Printf.eprintf "[2/3] Filling form...\n%!" ;
      HD.Stateful.init (module Octez_manager_ui.Install_node_form_v3.Page) ;
      ignore (HD.Stateful.idle_wait ~iterations:10 ~sleep:0.001 ()) ;

      (* Set App Bin Dir to downloaded binaries *)
      Printf.eprintf "  Setting binary directory to %s\n%!" bin_dir ;
      for _ = 1 to 6 do
        send_continue "Down"
      done ;
      (* Navigate to App Bin Dir field *)
      send_continue "Enter" ;
      ignore (HD.Stateful.idle_wait ~iterations:5 ~sleep:0.001 ()) ;
      send_continue "\001" ;
      (* Ctrl+A *)
      send_continue "Backspace" ;
      type_text bin_dir ;
      send_continue "Enter" ;
      ignore (HD.Stateful.idle_wait ~iterations:10 ~sleep:0.001 ()) ;

      (* Go to Instance Name field (end of form) *)
      Printf.eprintf "  Setting instance name to %s\n%!" instance ;
      send_continue "End" ;
      (* Jump to last field *)
      send_continue "Up" ;
      (* Move up from Confirm to Instance Name *)
      send_continue "Enter" ;
      ignore (HD.Stateful.idle_wait ~iterations:5 ~sleep:0.001 ()) ;
      send_continue "\001" ;
      send_continue "Backspace" ;
      type_text instance ;
      send_continue "Enter" ;
      ignore (HD.Stateful.idle_wait ~iterations:10 ~sleep:0.001 ()) ;

      (* Disable snapshot for speed *)
      Printf.eprintf "  Disabling snapshot for speed\n%!" ;
      send_continue "Home" ;
      for _ = 1 to 2 do
        send_continue "Down"
      done ;
      (* Navigate to Snapshot field *)
      send_continue "Enter" ;
      ignore (HD.Stateful.idle_wait ~iterations:5 ~sleep:0.001 ()) ;
      send_continue "Enter" ;
      (* Select first option (None) *)
      ignore (HD.Stateful.idle_wait ~iterations:10 ~sleep:0.001 ()) ;

      (* Submit *)
      Printf.eprintf "  Submitting form...\n%!" ;
      send_continue "End" ;
      send_continue "Enter" ;
      Printf.eprintf "✓ Form submitted\n%!" ;

      (* Step 3: Wait for service creation *)
      Printf.eprintf "[3/3] Waiting for service to be created...\n%!" ;
      let rec wait_for_service attempts =
        match Octez_manager_lib.Service_registry.find ~instance with
        | Ok (Some svc) ->
            Printf.eprintf "✓ Service created successfully\n%!" ;
            Printf.eprintf "  Instance: %s\n%!" svc.instance ;
            Printf.eprintf "  Role: %s\n%!" svc.role ;
            ()
        | Ok None when attempts <= 0 ->
            Printf.eprintf "✗ Service not found in registry after 30s\n%!" ;
            Alcotest.fail "Service was not created after waiting"
        | Error (`Msg e) when attempts <= 0 ->
            Printf.eprintf "✗ Error checking service: %s\n%!" e ;
            Alcotest.fail "Service lookup failed"
        | Ok None | Error _ ->
            Unix.sleepf 0.1 ;
            wait_for_service (attempts - 1)
      in
      wait_for_service 300 ;

      (* Wait up to 30 seconds *)

      (* Cleanup *)
      Printf.eprintf "\n[Cleanup] Removing test service...\n%!" ;
      (match
         Octez_manager_lib.Removal.remove_service
           ~delete_data_dir:true
           ~instance
           ()
       with
      | Ok () -> Printf.eprintf "✓ Cleanup complete\n%!"
      | Error (`Msg e) -> Printf.eprintf "⚠ Cleanup error: %s\n%!" e) ;

      Printf.eprintf "\n=== Golden Path Test PASSED ===\n%!")

let () =
  Alcotest.run
    "Golden Path (TUI)"
    [
      ("golden_path", [("create node service", `Slow, test_create_node_service)]);
    ]
