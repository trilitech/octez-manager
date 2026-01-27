(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Golden Path TUI Test - V2 with Declarative Commands

    End-to-end test using declarative command list that:
    1. Install a node (async via Job_manager)
    2. Install a DAL node (sync, references the node)
    3. Install a baker (sync, references the node)
    4. Install an accuser (sync, references the node)
    5. Verify all four services appear on the instances page

    Includes built-in regression testing and screenshot debugging.

    SAFETY: This test ONLY runs in CI (Docker containers with systemd).
    It will skip when run locally to avoid creating services on your system. *)

module HD = Lib_miaou_internal.Headless_driver
module TH = Tui_test_helpers_lib.Tui_test_helpers
module Instances = Octez_manager_ui.Instances
module TCR = Tui_command_runner_lib.Tui_command_runner
open TCR

(* ============================================================ *)
(* Helpers *)
(* ============================================================ *)

(** Repeat a key press [n] times. *)
let keys_down n = List.init n (fun _ -> Key "Down")

(** Open the create menu ('c') and select the nth item (0-indexed).
    Menu order: Node=0, DAL Node=1, Baker=2, Accuser=3. *)
let open_create_menu ~menu_index ~target_page =
  [Key "c"; WaitFor [ModalActive; MaxIterations 50]]
  @ keys_down menu_index
  @ [Key "Enter"; WaitFor [PageSwitched target_page; MaxIterations 50]]

(** Select the first node in a node-selection modal.
    For DAL/Accuser: items are [Node ...; Endpoint], node is first.
    Cursor must be on the node selection field. *)
let select_first_node =
  [
    Key "Enter";
    WaitFor [ModalActive; MaxIterations 10];
    Key "Enter";
    (* Select first item: the node *)
    WaitFor [ModalClosed; MaxIterations 10];
  ]

(** Select the second item in a node-selection modal.
    For Baker: items are [External; Node ...], node is second. *)
let select_second_item =
  [
    Key "Enter";
    WaitFor [ModalActive; MaxIterations 10];
    Key "Down";
    Key "Enter";
    WaitFor [ModalClosed; MaxIterations 10];
  ]

(** Navigate to Confirm & Install and press Enter.
    [downs] = number of Down presses from current position to reach Confirm. *)
let submit_form ~downs = keys_down downs @ [Key "Enter"]

(** Wait for sync install to complete and return to instances page.
    Sync forms (DAL, Baker, Accuser) block on send_key until done,
    then the framework navigates to instances via Context.navigate.
    We wait for the instances page hint text to confirm we're there. *)
let wait_for_sync_install =
  [WaitFor [ScreenContains "Hint: c create"; MaxIterations 500]]

(* ============================================================ *)
(* Test Script *)
(* ============================================================ *)

let golden_path_script =
  (* ── Step 1: Install Node ─────────────────────────────────── *)
  [Comment "=== Step 1: Install Node ==="; Screenshot "00_initial_state"]
  @ open_create_menu ~menu_index:0 ~target_page:"install_node_form_v3"
  @ [
      WaitFor [ScreenContains "App Bin Dir"; MaxIterations 10];
      CheckRegression "01_node_form";
      Comment "Navigate to Snapshot (Home, Down x2) and disable it";
      Key "Home";
      Key "Down";
      Key "Down";
      Key "Enter";
      WaitFor [ModalActive; MaxIterations 5];
      Key "Enter";
      (* Select first: None (manual sync) *)
      WaitFor [ModalClosed; MaxIterations 10];
    ]
  (* From Snapshot (row 2), Confirm is row 15 => 13 Downs *)
  @ submit_form ~downs:13
  @ [
      Comment "Node install is async (Job_manager). Wait for instances page.";
      WaitFor [ScreenContains "Hint: c create"; MaxIterations 100];
      Screenshot "02_node_installing";
      Comment "Wait for install to complete (toast appears).";
      WaitFor [ScreenContains "installed successfully"; MaxIterations 6000];
      Screenshot "03_node_installed";
      AssertService "node-shadownet";
    ]
  (* ── Step 2: Install DAL Node ─────────────────────────────── *)
  @ [Comment "=== Step 2: Install DAL Node ==="]
  @ open_create_menu ~menu_index:1 ~target_page:"install_dal_node_form_v3"
  @ [
      WaitFor [ScreenContains "DAL RPC"; MaxIterations 10];
      Comment "Cursor is on Node field (first). Select our node.";
    ]
  @ select_first_node
  @ [Screenshot "04_dal_node_selected"]
  (* DAL form: 10 fields + confirm. Cursor on field 0, need 10 Downs *)
  @ submit_form ~downs:10
  @ wait_for_sync_install
  @ [Screenshot "05_dal_installed"; AssertService "dal-shadownet"]
  (* ── Step 3: Install Baker ────────────────────────────────── *)
  @ [Comment "=== Step 3: Install Baker ==="]
  @ open_create_menu ~menu_index:2 ~target_page:"install_baker_form_v3"
  @ [
      WaitFor [ScreenContains "Liquidity Baking"; MaxIterations 10];
      Comment "Cursor on Parent Node (first). Select our node (second item).";
    ]
  @ select_second_item
  @ [Screenshot "06_baker_node_selected"]
  (* Baker form: 13 fields + confirm. Cursor on field 0, need 13 Downs *)
  @ submit_form ~downs:13
  @ wait_for_sync_install
  @ [Screenshot "07_baker_installed"; AssertService "baker-shadownet"]
  (* ── Step 4: Install Accuser ──────────────────────────────── *)
  @ [Comment "=== Step 4: Install Accuser ==="]
  @ open_create_menu ~menu_index:3 ~target_page:"install_accuser_form_v3"
  @ [
      WaitFor [ScreenContains "Base Dir"; MaxIterations 10];
      Comment "Cursor on Node field (first). Select our node.";
    ]
  @ select_first_node
  @ [Screenshot "08_accuser_node_selected"]
  (* Accuser form: 8 fields + confirm. Cursor on field 0, need 8 Downs *)
  @ submit_form ~downs:8
  @ wait_for_sync_install
  @ [Screenshot "09_accuser_installed"; AssertService "accuser-shadownet"]
  (* ── Step 5: Verify all services on instances page ────────── *)
  @ [
      Comment "=== Step 5: Final Verification ===";
      Comment "Wait for the instances cache to show all 4 managed services.";
      WaitFor
        [
          ScreenMatches
            (fun s ->
              (* Summary says "Managed: 4" when externals exist,
                 or "Total instances: 4" when they don't. *)
              let has str =
                try
                  ignore (Str.search_forward (Str.regexp_string str) s 0) ;
                  true
                with Not_found -> false
              in
              has "Managed: 4" || has "Total instances: 4");
          MaxIterations 1000;
        ];
      Comment
        "Tall single-column view: all services should be listed vertically.";
      Resize (60, 80);
      Screenshot "10_all_services_single_column";
      Assert
        ( (fun s ->
            List.for_all
              (fun name ->
                try
                  ignore (Str.search_forward (Str.regexp_string name) s 0) ;
                  true
                with Not_found -> false)
              [
                "node-shadownet";
                "dal-shadownet";
                "baker-shadownet";
                "accuser-shadownet";
              ]),
          "All four services visible in single-column layout" );
      Comment "Wide multi-column view: roles should split across columns.";
      Resize (50, 160);
      Screenshot "11_all_services_multi_column";
      Assert
        ( (fun s ->
            List.for_all
              (fun name ->
                try
                  ignore (Str.search_forward (Str.regexp_string name) s 0) ;
                  true
                with Not_found -> false)
              [
                "node-shadownet";
                "dal-shadownet";
                "baker-shadownet";
                "accuser-shadownet";
              ]),
          "All four services visible in multi-column layout" );
    ]

(* ============================================================ *)
(* Test Function *)
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
      let instances =
        [
          "accuser-shadownet";
          "baker-shadownet";
          "dal-shadownet";
          "node-shadownet";
        ]
      in

      Printf.eprintf "\n=== Golden Path TUI Test V2 (Declarative) ===\n" ;
      Printf.eprintf "Instances: %s\n%!" (String.concat ", " instances) ;

      (* Check for binaries *)
      Printf.eprintf "\n[Setup] Checking for Octez binaries...\n%!" ;
      let _bin_dir =
        let ci_path = "/usr/local/bin" in
        let local_path =
          Filename.concat real_home ".local/share/octez-manager/binaries/v24.1"
        in
        if Sys.file_exists (Filename.concat ci_path "octez-node") then (
          Printf.eprintf "✓ Using CI binaries at: %s\n%!" ci_path ;
          ci_path)
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

      (* Register all pages so the headless driver can switch between them *)
      Octez_manager_ui.Manager_app.register_pages () ;

      (* Initialize headless driver *)
      HD.Stateful.init (module Instances.Page) ;

      (* Run command script *)
      TCR.run_commands golden_path_script ;

      (* Cleanup - remove in reverse order (dependents first, node last) *)
      Printf.eprintf "\n[Cleanup] Removing test services...\n%!" ;
      List.iter
        (fun instance ->
          match
            Octez_manager_lib.Removal.remove_service
              ~delete_data_dir:true
              ~instance
              ()
          with
          | Ok () -> Printf.eprintf "✓ Removed %s\n%!" instance
          | Error (`Msg e) ->
              Printf.eprintf "⚠ Cleanup error for %s: %s\n%!" instance e)
        instances ;

      Printf.eprintf "\n=== Golden Path Test PASSED ===\n%!")

(* ============================================================ *)
(* Test Runner *)
(* ============================================================ *)

let () =
  (* Wrap test in Eio_main.run to provide fiber runtime for background tasks *)
  Eio_main.run (fun env ->
      Eio.Switch.run (fun sw ->
          (* Initialize fiber runtime so headless driver can execute effects *)
          Miaou_helpers.Fiber_runtime.init ~env ~sw ;

          (* Run Alcotest inside the Eio context *)
          Alcotest.run
            "Golden Path (TUI V2)"
            [
              ( "golden_path",
                [("create node service", `Slow, test_create_node_service)] );
            ]))
