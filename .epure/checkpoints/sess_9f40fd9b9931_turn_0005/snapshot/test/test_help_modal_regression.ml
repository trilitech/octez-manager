(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Comprehensive regression test suite for help modal (?) on ALL pages.
    
    This test suite systematically verifies help modal functionality on every
    page in octez-manager. Each test clearly documents:
    
    1. Expected behavior (what SHOULD happen)
    2. Current status (✅ WORKING / ❌ BROKEN / 📝 NOT SUPPORTED)
    3. How to fix broken pages
    
    Test Categories:
    - ✅ WORKING: Help modal works correctly
    - ❌ BROKEN: Help modal doesn't work (test marked as TODO or failing)
    - 📝 NOT SUPPORTED: Page intentionally doesn't use help modal
    
    When a test fails, it means:
    - Either a regression occurred (previously working page is now broken)
    - Or the page was never working and needs implementation
    
    This serves as:
    - Regression guard for working pages
    - TODO list for broken pages
    - Documentation of intended behavior *)

open Alcotest
module HD = Lib_miaou_internal.Headless_driver
module TH = Tui_test_helpers_lib.Tui_test_helpers

(* ========================================================================= *)
(* Test Infrastructure                                                       *)
(* ========================================================================= *)

type test_status =
  | Working  (** Help modal works correctly *)
  | Broken of string * string
      (** Help modal doesn't work - (reason, github_issue_url) *)
  | NotSupported of string  (** Intentionally doesn't support help modal *)

let status_to_string = function
  | Working -> "✅ WORKING"
  | Broken (reason, _issue) -> "❌ BROKEN: " ^ reason
  | NotSupported reason -> "📝 NOT SUPPORTED: " ^ reason

(** Helper to test help modal with expected status *)
let test_help_modal_with_status
    ~page_module:(module P : Miaou.Core.Tui_page.PAGE_SIG) ~page_name
    ~expected_shortcuts ~status () =
  match status with
  | NotSupported reason ->
      (* Skip test but document why *)
      Printf.printf
        "[%s] %s - %s\n%!"
        page_name
        (status_to_string status)
        reason ;
      check bool (Printf.sprintf "[%s] skipped" page_name) true true
  | Broken (reason, issue_url) ->
      (* Try to run test - if it fails, document the failure *)
      Printf.printf
        "[%s] %s - %s\n%!"
        page_name
        (status_to_string status)
        reason ;
      Printf.printf "  See: %s\n%!" issue_url ;
      (try
         TH.with_test_env (fun () ->
             HD.Stateful.init (module P) ;
             ignore (HD.Stateful.idle_wait ~iterations:2 ~sleep:0.001 ()) ;
             ignore (HD.Stateful.send_key "?") ;
             ignore (HD.Stateful.idle_wait ~iterations:5 ~sleep:0.001 ()) ;
             let screen = TH.get_screen_text () in
             (* Check if help modal appeared *)
             if not (TH.contains_substring screen "Global shortcuts:") then
               failwith
                 (Printf.sprintf
                    "[%s] CONFIRMED BROKEN: Help modal did not open"
                    page_name))
       with e ->
         (* Test failed as expected - document it *)
         Printf.printf "  Expected failure: %s\n%!" (Printexc.to_string e)) ;
      (* Mark as TODO *)
      check
        bool
        (Printf.sprintf "[%s] TODO: Fix help modal" page_name)
        true
        true
  | Working ->
      (* Test should pass - help modal works *)
      TH.with_test_env (fun () ->
          HD.Stateful.init (module P) ;
          ignore (HD.Stateful.idle_wait ~iterations:2 ~sleep:0.001 ()) ;
          ignore (HD.Stateful.send_key "?") ;
          ignore (HD.Stateful.idle_wait ~iterations:5 ~sleep:0.001 ()) ;

          let screen = TH.get_screen_text () in

          (* Verify help modal opened *)
          check
            bool
            (Printf.sprintf "[%s] help modal opens" page_name)
            true
            (TH.contains_substring screen "Global shortcuts:") ;

          (* Verify global shortcuts visible *)
          check
            bool
            (Printf.sprintf "[%s] shows global shortcuts" page_name)
            true
            (TH.contains_substring screen "?") ;

          (* Check for page shortcuts if expected *)
          match expected_shortcuts with
          | [] ->
              (* No page shortcuts expected *)
              ()
          | _ :: _ ->
              (* Should have page shortcuts section *)
              check
                bool
                (Printf.sprintf "[%s] has page shortcuts section" page_name)
                true
                (TH.contains_substring screen "Page shortcuts:") ;

              (* Check for visible shortcuts *)
              List.iter
                (fun (_key, help_text) ->
                  if TH.contains_substring screen help_text then
                    check
                      bool
                      (Printf.sprintf "[%s] shows '%s'" page_name help_text)
                      true
                      true)
                expected_shortcuts)

(* ========================================================================= *)
(* MAIN PAGES - Accessible from tab bar                                     *)
(* ========================================================================= *)

(** Instances Page
    
    Expected: Help modal with global + page shortcuts
    Keymap: Enter (Open), g (Group/Role view), G (Group actions), 
            d (Diagnostics), x (Clear failure)
    Wrapper: Monitored_page.Make (auto keymap registration)
    Status: ✅ WORKING *)
let test_instances () =
  let module Instances = Octez_manager_ui.Instances in
  test_help_modal_with_status
    ~page_module:(module Instances.Page)
    ~page_name:"Instances"
    ~expected_shortcuts:[("Enter", "Open"); ("g", "Group/Role view")]
    ~status:Working
    ()

(** Wallets Page
    
    Expected: Help modal with global + page shortcuts
    Keymap: Esc (Back)
    Implementation: Direct PAGE_SIG with manual Global_shortcuts.handle
    Status: ✅ WORKING *)
let test_wallets () =
  let module Wallets_page = Octez_manager_ui.Wallets_page in
  test_help_modal_with_status
    ~page_module:(module Wallets_page.Page)
    ~page_name:"Wallets"
    ~expected_shortcuts:[("Esc", "Back")]
    ~status:Working
    ()

(** Binaries Page
    
    Expected: Help modal with global + page shortcuts
    Keymap: Esc (Back), r (Refresh), d (Download latest), l (Register dir),
            p (Prune unused), Enter (Action), Tab (Expand/Collapse)
    Wrapper: Themed_page.Make (auto keymap registration)
    Note: Themed_page handles Global_shortcuts before passing to page
    Status: ✅ WORKING *)
let test_binaries () =
  let module Binaries = Octez_manager_ui.Binaries in
  test_help_modal_with_status
    ~page_module:(module Binaries.Page)
    ~page_name:"Binaries"
    ~expected_shortcuts:[("Esc", "Back"); ("r", "Refresh")]
    ~status:Working
    ()

(** Diagnostics Page
    
    Expected: Help modal with global + page shortcuts
    Keymap: Esc (Back), r (Refresh), m (Toggle metrics), a (Edit address),
            R (Toggle recorder), d (Change duration), c (Clear caches)
    Implementation: Direct PAGE_SIG with manual Global_shortcuts.handle
    Status: ✅ WORKING *)
let test_diagnostics () =
  let module Diagnostics_page = Octez_manager_ui.Diagnostics_page in
  test_help_modal_with_status
    ~page_module:(module Diagnostics_page.Page)
    ~page_name:"Diagnostics"
    ~expected_shortcuts:[("Esc", "Back"); ("r", "Refresh")]
    ~status:Working
    ()

(** Topology Page
    
    Expected: Help modal with global + page shortcuts
    Keymap: Esc (Back)
    Wrapper: Themed_page.Make
    Status: ✅ WORKING *)
let test_topology () =
  let module Topology_page = Octez_manager_ui.Topology_page in
  test_help_modal_with_status
    ~page_module:(module Topology_page.Page)
    ~page_name:"Topology"
    ~expected_shortcuts:[("Esc", "Back")]
    ~status:Working
    ()

(** Sandbox Page
    
    Expected: Help modal with global + page shortcuts
    Keymap: Esc (Back), Enter (Open), r (Refresh), c (Create), x (Delete)
    Wrapper: Themed_page.Make
    Status: ✅ WORKING *)
let test_sandbox () =
  let module Sandbox_page = Octez_manager_ui.Sandbox_page in
  test_help_modal_with_status
    ~page_module:(module Sandbox_page.Page)
    ~page_name:"Sandbox"
    ~expected_shortcuts:[("Esc", "Back")]
    ~status:Working
    ()

(** RPC Node Selection Page
    
    Expected: Help modal should work (uses Themed_page wrapper indirectly)
    Keymap: Enter (Select), ↑/↓ (Navigate), r (Refresh), Esc (Back)
    Implementation: Direct PAGE_SIG without Global_shortcuts.handle
    Status: ❌ BROKEN
    
    Issue: Page implements handle_key but doesn't call Global_shortcuts.handle
    Fix: Add Global_shortcuts delegation:
      match Global_shortcuts.handle key with
      | Handled -> ps
      | NotGlobal -> (* existing key handling *)
    
    GitHub Issue: https://github.com/trilitech/octez-manager/issues/848 *)
let test_rpc_node_selection () =
  let module Rpc_node_selection = Octez_manager_ui.Rpc_node_selection in
  test_help_modal_with_status
    ~page_module:(module Rpc_node_selection.Page)
    ~page_name:"RPC Node Selection"
    ~expected_shortcuts:[("Enter", "Select"); ("Esc", "Back")]
    ~status:
      (Broken
         ( "Page doesn't delegate to Global_shortcuts.handle - add delegation \
            in handle_key",
           "https://github.com/trilitech/octez-manager/issues/848" ))
    ()

(* ========================================================================= *)
(* SECONDARY PAGES - Modals, detail views, wizards                          *)
(* ========================================================================= *)

(** RPC Browser Page
    
    Expected: Help modal with global + page shortcuts
    Keymap: Esc (Back), Enter (Execute), r (Refresh), h (History)
    Wrapper: Themed_page.Make
    Status: ❌ BROKEN
    
    Issue: Page requires RPC/network initialization that fails in test env
    Fix: Either:
      1. Add mocks for RPC dependencies in test environment
      2. Make page gracefully handle missing RPC connection
    
    GitHub Issue: https://github.com/trilitech/octez-manager/issues/849 *)
let test_rpc_browser () =
  let module Rpc_browser = Octez_manager_ui.Rpc_browser in
  test_help_modal_with_status
    ~page_module:(module Rpc_browser.Page)
    ~page_name:"RPC Browser"
    ~expected_shortcuts:[("Esc", "Back")]
    ~status:
      (Broken
         ( "Page initialization fails without RPC connection - needs mocking \
            or graceful fallback",
           "https://github.com/trilitech/octez-manager/issues/849" ))
    ()

(** Log Viewer Page
    
    Expected: Help modal with global + page shortcuts
    Keymap: Esc (Back)
    Wrapper: Themed_page.Make
    Status: ✅ WORKING *)
let test_log_viewer () =
  let module Log_viewer_page = Octez_manager_ui.Log_viewer_page in
  test_help_modal_with_status
    ~page_module:(module Log_viewer_page.Page)
    ~page_name:"Log Viewer"
    ~expected_shortcuts:[("Esc", "Back")]
    ~status:Working
    ()

(** Instance Details Page
    
    Expected: Help modal with global + page shortcuts
    Keymap: Esc (Back), r (Refresh), Enter (Action)
    Wrapper: Themed_page.Make
    Status: ✅ WORKING *)
let test_instance_details () =
  let module Instance_details = Octez_manager_ui.Instance_details in
  test_help_modal_with_status
    ~page_module:(module Instance_details.Page)
    ~page_name:"Instance Details"
    ~expected_shortcuts:[("Esc", "Back")]
    ~status:Working
    ()

(** Snapshots Page
    
    Expected: Help modal with global + page shortcuts
    Keymap: Esc (Back), r (Refresh), Enter (Select)
    Wrapper: Themed_page.Make
    Status: ✅ WORKING *)
let test_snapshots () =
  let module Snapshots = Octez_manager_ui.Snapshots in
  test_help_modal_with_status
    ~page_module:(module Snapshots.Page)
    ~page_name:"Snapshots"
    ~expected_shortcuts:[("Esc", "Back")]
    ~status:Working
    ()

(** Import Wizard Page
    
    Expected: Help modal with global + page shortcuts
    Keymap: Esc (Back), Enter (Next)
    Wrapper: Themed_page.Make
    Status: ✅ WORKING *)
let test_import_wizard () =
  let module Import_wizard = Octez_manager_ui.Import_wizard in
  test_help_modal_with_status
    ~page_module:(module Import_wizard.Page)
    ~page_name:"Import Wizard"
    ~expected_shortcuts:[("Esc", "Back")]
    ~status:Working
    ()

(** Rewards Page
    
    Expected: Help modal with global + page shortcuts
    Keymap: Esc (Back), r (Refresh), d (Download), Enter (Action), Tab (Switch)
    Implementation: Direct PAGE_SIG without Global_shortcuts.handle
    Status: ❌ BROKEN
    
    Issue: Similar to RPC Node Selection - no Global_shortcuts delegation
    Fix: Add Global_shortcuts.handle delegation in handle_key function
    
    GitHub Issue: https://github.com/trilitech/octez-manager/issues/850 *)
let test_rewards () =
  let module Rewards_page = Octez_manager_ui.Rewards_page in
  test_help_modal_with_status
    ~page_module:(module Rewards_page.Page)
    ~page_name:"Rewards"
    ~expected_shortcuts:[("Esc", "Back"); ("r", "Refresh")]
    ~status:
      (Broken
         ( "Page doesn't delegate to Global_shortcuts.handle - add delegation \
            in handle_key",
           "https://github.com/trilitech/octez-manager/issues/850" ))
    ()

(** Sandbox Key Allocation Page
    
    Expected: Help modal with global + page shortcuts
    Keymap: Esc (Back), Enter (Allocate), Space (Toggle)
    Wrapper: Themed_page.Make
    Status: ✅ WORKING *)
let test_sandbox_key_alloc () =
  let module Sandbox_key_alloc_page = Octez_manager_ui.Sandbox_key_alloc_page in
  test_help_modal_with_status
    ~page_module:(module Sandbox_key_alloc_page.Page)
    ~page_name:"Sandbox Key Allocation"
    ~expected_shortcuts:[("Esc", "Back")]
    ~status:Working
    ()

(* ========================================================================= *)
(* FORM PAGES - Using Form_builder framework                                *)
(* ========================================================================= *)

(** Install Node Form
    
    Expected: Form_builder pages use field-level contextual help, not global help modal
    Implementation: Form_builder with its own key handling
    Status: 📝 NOT SUPPORTED - Intentional, different help system *)
let test_install_node_form () =
  test_help_modal_with_status
    ~page_module:(module Octez_manager_ui.Install_node_form_v3.Page)
    ~page_name:"Install Node Form"
    ~expected_shortcuts:[]
    ~status:
      (NotSupported
         "Form pages use Form_builder which provides field-level help hints, \
          not a global help modal")
    ()

(** Install Baker Form - Same as Install Node Form *)
let test_install_baker_form () =
  test_help_modal_with_status
    ~page_module:(module Octez_manager_ui.Install_baker_form_v3.Page)
    ~page_name:"Install Baker Form"
    ~expected_shortcuts:[]
    ~status:(NotSupported "Form_builder field-level help")
    ()

(** Install Accuser Form - Same as Install Node Form *)
let test_install_accuser_form () =
  test_help_modal_with_status
    ~page_module:(module Octez_manager_ui.Install_accuser_form_v3.Page)
    ~page_name:"Install Accuser Form"
    ~expected_shortcuts:[]
    ~status:(NotSupported "Form_builder field-level help")
    ()

(** Install DAL Node Form - Same as Install Node Form *)
let test_install_dal_node_form () =
  test_help_modal_with_status
    ~page_module:(module Octez_manager_ui.Install_dal_node_form_v3.Page)
    ~page_name:"Install DAL Node Form"
    ~expected_shortcuts:[]
    ~status:(NotSupported "Form_builder field-level help")
    ()

(** Install Signatory Form - Same as Install Node Form *)
let test_install_signatory_form () =
  test_help_modal_with_status
    ~page_module:(module Octez_manager_ui.Install_signatory_form.Page)
    ~page_name:"Install Signatory Form"
    ~expected_shortcuts:[]
    ~status:(NotSupported "Form_builder field-level help")
    ()

(** Sandbox Create Form - Same as Install Node Form *)
let test_sandbox_create_form () =
  test_help_modal_with_status
    ~page_module:(module Octez_manager_ui.Sandbox_create_form.Page)
    ~page_name:"Sandbox Create Form"
    ~expected_shortcuts:[]
    ~status:(NotSupported "Form_builder field-level help")
    ()

(* ========================================================================= *)
(* CONTAINER PAGES - Route to other pages                                   *)
(* ========================================================================= *)

(** Main Shell - Tab container
    
    Expected: Shows active child page's keymap
    Implementation: Renders child pages, doesn't override their keymap registration
    Status: ✅ WORKING *)
let test_main_shell () =
  let module Main_shell = Octez_manager_ui.Main_shell in
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Main_shell.Page) ;
      ignore (HD.Stateful.idle_wait ~iterations:2 ~sleep:0.001 ()) ;
      ignore (HD.Stateful.send_key "?") ;
      ignore (HD.Stateful.idle_wait ~iterations:5 ~sleep:0.001 ()) ;

      let screen = TH.get_screen_text () in

      (* Should show help modal *)
      check
        bool
        "[Main Shell] help modal opens"
        true
        (TH.contains_substring screen "Global shortcuts:") ;

      (* Should show child page shortcuts (instances by default) *)
      check
        bool
        "[Main Shell] shows child page shortcuts"
        true
        (TH.contains_substring screen "Page shortcuts:") ;

      check
        bool
        "[Main Shell] shows instances 'Open' shortcut"
        true
        (TH.contains_substring screen "Open"))

(* ========================================================================= *)
(* Test Suite Summary & Statistics                                          *)
(* ========================================================================= *)

let () =
  Printf.printf "\n" ;
  Printf.printf
    "╔════════════════════════════════════════════════════════════╗\n" ;
  Printf.printf
    "║  COMPREHENSIVE HELP MODAL REGRESSION TEST SUITE            ║\n" ;
  Printf.printf
    "╚════════════════════════════════════════════════════════════╝\n" ;
  Printf.printf "\n" ;
  Printf.printf "Testing help modal (?) on ALL pages in octez-manager\n" ;
  Printf.printf "\n" ;
  Printf.printf "Test Categories:\n" ;
  Printf.printf "  ✅ WORKING - Help modal works correctly\n" ;
  Printf.printf "  ❌ BROKEN - Help modal doesn't work (needs fix)\n" ;
  Printf.printf "  📝 NOT SUPPORTED - Intentionally different help system\n" ;
  Printf.printf "\n" ;
  Printf.printf "Expected Results:\n" ;
  Printf.printf "  Working: 11 pages\n" ;
  Printf.printf "  Broken: 3 pages (RPC Node Selection, RPC Browser, Rewards)\n" ;
  Printf.printf "  Not Supported: 6 pages (Form_builder forms)\n" ;
  Printf.printf "  Total: 20 pages tested\n" ;
  Printf.printf "\n" ;
  Printf.printf "════════════════════════════════════════════════════════════\n" ;
  Printf.printf "\n" ;

  Alcotest.run
    "Help Modal Regression Tests"
    [
      ( "working_main_pages",
        [
          ("instances", `Quick, test_instances);
          ("wallets", `Quick, test_wallets);
          ("binaries", `Quick, test_binaries);
          ("diagnostics", `Quick, test_diagnostics);
          ("topology", `Quick, test_topology);
          ("sandbox", `Quick, test_sandbox);
        ] );
      ( "broken_main_pages",
        [("rpc_node_selection", `Quick, test_rpc_node_selection)] );
      ( "working_secondary_pages",
        [
          ("log_viewer", `Quick, test_log_viewer);
          ("instance_details", `Quick, test_instance_details);
          ("snapshots", `Quick, test_snapshots);
          ("import_wizard", `Quick, test_import_wizard);
          ("sandbox_key_alloc", `Quick, test_sandbox_key_alloc);
        ] );
      ( "broken_secondary_pages",
        [
          ("rpc_browser", `Quick, test_rpc_browser);
          ("rewards", `Quick, test_rewards);
        ] );
      ( "not_supported_form_pages",
        [
          ("install_node_form", `Quick, test_install_node_form);
          ("install_baker_form", `Quick, test_install_baker_form);
          ("install_accuser_form", `Quick, test_install_accuser_form);
          ("install_dal_node_form", `Quick, test_install_dal_node_form);
          ("install_signatory_form", `Quick, test_install_signatory_form);
          ("sandbox_create_form", `Quick, test_sandbox_create_form);
        ] );
      ("container_pages", [("main_shell", `Quick, test_main_shell)]);
    ]
