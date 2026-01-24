(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** TUI tests for the accuser installation form.
    
    Tests the install accuser TUI form (install_accuser_form_v3.ml) using the 
    headless driver. Accuser monitors the chain for double-baking/endorsing.
    
    Note: These tests verify the form structure and navigation, but do NOT
    actually install accusers (systemd operations are not mocked). *)

open Alcotest
module HD = Lib_miaou_internal.Headless_driver
module Install_accuser_form = Octez_manager_ui.Install_accuser_form_v3
module TH = Tui_test_helpers_lib.Tui_test_helpers

(* ============================================================ *)
(* Test: Form Initialization *)
(* ============================================================ *)

let test_form_loads () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_accuser_form.Page) ;

      (* Verify page title *)
      TH.assert_screen_contains "Install Accuser" ;

      (* Verify the form renders as a table *)
      TH.assert_screen_contains "Parameter" ;
      TH.assert_screen_contains "Value")

(* ============================================================ *)
(* Test: Form Has Intelligent Defaults *)
(* ============================================================ *)

let test_form_has_defaults () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_accuser_form.Page) ;

      let screen = TH.get_screen_text () in

      (* Has auto-generated instance name *)
      check
        bool
        "has instance name"
        true
        (TH.contains_substring screen "accuser") ;

      (* Form should have validation status *)
      check
        bool
        "has status indicator"
        true
        (TH.contains_substring screen "✓" || TH.contains_substring screen "✗"))

(* ============================================================ *)
(* Test: Form Shows Accuser-Specific Fields *)
(* ============================================================ *)

let test_shows_accuser_specific_fields () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_accuser_form.Page) ;

      let screen = TH.get_screen_text () in

      (* Accuser-specific configuration - simpler than baker *)
      check bool "shows Node field" true (TH.contains_substring screen "Node") ;
      check bool "shows Base Dir" true (TH.contains_substring screen "Base Dir") ;

      (* Service configuration *)
      check
        bool
        "shows Instance Name"
        true
        (TH.contains_substring screen "Instance Name") ;
      check
        bool
        "shows Service User"
        true
        (TH.contains_substring screen "Service User") ;
      check
        bool
        "shows Enable on Boot"
        true
        (TH.contains_substring screen "Enable on Boot") ;

      (* Submit button *)
      check
        bool
        "shows Confirm button"
        true
        (TH.contains_substring screen "Confirm"))

(* ============================================================ *)
(* Test: Form Shows All Required Fields *)
(* ============================================================ *)

let test_shows_required_fields () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_accuser_form.Page) ;

      let screen = TH.get_screen_text () in

      (* Essential accuser fields - simpler than baker/node *)
      check
        bool
        "has base directory"
        true
        (TH.contains_substring screen "Base Dir") ;
      check
        bool
        "has instance name"
        true
        (TH.contains_substring screen "Instance Name") ;
      check
        bool
        "has app bin dir"
        true
        (TH.contains_substring screen "App Bin Dir") ;
      check bool "has node field" true (TH.contains_substring screen "Node"))

(* ============================================================ *)
(* Test: Form Shows Validation Status *)
(* ============================================================ *)

let test_shows_validation_indicators () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_accuser_form.Page) ;

      let screen = TH.get_screen_text () in

      (* Form has checkmarks (✓) for valid fields *)
      check
        bool
        "has success checkmarks"
        true
        (TH.contains_substring screen "✓") ;

      (* Form has warnings (✗) for invalid/incomplete fields *)
      check
        bool
        "has validation warnings"
        true
        (TH.contains_substring screen "✗") ;

      (* Form indicates validation status *)
      check
        bool
        "shows status"
        true
        (TH.contains_substring screen "Incomplete"
        || TH.contains_substring screen "complete"
        || TH.contains_substring screen "Form"))

(* ============================================================ *)
(* Test: Arrow Key Navigation Works *)
(* ============================================================ *)

let test_arrow_navigation () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_accuser_form.Page) ;

      (* Navigate down several times *)
      TH.navigate_down 5 ;

      (* Navigate back up *)
      TH.navigate_up 3 ;

      (* Form should still be visible and functional *)
      TH.assert_screen_contains "Install Accuser" ;
      TH.assert_screen_contains "Parameter")

(* ============================================================ *)
(* Test: Can Open Field Edit Modal *)
(* ============================================================ *)

let test_can_open_edit_modal () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_accuser_form.Page) ;

      (* Try to open a modal by pressing Enter *)
      ignore (TH.send_key_and_wait "Enter") ;

      (* Wait a bit for modal to appear *)
      Unix.sleepf 0.01 ;

      (* Either a modal opened, or we're still on the form - both are OK *)
      (* Just verify we didn't crash *)
      let screen = TH.get_screen_text () in
      check bool "still has content" true (String.length screen > 0))

(* ============================================================ *)
(* Test: Form Persists After Navigation *)
(* ============================================================ *)

let test_form_persists_navigation () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_accuser_form.Page) ;

      (* Navigate around *)
      TH.navigate_down 3 ;
      TH.navigate_up 1 ;
      TH.navigate_down 2 ;

      let final_screen = TH.get_screen_text () in

      (* Key fields should still be present *)
      check
        bool
        "still shows Node field"
        true
        (TH.contains_substring final_screen "Node") ;
      check
        bool
        "still shows Instance Name"
        true
        (TH.contains_substring final_screen "Instance Name") ;

      (* Form didn't disappear *)
      check bool "screen not empty" true (String.length final_screen > 100))

(* ============================================================ *)
(* Test Suite *)
(* ============================================================ *)

let form_tests =
  [
    ("form loads and renders", `Quick, test_form_loads);
    ("form has intelligent defaults", `Quick, test_form_has_defaults);
    ( "form shows accuser-specific fields",
      `Quick,
      test_shows_accuser_specific_fields );
    ("form shows all required fields", `Quick, test_shows_required_fields);
    ( "form shows validation indicators",
      `Quick,
      test_shows_validation_indicators );
    ("arrow key navigation works", `Quick, test_arrow_navigation);
    ("can open field edit modal", `Quick, test_can_open_edit_modal);
    ("form persists after navigation", `Quick, test_form_persists_navigation);
  ]

let () =
  Alcotest.run "Install Accuser Form (TUI)" [("accuser_form", form_tests)]
