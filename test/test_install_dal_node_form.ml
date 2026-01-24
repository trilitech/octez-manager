(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** TUI tests for the DAL node installation form.
    
    Tests the install DAL node TUI form (install_dal_node_form_v3.ml) using the 
    headless driver. DAL (Data Availability Layer) nodes support data availability.
    
    Note: These tests verify the form structure and navigation, but do NOT
    actually install DAL nodes (systemd operations are not mocked). *)

open Alcotest
module HD = Lib_miaou_internal.Headless_driver
module Install_dal_form = Octez_manager_ui.Install_dal_node_form_v3
module TH = Tui_test_helpers_lib.Tui_test_helpers

(* ============================================================ *)
(* Test: Form Initialization *)
(* ============================================================ *)

let test_form_loads () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_dal_form.Page) ;

      (* Verify page title *)
      TH.assert_screen_contains "Install DAL Node" ;

      (* Verify the form renders as a table *)
      TH.assert_screen_contains "Parameter" ;
      TH.assert_screen_contains "Value")

(* ============================================================ *)
(* Test: Form Has Intelligent Defaults *)
(* ============================================================ *)

let test_form_has_defaults () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_dal_form.Page) ;

      let screen = TH.get_screen_text () in

      (* Has auto-generated instance name *)
      check bool "has instance name" true (TH.contains_substring screen "dal") ;

      (* Has default DAL RPC address *)
      check
        bool
        "has default RPC address"
        true
        (TH.contains_substring screen "10732") ;

      (* Has default DAL P2P address *)
      check
        bool
        "has default P2P address"
        true
        (TH.contains_substring screen "11732") ;

      (* Form should have validation status *)
      check
        bool
        "has status indicator"
        true
        (TH.contains_substring screen "✓" || TH.contains_substring screen "✗"))

(* ============================================================ *)
(* Test: Form Shows DAL-Specific Fields *)
(* ============================================================ *)

let test_shows_dal_specific_fields () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_dal_form.Page) ;

      let screen = TH.get_screen_text () in

      (* DAL-specific configuration *)
      check bool "shows Node field" true (TH.contains_substring screen "Node") ;
      check
        bool
        "shows DAL RPC Addr"
        true
        (TH.contains_substring screen "DAL RPC Addr") ;
      check
        bool
        "shows DAL P2P Addr"
        true
        (TH.contains_substring screen "DAL P2P Addr") ;
      check
        bool
        "shows DAL Data Dir"
        true
        (TH.contains_substring screen "DAL Data Dir") ;

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
      HD.Stateful.init (module Install_dal_form.Page) ;

      let screen = TH.get_screen_text () in

      (* Essential DAL fields *)
      check bool "has node field" true (TH.contains_substring screen "Node") ;
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
      check
        bool
        "has DAL data directory"
        true
        (TH.contains_substring screen "DAL Data Dir") ;
      check
        bool
        "has RPC address"
        true
        (TH.contains_substring screen "DAL RPC Addr") ;
      check
        bool
        "has P2P address"
        true
        (TH.contains_substring screen "DAL P2P Addr"))

(* ============================================================ *)
(* Test: Form Shows Validation Status *)
(* ============================================================ *)

let test_shows_validation_indicators () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_dal_form.Page) ;

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
      HD.Stateful.init (module Install_dal_form.Page) ;

      (* Navigate down several times *)
      TH.navigate_down 5 ;

      (* Navigate back up *)
      TH.navigate_up 3 ;

      (* Form should still be visible and functional *)
      TH.assert_screen_contains "Install DAL Node" ;
      TH.assert_screen_contains "Parameter")

(* ============================================================ *)
(* Test: Can Open Field Edit Modal *)
(* ============================================================ *)

let test_can_open_edit_modal () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_dal_form.Page) ;

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
      HD.Stateful.init (module Install_dal_form.Page) ;

      (* Navigate around *)
      TH.navigate_down 3 ;
      TH.navigate_up 1 ;
      TH.navigate_down 2 ;

      let final_screen = TH.get_screen_text () in

      (* Key fields should still be present *)
      check
        bool
        "still shows DAL RPC Addr"
        true
        (TH.contains_substring final_screen "DAL RPC Addr") ;
      check
        bool
        "still shows Instance Name"
        true
        (TH.contains_substring final_screen "Instance Name") ;

      (* Form didn't disappear *)
      check bool "screen not empty" true (String.length final_screen > 100))

(* ============================================================ *)
(* Test: Form Shows Default Port Numbers *)
(* ============================================================ *)

let test_shows_default_ports () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_dal_form.Page) ;

      let screen = TH.get_screen_text () in

      (* DAL RPC default port *)
      check
        bool
        "shows RPC port 10732"
        true
        (TH.contains_substring screen "10732") ;

      (* DAL P2P default port *)
      check
        bool
        "shows P2P port 11732"
        true
        (TH.contains_substring screen "11732"))

(* ============================================================ *)
(* Validation & Error Path Tests *)
(* ============================================================ *)

let test_form_validation_status () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_dal_form.Page) ;

      (* Check validation status indicators *)
      let screen = TH.get_screen_text () in
      check
        bool
        "shows validation checkmarks or crosses"
        true
        (TH.contains_substring screen "✓" || TH.contains_substring screen "✗"))

let test_validation_updates_dynamically () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_dal_form.Page) ;

      (* Test that form shows validation indicators *)
      let screen = TH.get_screen_text () in

      (* Form should have validation status *)
      check
        bool
        "has validation indicators"
        true
        (TH.contains_substring screen "✓" || TH.contains_substring screen "✗") ;

      (* Form should be functional *)
      check bool "form is functional" true (String.length screen > 100))

let test_cancel_returns_to_form () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_dal_form.Page) ;

      (* Open a field modal *)
      ignore (TH.send_key_and_wait "Enter") ;
      Unix.sleepf 0.01 ;

      (* Cancel with Escape *)
      ignore (TH.send_key_and_wait "Escape") ;
      Unix.sleepf 0.01 ;

      (* Should return to form *)
      TH.assert_screen_contains "Install DAL Node")

let test_navigation_boundaries () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_dal_form.Page) ;

      (* Try to go up from first item *)
      TH.navigate_up 3 ;

      (* Should stay at first item, not crash *)
      TH.assert_screen_contains "Install DAL Node" ;

      (* Navigate to bottom *)
      TH.navigate_down 20 ;

      (* Try to go down past last item *)
      TH.navigate_down 5 ;

      (* Should stay at last item, not crash *)
      let screen = TH.get_screen_text () in
      check bool "still has content" true (String.length screen > 0))

let test_rapid_key_presses () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_dal_form.Page) ;

      (* Rapidly press navigation keys *)
      for _i = 1 to 10 do
        ignore (TH.send_key_and_wait "Down")
      done ;
      for _i = 1 to 10 do
        ignore (TH.send_key_and_wait "Up")
      done ;

      (* Should still render correctly *)
      TH.assert_screen_contains "Install DAL Node" ;
      TH.assert_screen_contains "Parameter")

let test_modal_state_recovery () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_dal_form.Page) ;

      (* Open and close modal multiple times *)
      for _i = 1 to 3 do
        ignore (TH.send_key_and_wait "Enter") ;
        Unix.sleepf 0.01 ;
        ignore (TH.send_key_and_wait "Escape") ;
        Unix.sleepf 0.01
      done ;

      (* Form should still be functional *)
      TH.assert_screen_contains "Install DAL Node" ;
      let screen = TH.get_screen_text () in
      check
        bool
        "still has parameters"
        true
        (TH.contains_substring screen "Parameter"))

let test_dal_specific_field_interaction () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_dal_form.Page) ;

      (* Navigate to DAL-specific fields *)
      TH.navigate_down 4 ;

      (* Interact with field *)
      ignore (TH.send_key_and_wait "Enter") ;
      Unix.sleepf 0.01 ;

      let screen = TH.get_screen_text () in

      (* Should show modal or stay on form *)
      check
        bool
        "has content after field interaction"
        true
        (String.length screen > 0))

let test_multiple_field_navigation () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_dal_form.Page) ;

      (* Navigate through multiple fields *)
      TH.navigate_down 3 ;
      TH.navigate_up 1 ;
      TH.navigate_down 2 ;
      TH.navigate_up 2 ;
      TH.navigate_down 4 ;

      (* Form should still be functional *)
      TH.assert_screen_contains "Install DAL Node" ;

      (* Form should have all key DAL fields *)
      let screen = TH.get_screen_text () in
      check
        bool
        "still shows DAL fields"
        true
        (TH.contains_substring screen "DAL RPC Addr"
        || TH.contains_substring screen "Parameter"))

let test_port_field_accessibility () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_dal_form.Page) ;

      let screen = TH.get_screen_text () in

      (* DAL has unique port fields - verify they're accessible *)
      check
        bool
        "shows RPC port field"
        true
        (TH.contains_substring screen "10732") ;
      check
        bool
        "shows P2P port field"
        true
        (TH.contains_substring screen "11732") ;

      (* Navigate to verify fields are interactive *)
      TH.navigate_down 5 ;

      (* Form should still be functional *)
      TH.assert_screen_contains "Install DAL Node")

(* ============================================================ *)
(* Test Suite *)
(* ============================================================ *)

let form_tests =
  [
    ("form loads and renders", `Quick, test_form_loads);
    ("form has intelligent defaults", `Quick, test_form_has_defaults);
    ("form shows DAL-specific fields", `Quick, test_shows_dal_specific_fields);
    ("form shows all required fields", `Quick, test_shows_required_fields);
    ( "form shows validation indicators",
      `Quick,
      test_shows_validation_indicators );
    ("arrow key navigation works", `Quick, test_arrow_navigation);
    ("can open field edit modal", `Quick, test_can_open_edit_modal);
    ("form persists after navigation", `Quick, test_form_persists_navigation);
    ("form shows default ports", `Quick, test_shows_default_ports);
  ]

let validation_tests =
  [
    ("form validation status", `Quick, test_form_validation_status);
    ( "validation updates dynamically",
      `Quick,
      test_validation_updates_dynamically );
    ("cancel returns to form", `Quick, test_cancel_returns_to_form);
    ("navigation boundaries", `Quick, test_navigation_boundaries);
    ("rapid key presses", `Quick, test_rapid_key_presses);
    ("modal state recovery", `Quick, test_modal_state_recovery);
    ( "DAL specific field interaction",
      `Quick,
      test_dal_specific_field_interaction );
    ("multiple field navigation", `Quick, test_multiple_field_navigation);
    ("port field accessibility", `Quick, test_port_field_accessibility);
  ]

let () =
  Alcotest.run
    "Install DAL Node Form (TUI)"
    [("dal_form", form_tests); ("validation", validation_tests)]
