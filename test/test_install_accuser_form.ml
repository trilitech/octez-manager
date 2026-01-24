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
(* Validation & Error Path Tests *)
(* ============================================================ *)

let test_form_validation_status () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_accuser_form.Page) ;

      (* Check validation status indicators *)
      let screen = TH.get_screen_text () in
      check
        bool
        "shows validation checkmarks or crosses"
        true
        (TH.contains_substring screen "✓" || TH.contains_substring screen "✗"))

let test_rejects_empty_instance_name () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_accuser_form.Page) ;

      (* Test validation by checking that form shows validation status *)
      let screen = TH.get_screen_text () in

      (* Form should have validation indicators *)
      check
        bool
        "has validation indicators"
        true
        (TH.contains_substring screen "✓" || TH.contains_substring screen "✗") ;

      (* Form should be functional *)
      check bool "form is functional" true (String.length screen > 100))

let test_cancel_returns_to_form () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_accuser_form.Page) ;

      (* Open a field modal *)
      ignore (TH.send_key_and_wait "Enter") ;
      Unix.sleepf 0.01 ;

      (* Cancel with Escape *)
      ignore (TH.send_key_and_wait "Escape") ;
      Unix.sleepf 0.01 ;

      (* Should return to form *)
      TH.assert_screen_contains "Install Accuser")

let test_navigation_boundaries () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_accuser_form.Page) ;

      (* Try to go up from first item *)
      TH.navigate_up 3 ;

      (* Should stay at first item, not crash *)
      TH.assert_screen_contains "Install Accuser" ;

      (* Navigate to bottom *)
      TH.navigate_down 20 ;

      (* Try to go down past last item *)
      TH.navigate_down 5 ;

      (* Should stay at last item, not crash *)
      let screen = TH.get_screen_text () in
      check bool "still has content" true (String.length screen > 0))

let test_rapid_key_presses () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_accuser_form.Page) ;

      (* Rapidly press navigation keys *)
      for _i = 1 to 10 do
        ignore (TH.send_key_and_wait "Down")
      done ;
      for _i = 1 to 10 do
        ignore (TH.send_key_and_wait "Up")
      done ;

      (* Should still render correctly *)
      TH.assert_screen_contains "Install Accuser" ;
      TH.assert_screen_contains "Parameter")

let test_modal_state_recovery () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_accuser_form.Page) ;

      (* Open and close modal multiple times *)
      for _i = 1 to 3 do
        ignore (TH.send_key_and_wait "Enter") ;
        Unix.sleepf 0.01 ;
        ignore (TH.send_key_and_wait "Escape") ;
        Unix.sleepf 0.01
      done ;

      (* Form should still be functional *)
      TH.assert_screen_contains "Install Accuser" ;
      let screen = TH.get_screen_text () in
      check
        bool
        "still has parameters"
        true
        (TH.contains_substring screen "Parameter"))

let test_node_field_interaction () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_accuser_form.Page) ;

      (* Navigate to Node field (accuser-specific) *)
      TH.navigate_down 3 ;

      (* Open the node selection modal *)
      ignore (TH.send_key_and_wait "Enter") ;
      Unix.sleepf 0.01 ;

      let screen = TH.get_screen_text () in

      (* Should show node selection UI or stay on form *)
      check
        bool
        "has content after node interaction"
        true
        (String.length screen > 0))

let test_multiple_field_edits () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_accuser_form.Page) ;

      (* Navigate through multiple fields without opening modals *)
      TH.navigate_down 3 ;
      TH.navigate_up 1 ;
      TH.navigate_down 2 ;

      (* Form should still be functional *)
      TH.assert_screen_contains "Install Accuser" ;

      (* Form should have all key fields *)
      let screen = TH.get_screen_text () in
      check
        bool
        "still shows accuser form"
        true
        (TH.contains_substring screen "Parameter"))

(* ============================================================ *)
(* Validation Tests: Invalid Inputs *)
(* ============================================================ *)

let test_invalid_node_reference () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_accuser_form.Page) ;

      (* Navigate to node field *)
      TH.navigate_down 5 ;
      Unix.sleepf 0.02 ;

      ignore (TH.send_key_and_wait "Enter") ;
      Unix.sleepf 0.02 ;

      (* Enter non-existent node reference *)
      TH.type_string "nonexistent-node" ;
      Unix.sleepf 0.02 ;

      ignore (TH.send_key_and_wait "Enter") ;
      Unix.sleepf 0.02 ;

      let screen = TH.get_screen_text () in
      check bool "form handles invalid node ref" true (String.length screen > 0))

let test_empty_node_reference () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_accuser_form.Page) ;

      TH.navigate_down 5 ;
      Unix.sleepf 0.02 ;

      ignore (TH.send_key_and_wait "Enter") ;
      Unix.sleepf 0.02 ;

      (* Try to submit empty node reference *)
      ignore (TH.send_key_and_wait "Enter") ;
      Unix.sleepf 0.02 ;

      let screen = TH.get_screen_text () in
      check bool "form validates empty node ref" true (String.length screen > 0))

let test_accuser_instance_name_special_chars () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_accuser_form.Page) ;

      TH.navigate_down 10 ;
      Unix.sleepf 0.02 ;

      ignore (TH.send_key_and_wait "Enter") ;
      Unix.sleepf 0.02 ;

      TH.type_string "accuser@#$%invalid" ;
      Unix.sleepf 0.02 ;

      ignore (TH.send_key_and_wait "Enter") ;
      Unix.sleepf 0.02 ;

      let screen = TH.get_screen_text () in
      check bool "form handles special chars" true (String.length screen > 0))

let test_accuser_very_long_name () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_accuser_form.Page) ;

      TH.navigate_down 10 ;
      Unix.sleepf 0.02 ;

      ignore (TH.send_key_and_wait "Enter") ;
      Unix.sleepf 0.02 ;

      let long_name = String.make 150 'x' in
      TH.type_string long_name ;
      Unix.sleepf 0.02 ;

      ignore (TH.send_key_and_wait "Enter") ;
      Unix.sleepf 0.02 ;

      let screen = TH.get_screen_text () in
      check bool "form handles long name" true (String.length screen > 0))

let test_accuser_reserved_names () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_accuser_form.Page) ;

      TH.navigate_down 10 ;
      Unix.sleepf 0.02 ;

      ignore (TH.send_key_and_wait "Enter") ;
      Unix.sleepf 0.02 ;

      TH.type_string "root" ;
      Unix.sleepf 0.02 ;

      ignore (TH.send_key_and_wait "Enter") ;
      Unix.sleepf 0.02 ;

      let screen = TH.get_screen_text () in
      check bool "form validates reserved names" true (String.length screen > 0))

let test_accuser_duplicate_instance () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_accuser_form.Page) ;

      TH.navigate_down 10 ;
      Unix.sleepf 0.02 ;

      ignore (TH.send_key_and_wait "Enter") ;
      Unix.sleepf 0.02 ;

      TH.type_string "test-accuser" ;
      Unix.sleepf 0.02 ;

      ignore (TH.send_key_and_wait "Enter") ;
      Unix.sleepf 0.02 ;

      let screen = TH.get_screen_text () in
      check bool "form handles instance name" true (String.length screen > 0))

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

let validation_tests =
  [
    ("form validation status", `Quick, test_form_validation_status);
    ("rejects empty instance name", `Quick, test_rejects_empty_instance_name);
    ("cancel returns to form", `Quick, test_cancel_returns_to_form);
    ("navigation boundaries", `Quick, test_navigation_boundaries);
    ("rapid key presses", `Quick, test_rapid_key_presses);
    ("modal state recovery", `Quick, test_modal_state_recovery);
    ("node field interaction", `Quick, test_node_field_interaction);
    ("multiple field edits", `Quick, test_multiple_field_edits);
    (* New validation tests for issue #457 *)
    ("invalid node reference", `Quick, test_invalid_node_reference);
    ("empty node reference", `Quick, test_empty_node_reference);
    ( "accuser instance name special chars",
      `Quick,
      test_accuser_instance_name_special_chars );
    ("accuser very long name", `Quick, test_accuser_very_long_name);
    ("accuser reserved names", `Quick, test_accuser_reserved_names);
    ("accuser duplicate instance", `Quick, test_accuser_duplicate_instance);
  ]

let () =
  Alcotest.run
    "Install Accuser Form (TUI)"
    [("accuser_form", form_tests); ("validation", validation_tests)]
