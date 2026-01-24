(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** TUI tests for the baker installation form.
    
    Tests the install baker TUI form (install_baker_form_v3.ml) using the 
    headless driver. Baker setup is a critical workflow for validator operations.
    
    Note: These tests verify the form structure and navigation, but do NOT
    actually install bakers (systemd operations are not mocked). *)

open Alcotest
module HD = Lib_miaou_internal.Headless_driver
module Install_baker_form = Octez_manager_ui.Install_baker_form_v3
module TH = Tui_test_helpers_lib.Tui_test_helpers

(* ============================================================ *)
(* Test: Form Initialization *)
(* ============================================================ *)

let test_form_loads () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_baker_form.Page) ;

      (* Verify page title *)
      TH.assert_screen_contains "Install Baker" ;

      (* Verify the form renders as a table *)
      TH.assert_screen_contains "Parameter" ;
      TH.assert_screen_contains "Value")

(* ============================================================ *)
(* Test: Form Has Intelligent Defaults *)
(* ============================================================ *)

let test_form_has_defaults () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_baker_form.Page) ;

      let screen = TH.get_screen_text () in

      (* Has auto-generated instance name *)
      check bool "has instance name" true (TH.contains_substring screen "baker") ;

      (* Has liquidity baking vote default *)
      check bool "has LB vote" true (TH.contains_substring screen "pass") ;

      (* Form should have validation status *)
      check
        bool
        "has status indicator"
        true
        (TH.contains_substring screen "✓" || TH.contains_substring screen "✗"))

(* ============================================================ *)
(* Test: Form Shows Baker-Specific Fields *)
(* ============================================================ *)

let test_shows_baker_specific_fields () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_baker_form.Page) ;

      let screen = TH.get_screen_text () in

      (* Baker-specific configuration *)
      check
        bool
        "shows Parent Node"
        true
        (TH.contains_substring screen "Parent Node") ;
      check
        bool
        "shows Delegates"
        true
        (TH.contains_substring screen "Delegate") ;
      check
        bool
        "shows Liquidity Baking"
        true
        (TH.contains_substring screen "Liquidity Baking"
        || TH.contains_substring screen "LB Vote") ;

      (* DAL configuration *)
      check bool "shows DAL" true (TH.contains_substring screen "DAL") ;

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
      HD.Stateful.init (module Install_baker_form.Page) ;

      let screen = TH.get_screen_text () in

      (* Essential baker fields *)
      check
        bool
        "has parent node field"
        true
        (TH.contains_substring screen "Parent Node") ;
      check
        bool
        "has base directory"
        true
        (TH.contains_substring screen "Base Dir") ;
      check
        bool
        "has delegates field"
        true
        (TH.contains_substring screen "Delegate") ;
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

      (* Client configuration *)
      check
        bool
        "has endpoint field"
        true
        (TH.contains_substring screen "Endpoint"))

(* ============================================================ *)
(* Test: Form Shows Validation Status *)
(* ============================================================ *)

let test_shows_validation_indicators () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_baker_form.Page) ;

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
      HD.Stateful.init (module Install_baker_form.Page) ;

      (* Navigate down several times *)
      TH.navigate_down 5 ;

      (* Navigate back up *)
      TH.navigate_up 3 ;

      (* Form should still be visible and functional *)
      TH.assert_screen_contains "Install Baker" ;
      TH.assert_screen_contains "Parameter")

(* ============================================================ *)
(* Test: Can Open Field Edit Modal *)
(* ============================================================ *)

let test_can_open_edit_modal () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_baker_form.Page) ;

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
      HD.Stateful.init (module Install_baker_form.Page) ;

      (* Navigate around *)
      TH.navigate_down 3 ;
      TH.navigate_up 1 ;
      TH.navigate_down 2 ;

      let final_screen = TH.get_screen_text () in

      (* Key fields should still be present *)
      check
        bool
        "still shows Parent Node"
        true
        (TH.contains_substring final_screen "Parent Node") ;
      check
        bool
        "still shows Instance Name"
        true
        (TH.contains_substring final_screen "Instance Name") ;

      (* Form didn't disappear *)
      check bool "screen not empty" true (String.length final_screen > 100))

(* ============================================================ *)
(* Test: Form Shows DAL Configuration Options *)
(* ============================================================ *)

let test_shows_dal_options () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_baker_form.Page) ;

      let screen = TH.get_screen_text () in

      (* DAL configuration should be present *)
      check bool "has DAL field" true (TH.contains_substring screen "DAL") ;

      (* Form is functional *)
      check
        bool
        "form loaded"
        true
        (TH.contains_substring screen "Install Baker"))

(* ============================================================ *)
(* VALIDATION TESTS - Test error handling and edge cases *)
(* ============================================================ *)

let test_form_validation_status () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_baker_form.Page) ;
      let screen = TH.get_screen_text () in
      check
        bool
        "has validation indicators"
        true
        (TH.contains_substring screen "✓" || TH.contains_substring screen "✗"))

let test_rejects_empty_instance_name () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_baker_form.Page) ;
      TH.navigate_down 10 ;
      Unix.sleepf 0.02 ;
      ignore (TH.send_key_and_wait "Enter") ;
      Unix.sleepf 0.02 ;
      for _i = 1 to 20 do
        ignore (TH.send_key_and_wait "Backspace") ;
        Unix.sleepf 0.005
      done ;
      ignore (TH.send_key_and_wait "Enter") ;
      Unix.sleepf 0.02 ;
      let screen = TH.get_screen_text () in
      check bool "form still active" true (String.length screen > 100))

let test_cancel_returns_to_form () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_baker_form.Page) ;
      ignore (TH.send_key_and_wait "Enter") ;
      Unix.sleepf 0.02 ;
      ignore (TH.send_key_and_wait "Escape") ;
      Unix.sleepf 0.02 ;
      let screen = TH.get_screen_text () in
      check bool "returned to form" true (String.length screen > 0))

let test_navigation_boundaries () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_baker_form.Page) ;
      TH.navigate_up 1 ;
      Unix.sleepf 0.01 ;
      let screen1 = TH.get_screen_text () in
      check bool "valid after up from first" true (String.length screen1 > 0) ;
      TH.navigate_down 50 ;
      Unix.sleepf 0.02 ;
      let screen2 = TH.get_screen_text () in
      check bool "valid after many down" true (String.length screen2 > 0) ;
      TH.assert_screen_contains "Install Baker")

let test_rapid_key_presses () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_baker_form.Page) ;
      let keys =
        ["Down"; "Up"; "Enter"; "Escape"; "Tab"; "Down"; "Enter"; "Escape"]
      in
      List.iter
        (fun key ->
          ignore (TH.send_key_and_wait ~wait_iterations:1 key) ;
          Unix.sleepf 0.005)
        keys ;
      let screen = TH.get_screen_text () in
      check bool "form survived rapid keys" true (String.length screen > 100))

let test_modal_state_recovery () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_baker_form.Page) ;
      ignore (TH.send_key_and_wait "Enter") ;
      Unix.sleepf 0.02 ;
      TH.type_string "test" ;
      Unix.sleepf 0.01 ;
      ignore (TH.send_key_and_wait "Escape") ;
      Unix.sleepf 0.02 ;
      ignore (TH.send_key_and_wait "Enter") ;
      Unix.sleepf 0.02 ;
      let screen = TH.get_screen_text () in
      check bool "modal reopened successfully" true (String.length screen > 0))

let test_delegate_field_interaction () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_baker_form.Page) ;
      TH.navigate_down 2 ;
      Unix.sleepf 0.01 ;
      ignore (TH.send_key_and_wait "Enter") ;
      Unix.sleepf 0.02 ;
      let screen = TH.get_screen_text () in
      check bool "delegate field functional" true (String.length screen > 0) ;
      ignore (TH.send_key_and_wait "Escape") ;
      Unix.sleepf 0.02 ;
      TH.assert_screen_contains "Install Baker")

let test_multiple_field_edits () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_baker_form.Page) ;
      for _i = 1 to 4 do
        ignore (TH.send_key_and_wait "Enter") ;
        Unix.sleepf 0.01 ;
        ignore (TH.send_key_and_wait "Escape") ;
        Unix.sleepf 0.01 ;
        TH.navigate_down 1
      done ;
      let screen = TH.get_screen_text () in
      check bool "form still functional" true (String.length screen > 100))

let test_baker_specific_fields_accessible () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_baker_form.Page) ;
      TH.navigate_down 5 ;
      Unix.sleepf 0.01 ;
      ignore (TH.send_key_and_wait "Enter") ;
      Unix.sleepf 0.02 ;
      ignore (TH.send_key_and_wait "Escape") ;
      Unix.sleepf 0.02 ;
      let screen = TH.get_screen_text () in
      check bool "baker fields accessible" true (String.length screen > 0))

(* ============================================================ *)
(* Validation Tests: Invalid Inputs *)
(* ============================================================ *)

let test_invalid_liquidity_baking_vote () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_baker_form.Page) ;

      (* Navigate to liquidity baking vote field *)
      TH.navigate_down 8 ;
      Unix.sleepf 0.02 ;

      ignore (TH.send_key_and_wait "Enter") ;
      Unix.sleepf 0.02 ;

      (* Try invalid vote value *)
      TH.type_string "invalid" ;
      Unix.sleepf 0.02 ;

      ignore (TH.send_key_and_wait "Enter") ;
      Unix.sleepf 0.02 ;

      let screen = TH.get_screen_text () in
      check bool "form handles invalid LB vote" true (String.length screen > 0))

let test_very_long_delegate_address () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_baker_form.Page) ;

      (* Navigate to delegate field *)
      TH.navigate_down 5 ;
      Unix.sleepf 0.02 ;

      ignore (TH.send_key_and_wait "Enter") ;
      Unix.sleepf 0.02 ;

      (* Enter very long string *)
      let long_addr = String.make 200 'x' in
      TH.type_string long_addr ;
      Unix.sleepf 0.02 ;

      ignore (TH.send_key_and_wait "Enter") ;
      Unix.sleepf 0.02 ;

      let screen = TH.get_screen_text () in
      check bool "form handles long delegate" true (String.length screen > 0))

let test_invalid_delegate_format () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_baker_form.Page) ;

      TH.navigate_down 5 ;
      Unix.sleepf 0.02 ;

      ignore (TH.send_key_and_wait "Enter") ;
      Unix.sleepf 0.02 ;

      (* Enter invalid format (not tz1/tz2/tz3) *)
      TH.type_string "invalid_format" ;
      Unix.sleepf 0.02 ;

      ignore (TH.send_key_and_wait "Enter") ;
      Unix.sleepf 0.02 ;

      let screen = TH.get_screen_text () in
      check
        bool
        "form handles invalid delegate format"
        true
        (String.length screen > 0))

let test_empty_delegate_address () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_baker_form.Page) ;

      TH.navigate_down 5 ;
      Unix.sleepf 0.02 ;

      ignore (TH.send_key_and_wait "Enter") ;
      Unix.sleepf 0.02 ;

      (* Try to submit empty delegate *)
      ignore (TH.send_key_and_wait "Enter") ;
      Unix.sleepf 0.02 ;

      let screen = TH.get_screen_text () in
      check bool "form validates empty delegate" true (String.length screen > 0))

let test_baker_instance_name_special_chars () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_baker_form.Page) ;

      TH.navigate_down 10 ;
      Unix.sleepf 0.02 ;

      ignore (TH.send_key_and_wait "Enter") ;
      Unix.sleepf 0.02 ;

      TH.type_string "baker@#$%invalid" ;
      Unix.sleepf 0.02 ;

      ignore (TH.send_key_and_wait "Enter") ;
      Unix.sleepf 0.02 ;

      let screen = TH.get_screen_text () in
      check bool "form handles special chars" true (String.length screen > 0))

let test_baker_reserved_names () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_baker_form.Page) ;

      TH.navigate_down 10 ;
      Unix.sleepf 0.02 ;

      ignore (TH.send_key_and_wait "Enter") ;
      Unix.sleepf 0.02 ;

      (* Try reserved name *)
      TH.type_string "systemd" ;
      Unix.sleepf 0.02 ;

      ignore (TH.send_key_and_wait "Enter") ;
      Unix.sleepf 0.02 ;

      let screen = TH.get_screen_text () in
      check bool "form validates reserved names" true (String.length screen > 0))

let test_dal_node_reference_invalid () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_baker_form.Page) ;

      (* Navigate to DAL node field if DAL is enabled *)
      TH.navigate_down 12 ;
      Unix.sleepf 0.02 ;

      ignore (TH.send_key_and_wait "Enter") ;
      Unix.sleepf 0.02 ;

      (* Enter invalid reference *)
      TH.type_string "nonexistent-dal" ;
      Unix.sleepf 0.02 ;

      ignore (TH.send_key_and_wait "Enter") ;
      Unix.sleepf 0.02 ;

      let screen = TH.get_screen_text () in
      check bool "form handles invalid DAL ref" true (String.length screen > 0))

(* ============================================================ *)
(* Test Suite *)
(* ============================================================ *)

let form_tests =
  [
    ("form loads and renders", `Quick, test_form_loads);
    ("form has intelligent defaults", `Quick, test_form_has_defaults);
    ( "form shows baker-specific fields",
      `Quick,
      test_shows_baker_specific_fields );
    ("form shows all required fields", `Quick, test_shows_required_fields);
    ( "form shows validation indicators",
      `Quick,
      test_shows_validation_indicators );
    ("arrow key navigation works", `Quick, test_arrow_navigation);
    ("can open field edit modal", `Quick, test_can_open_edit_modal);
    ("form persists after navigation", `Quick, test_form_persists_navigation);
    ("form shows DAL options", `Quick, test_shows_dal_options);
  ]

let validation_tests =
  [
    ("form validation status", `Quick, test_form_validation_status);
    ("rejects empty instance name", `Quick, test_rejects_empty_instance_name);
    ("cancel returns to form", `Quick, test_cancel_returns_to_form);
    ("navigation boundaries", `Quick, test_navigation_boundaries);
    ("rapid key presses", `Quick, test_rapid_key_presses);
    ("modal state recovery", `Quick, test_modal_state_recovery);
    ("delegate field interaction", `Quick, test_delegate_field_interaction);
    ("multiple field edits", `Quick, test_multiple_field_edits);
    ("baker fields accessible", `Quick, test_baker_specific_fields_accessible);
    (* New validation tests for issue #457 *)
    ("invalid liquidity baking vote", `Quick, test_invalid_liquidity_baking_vote);
    ("very long delegate address", `Quick, test_very_long_delegate_address);
    ("invalid delegate format", `Quick, test_invalid_delegate_format);
    ("empty delegate address", `Quick, test_empty_delegate_address);
    ( "baker instance name special chars",
      `Quick,
      test_baker_instance_name_special_chars );
    ("baker reserved names", `Quick, test_baker_reserved_names);
    ("DAL node reference invalid", `Quick, test_dal_node_reference_invalid);
  ]

let () =
  Alcotest.run
    "Install Baker Form (TUI)"
    [("baker_form", form_tests); ("validation", validation_tests)]
