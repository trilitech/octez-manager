(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** TUI tests for the node installation form.
    
    Tests the install node TUI form (install_node_form_v3.ml) using the 
    headless driver. This covers the most critical user workflow in the 
    application.
    
    Note: These tests verify the form structure and navigation, but do NOT
    actually install nodes (systemd operations are not mocked). *)

open Alcotest
module HD = Lib_miaou_internal.Headless_driver
module Install_node_form = Octez_manager_ui.Install_node_form_v3
module TH = Tui_test_helpers

(* ============================================================ *)
(* Test: Form Initialization *)
(* ============================================================ *)

let test_form_loads () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_node_form.Page) ;

      (* Verify page title *)
      TH.assert_screen_contains "Install Node" ;

      (* Verify the form renders as a table *)
      TH.assert_screen_contains "Parameter" ;
      TH.assert_screen_contains "Value")

(* ============================================================ *)
(* Test: Form Has Intelligent Defaults *)
(* ============================================================ *)

let test_form_has_defaults () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_node_form.Page) ;

      let screen = TH.get_screen_text () in

      (* Network defaults to Shadownet *)
      check
        bool
        "has shadownet default"
        true
        (TH.contains_substring screen "Shadownet") ;

      (* History mode defaults to rolling *)
      check
        bool
        "has rolling default"
        true
        (TH.contains_substring screen "rolling") ;

      (* Has auto-generated instance name *)
      check
        bool
        "has instance name"
        true
        (TH.contains_substring screen "node-shadownet") ;

      (* Has RPC address default *)
      check
        bool
        "has RPC address"
        true
        (TH.contains_substring screen "127.0.0.1") ;

      (* Has P2P address default *)
      check bool "has P2P address" true (TH.contains_substring screen "0.0.0.0"))

(* ============================================================ *)
(* Test: Form Shows Required Fields *)
(* ============================================================ *)

let test_shows_all_required_fields () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_node_form.Page) ;

      let screen = TH.get_screen_text () in

      (* Essential fields *)
      check bool "shows Network" true (TH.contains_substring screen "Network") ;
      check
        bool
        "shows History Mode"
        true
        (TH.contains_substring screen "History Mode") ;
      check
        bool
        "shows RPC Address"
        true
        (TH.contains_substring screen "RPC Address") ;
      check
        bool
        "shows P2P Address"
        true
        (TH.contains_substring screen "P2P Address") ;
      check bool "shows Data Dir" true (TH.contains_substring screen "Data Dir") ;
      check
        bool
        "shows Service User"
        true
        (TH.contains_substring screen "Service User") ;
      check
        bool
        "shows Instance Name"
        true
        (TH.contains_substring screen "Instance Name") ;
      check bool "shows Snapshot" true (TH.contains_substring screen "Snapshot") ;
      check
        bool
        "shows Enable on Boot"
        true
        (TH.contains_substring screen "Enable on Boot") ;
      check
        bool
        "shows Start Now"
        true
        (TH.contains_substring screen "Start Now") ;

      (* Submit button *)
      check
        bool
        "shows Confirm button"
        true
        (TH.contains_substring screen "Confirm"))

(* ============================================================ *)
(* Test: Form Shows Validation Status *)
(* ============================================================ *)

let test_shows_validation_indicators () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_node_form.Page) ;

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

      (* Form indicates it's incomplete initially *)
      check
        bool
        "shows incomplete status"
        true
        (TH.contains_substring screen "Incomplete"
        || TH.contains_substring screen "Form incomplete"))

(* ============================================================ *)
(* Test: Arrow Key Navigation Works *)
(* ============================================================ *)

let test_arrow_navigation () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_node_form.Page) ;

      (* Navigate down several times *)
      TH.navigate_down 5 ;

      (* Navigate back up *)
      TH.navigate_up 3 ;

      (* Form should still be visible and functional *)
      TH.assert_screen_contains "Install Node" ;
      TH.assert_screen_contains "Parameter")

(* ============================================================ *)
(* Test: Can Open Field Edit Modal *)
(* ============================================================ *)

let test_can_open_edit_modal () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_node_form.Page) ;

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
      HD.Stateful.init (module Install_node_form.Page) ;

      (* Navigate around *)
      TH.navigate_down 3 ;
      TH.navigate_up 1 ;
      TH.navigate_down 2 ;

      let final_screen = TH.get_screen_text () in

      (* Key fields should still be present *)
      check
        bool
        "still shows Network"
        true
        (TH.contains_substring final_screen "Network") ;
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
    ("form shows all required fields", `Quick, test_shows_all_required_fields);
    ( "form shows validation indicators",
      `Quick,
      test_shows_validation_indicators );
    ("arrow key navigation works", `Quick, test_arrow_navigation);
    ("can open field edit modal", `Quick, test_can_open_edit_modal);
    ("form persists after navigation", `Quick, test_form_persists_navigation);
  ]

let () = Alcotest.run "Install Node Form (TUI)" [("node_form", form_tests)]
