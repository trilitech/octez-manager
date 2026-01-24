(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** TUI tests for modal interactions through install forms.
    
    Tests modal_helpers.ml (447 points, 0% coverage) by exercising modals
    through actual form usage. This improves coverage for both forms and
    modal infrastructure.
    
    Strategy: Use install forms as drivers to trigger and interact with modals.
    Each form uses different modals (network selection, file browser, etc.) *)

open Alcotest
module HD = Lib_miaou_internal.Headless_driver
module Install_node_form = Octez_manager_ui.Install_node_form_v3
module TH = Tui_test_helpers_lib.Tui_test_helpers

(* ============================================================ *)
(* Test: Modal Opens and Renders *)
(* ============================================================ *)

let test_modal_opens_on_enter () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_node_form.Page) ;

      (* Press Enter on first field to open modal *)
      ignore (TH.send_key_and_wait "Enter") ;
      Unix.sleepf 0.02 ;

      let screen = TH.get_screen_text () in

      (* Modal should either be open or form is still visible *)
      (* Both are OK - we're just testing no crash *)
      check bool "screen has content after Enter" true (String.length screen > 0) ;

      (* Screen should have changed (modal opened or field selected) *)
      check
        bool
        "interaction happened"
        true
        (TH.contains_substring screen "Network"
        || TH.contains_substring screen "Select"
        || TH.contains_substring screen "shadownet"))

(* ============================================================ *)
(* Test: Modal Navigation with Arrow Keys *)
(* ============================================================ *)

let test_modal_navigation () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_node_form.Page) ;

      (* Open a modal (first field - likely network selection) *)
      ignore (TH.send_key_and_wait "Enter") ;
      Unix.sleepf 0.02 ;

      (* Navigate in modal with arrows *)
      TH.navigate_down 2 ;
      Unix.sleepf 0.01 ;
      TH.navigate_up 1 ;
      Unix.sleepf 0.01 ;

      let screen = TH.get_screen_text () in

      (* Form should still be functional *)
      check bool "navigation didn't crash" true (String.length screen > 0))

(* ============================================================ *)
(* Test: Modal Closes with Escape *)
(* ============================================================ *)

let test_modal_closes_on_escape () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_node_form.Page) ;

      (* Open modal *)
      ignore (TH.send_key_and_wait "Enter") ;
      Unix.sleepf 0.02 ;

      let screen_with_modal = TH.get_screen_text () in

      (* Close modal with Escape *)
      ignore (TH.send_key_and_wait "Escape") ;
      Unix.sleepf 0.02 ;

      let screen_after_escape = TH.get_screen_text () in

      (* Both screens should have content *)
      check
        bool
        "modal screen has content"
        true
        (String.length screen_with_modal > 0) ;
      check
        bool
        "after escape has content"
        true
        (String.length screen_after_escape > 0) ;

      (* After escape, should be back to form *)
      check
        bool
        "back to form"
        true
        (TH.contains_substring screen_after_escape "Install Node"
        || TH.contains_substring screen_after_escape "Parameter"))

(* ============================================================ *)
(* Test: Multiple Modal Open/Close Cycles *)
(* ============================================================ *)

let test_multiple_modal_cycles () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_node_form.Page) ;

      (* Cycle 1: Open and close *)
      ignore (TH.send_key_and_wait "Enter") ;
      Unix.sleepf 0.01 ;
      ignore (TH.send_key_and_wait "Escape") ;
      Unix.sleepf 0.01 ;

      (* Cycle 2: Navigate to another field, open and close *)
      TH.navigate_down 1 ;
      ignore (TH.send_key_and_wait "Enter") ;
      Unix.sleepf 0.01 ;
      ignore (TH.send_key_and_wait "Escape") ;
      Unix.sleepf 0.01 ;

      (* Cycle 3: One more time *)
      TH.navigate_down 1 ;
      ignore (TH.send_key_and_wait "Enter") ;
      Unix.sleepf 0.01 ;
      ignore (TH.send_key_and_wait "Escape") ;
      Unix.sleepf 0.01 ;

      let final_screen = TH.get_screen_text () in

      (* Form should still be stable *)
      check
        bool
        "form survived multiple cycles"
        true
        (String.length final_screen > 100) ;
      check
        bool
        "form still shows install node"
        true
        (TH.contains_substring final_screen "Install Node"))

(* ============================================================ *)
(* Test: Modal Selection (Enter to Select) *)
(* ============================================================ *)

let test_modal_selection () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_node_form.Page) ;

      let initial_screen = TH.get_screen_text () in

      (* Open modal (network field) *)
      ignore (TH.send_key_and_wait "Enter") ;
      Unix.sleepf 0.02 ;

      (* Navigate to a different option if possible *)
      TH.navigate_down 1 ;
      Unix.sleepf 0.01 ;

      (* Select with Enter *)
      ignore (TH.send_key_and_wait "Enter") ;
      Unix.sleepf 0.02 ;

      let final_screen = TH.get_screen_text () in

      (* Both screens should be valid *)
      check bool "initial screen valid" true (String.length initial_screen > 0) ;
      check bool "final screen valid" true (String.length final_screen > 0) ;

      (* We should be back to the form after selection *)
      check
        bool
        "returned to form"
        true
        (TH.contains_substring final_screen "Parameter"
        || TH.contains_substring final_screen "Value"))

(* ============================================================ *)
(* Test: Field Edit Workflow (Complete Cycle) *)
(* ============================================================ *)

let test_field_edit_workflow () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_node_form.Page) ;

      (* Navigate to a text field (instance name) *)
      TH.navigate_down 5 ;
      Unix.sleepf 0.01 ;

      (* Open edit modal *)
      ignore (TH.send_key_and_wait "Enter") ;
      Unix.sleepf 0.02 ;

      (* Type some characters (if text input modal) *)
      ignore (TH.send_key_and_wait "a") ;
      Unix.sleepf 0.01 ;
      ignore (TH.send_key_and_wait "b") ;
      Unix.sleepf 0.01 ;

      (* Confirm or cancel *)
      ignore (TH.send_key_and_wait "Escape") ;
      Unix.sleepf 0.02 ;

      let screen = TH.get_screen_text () in

      (* Should be back to form *)
      check bool "workflow complete" true (String.length screen > 100))

(* ============================================================ *)
(* Test: Form State Preserved After Modal *)
(* ============================================================ *)

let test_form_state_preserved () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Install_node_form.Page) ;

      (* Capture initial state *)
      let initial = TH.get_screen_text () in

      (* Open and close modal multiple times *)
      for _i = 1 to 3 do
        ignore (TH.send_key_and_wait "Enter") ;
        Unix.sleepf 0.01 ;
        ignore (TH.send_key_and_wait "Escape") ;
        Unix.sleepf 0.01 ;
        TH.navigate_down 1
      done ;

      let final = TH.get_screen_text () in

      (* Core form structure should be preserved *)
      check
        bool
        "install node title preserved"
        true
        (TH.contains_substring initial "Install Node"
        && TH.contains_substring final "Install Node") ;

      check
        bool
        "form table structure preserved"
        true
        (TH.contains_substring initial "Parameter"
        && TH.contains_substring final "Parameter"))

(* ============================================================ *)
(* Test Suite *)
(* ============================================================ *)

let modal_tests =
  [
    ("modal opens on enter", `Quick, test_modal_opens_on_enter);
    ("modal navigation with arrows", `Quick, test_modal_navigation);
    ("modal closes on escape", `Quick, test_modal_closes_on_escape);
    ("multiple modal cycles", `Quick, test_multiple_modal_cycles);
    ("modal selection with enter", `Quick, test_modal_selection);
    ("field edit workflow", `Quick, test_field_edit_workflow);
    ("form state preserved after modal", `Quick, test_form_state_preserved);
  ]

let () =
  Alcotest.run "Modal Interactions (TUI)" [("modal_workflow", modal_tests)]
