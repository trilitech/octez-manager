(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Headless TUI tests for the "Add custom baker" modal flow on the Rewards page.

    Coverage:
    1. Pressing 'a' opens the first modal (baker-PKH prompt).
    2. A KT1 address is rejected by the PKH validator (modal stays open with
       an error indicator; the next prompt is not shown).
    3. The ['a'] keybinding is listed in handled_keys.

    Limits:
    - Driving the full 6-step flow through to [Custom_baker_registry.add] is
      not covered here because step 7 calls
      [Custom_baker_registry.resolve_octez_client_bin], which requires a real
      or stubbed [octez-client] binary on PATH.  The end-to-end path is
      exercised by [test_custom_baker_registry.ml] at the library level.
    - Collision detection is already covered by [test_custom_baker_registry.ml]. *)

open Alcotest
module HD = Lib_miaou_internal.Headless_driver
module TH = Tui_test_helpers_lib.Tui_test_helpers
module Rewards = Octez_manager_ui.Rewards_page
module Modal_manager = Miaou.Core.Modal_manager

(* ============================================================ *)
(* Test: 'a' keybinding is declared in handled_keys             *)
(* ============================================================ *)

let test_a_in_handled_keys () =
  let keys = Rewards.Page.handled_keys () in
  let has_a =
    List.exists (fun k -> String.equal (Miaou.Core.Keys.to_string k) "a") keys
  in
  check bool "'a' is in handled_keys" true has_a

(* ============================================================ *)
(* Test: pressing 'a' opens the first modal                     *)
(* ============================================================ *)

let test_press_a_opens_modal () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Rewards.Page) ;

      (* Dismiss any modal that may have appeared on init *)
      if Modal_manager.has_active () then (
        ignore (HD.Stateful.send_key "Escape") ;
        ignore (TH.wait_until_no_modal ())) ;

      (* Send 'a' — should open the baker-PKH prompt *)
      ignore (TH.send_key_and_wait "a") ;
      ignore (TH.wait_until_modal_active ()) ;

      check bool "modal opened after 'a'" true (Modal_manager.has_active ()) ;

      (* The screen should contain the first prompt title *)
      let screen = TH.get_screen_text () in
      check
        bool
        "first prompt title visible"
        true
        (TH.contains_substring screen "Baker PKH"
        || TH.contains_substring screen "Custom Baker"
        || TH.contains_substring screen "Add Custom") ;

      (* Clean up: dismiss modal *)
      ignore (HD.Stateful.send_key "Escape") ;
      ignore (TH.wait_until_no_modal ()))

(* ============================================================ *)
(* Test: KT1 address is rejected at the PKH validation step     *)
(* ============================================================ *)

let test_kt1_pkh_is_rejected () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Rewards.Page) ;

      (* Dismiss any startup modal *)
      if Modal_manager.has_active () then (
        ignore (HD.Stateful.send_key "Escape") ;
        ignore (TH.wait_until_no_modal ())) ;

      (* Open the add-baker modal *)
      ignore (TH.send_key_and_wait "a") ;
      ignore (TH.wait_until_modal_active ()) ;
      check bool "modal is open" true (Modal_manager.has_active ()) ;

      (* Type a KT1 address (36 chars but not a baker PKH) *)
      let kt1_addr = "KT1BEqzn5Wx8uJrZNvuS9DVHmLvG9td3fDLi" in
      TH.type_string kt1_addr ;
      ignore (HD.Stateful.idle_wait ~iterations:5 ~sleep:0.0 ()) ;

      (* Press Enter — validator should reject it *)
      ignore (TH.send_key_and_wait "Enter") ;
      ignore (HD.Stateful.idle_wait ~iterations:5 ~sleep:0.0 ()) ;

      (* Modal must still be active: KT1 was rejected *)
      check
        bool
        "modal still open after KT1 rejection"
        true
        (Modal_manager.has_active ()) ;

      (* The page should not have advanced to the next prompt *)
      let screen = TH.get_screen_text () in
      check
        bool
        "screen does not show network prompt"
        false
        (TH.contains_substring screen "2/6") ;

      (* Clean up *)
      ignore (HD.Stateful.send_key "Escape") ;
      ignore (TH.wait_until_no_modal ()))

(* ============================================================ *)
(* Test: Esc dismisses the modal mid-flow                       *)
(* ============================================================ *)

let test_esc_cancels_modal () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Rewards.Page) ;

      if Modal_manager.has_active () then (
        ignore (HD.Stateful.send_key "Escape") ;
        ignore (TH.wait_until_no_modal ())) ;

      ignore (TH.send_key_and_wait "a") ;
      ignore (TH.wait_until_modal_active ()) ;
      check bool "modal opened" true (Modal_manager.has_active ()) ;

      ignore (TH.send_key_and_wait "Escape") ;
      ignore (TH.wait_until_no_modal ()) ;

      check bool "modal closed after Esc" false (Modal_manager.has_active ()))

(* ============================================================ *)
(* Entry point                                                  *)
(* ============================================================ *)

let () =
  Alcotest.run
    "rewards_page_custom_baker"
    [
      ( "keybinding",
        [
          Alcotest.test_case
            "'a' is in handled_keys"
            `Quick
            test_a_in_handled_keys;
        ] );
      ( "modal_flow",
        [
          Alcotest.test_case
            "pressing 'a' opens first modal"
            `Quick
            test_press_a_opens_modal;
          Alcotest.test_case
            "KT1 address is rejected"
            `Quick
            test_kt1_pkh_is_rejected;
          Alcotest.test_case "Esc cancels modal" `Quick test_esc_cancels_modal;
        ] );
    ]
