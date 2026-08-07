(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Baseline tests for help modal - captures current behavior before improvements.
    
    These tests document the CURRENT state of the help modal (before implementation).
    They should all PASS, establishing a baseline for regression testing. *)

open Alcotest
module HD = Lib_miaou_internal.Headless_driver
module TH = Tui_test_helpers_lib.Tui_test_helpers
module Instances = Octez_manager_ui.Instances
module Wallets_page = Octez_manager_ui.Wallets_page

(** Test: Current help modal shows only global shortcuts *)
let test_current_help_modal_global_only () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Instances.Page) ;

      (* Open help modal *)
      ignore (HD.Stateful.send_key "?") ;
      ignore (HD.Stateful.idle_wait ~iterations:5 ~sleep:0.001 ()) ;

      let screen = TH.get_screen_text () in

      (* Current behavior: shows global shortcuts *)
      check
        bool
        "shows 'Global shortcuts:'"
        true
        (TH.contains_substring screen "Global shortcuts:") ;
      check bool "shows '?' key" true (TH.contains_substring screen "?") ;
      check bool "shows 'Esc/q'" true (TH.contains_substring screen "Esc") ;

      (* Current behavior: does NOT show page-specific shortcuts *)
      check
        bool
        "does NOT show 'Page shortcuts:'"
        false
        (TH.contains_substring screen "Page shortcuts:") ;
      check
        bool
        "does NOT show Enter key"
        false
        (TH.contains_substring screen "Enter"))

(** Test: Wallets page has custom help implementation *)
let test_wallets_custom_help () =
  TH.with_test_env (fun () ->
      (* Wallets page has custom help - this will be removed in Batch 5 *)
      HD.Stateful.init (module Wallets_page.Page) ;

      ignore (HD.Stateful.send_key "?") ;
      ignore (HD.Stateful.idle_wait ~iterations:5 ~sleep:0.001 ()) ;

      let screen = TH.get_screen_text () in

      (* Wallets shows custom help with page shortcuts *)
      check
        bool
        "wallets shows j/Down"
        true
        (TH.contains_substring screen "j/Down") ;
      check
        bool
        "wallets shows custom title"
        true
        (TH.contains_substring screen "Wallets Page Help"))

let () =
  Alcotest.run
    "Help Modal Baseline"
    [
      ( "baseline_behavior",
        [
          ("global shortcuts only", `Quick, test_current_help_modal_global_only);
          ("wallets custom help", `Quick, test_wallets_custom_help);
        ] );
    ]
