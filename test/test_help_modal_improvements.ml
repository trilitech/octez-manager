(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Tests for improved help modal - will FAIL until implementation complete.
    
    These tests define the TARGET behavior after implementing the help modal
    improvements. They should PASS after all batches are complete. *)

open Alcotest
module HD = Lib_miaou_internal.Headless_driver
module TH = Tui_test_helpers_lib.Tui_test_helpers
module Instances = Octez_manager_ui.Instances
module Diagnostics_page = Octez_manager_ui.Diagnostics_page
module Wallets_page = Octez_manager_ui.Wallets_page
module Main_shell = Octez_manager_ui.Main_shell

(** Test: Help modal shows both global and page sections *)
let test_help_modal_has_both_sections () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Instances.Page) ;

      ignore (HD.Stateful.send_key "?") ;
      ignore (HD.Stateful.idle_wait ~iterations:5 ~sleep:0.001 ()) ;

      let screen = TH.get_screen_text () in

      (* MUST have global shortcuts section *)
      check
        bool
        "has 'Global shortcuts:' section"
        true
        (TH.contains_substring screen "Global shortcuts:") ;

      (* MUST have page shortcuts section *)
      check
        bool
        "has 'Page shortcuts:' section"
        true
        (TH.contains_substring screen "Page shortcuts:"))

(** Test: Instances page shows its keymap *)
let test_instances_page_shortcuts () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Instances.Page) ;

      ignore (HD.Stateful.send_key "?") ;
      ignore (HD.Stateful.idle_wait ~iterations:5 ~sleep:0.001 ()) ;

      let screen = TH.get_screen_text () in

      (* From instances.ml:339 keymap *)
      check
        bool
        "shows 'Enter' shortcut"
        true
        (TH.contains_substring screen "Enter") ;
      check
        bool
        "shows 'Open' help text"
        true
        (TH.contains_substring screen "Open") ;
      check bool "shows 'g' shortcut" true (TH.contains_substring screen "g") ;
      check
        bool
        "shows 'Group actions' help"
        true
        (TH.contains_substring screen "Group actions"))

(** Test: Diagnostics page shows its keymap *)
let test_diagnostics_page_shortcuts () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Diagnostics_page.Page) ;

      ignore (HD.Stateful.send_key "?") ;
      ignore (HD.Stateful.idle_wait ~iterations:5 ~sleep:0.001 ()) ;

      let screen = TH.get_screen_text () in

      (* From diagnostics_page.ml:171 keymap *)
      check
        bool
        "shows 'r' for Refresh"
        true
        (TH.contains_substring screen "Refresh") ;
      check
        bool
        "shows 'm' for Toggle metrics"
        true
        (TH.contains_substring screen "Toggle metrics") ;
      check
        bool
        "shows 'R' for Toggle recorder"
        true
        (TH.contains_substring screen "Toggle recorder"))

(** Test: Wallets page uses global help modal (custom help removed) *)
let test_wallets_uses_global_help () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Wallets_page.Page) ;

      ignore (HD.Stateful.send_key "?") ;
      ignore (HD.Stateful.idle_wait ~iterations:5 ~sleep:0.001 ()) ;

      let screen = TH.get_screen_text () in

      (* Should NOT show old custom title *)
      check
        bool
        "does NOT show 'Wallets Page Help'"
        false
        (TH.contains_substring screen "Wallets Page Help") ;

      (* Should show new structured help *)
      check
        bool
        "shows 'Global shortcuts:'"
        true
        (TH.contains_substring screen "Global shortcuts:") ;
      check
        bool
        "shows 'Page shortcuts:'"
        true
        (TH.contains_substring screen "Page shortcuts:") ;

      (* Should show wallets keymap entries *)
      check
        bool
        "shows 'Esc' for Back"
        true
        (TH.contains_substring screen "Back"))

(** Test: Empty keymap shows only global shortcuts *)
let test_page_without_keymap () =
  TH.with_test_env (fun () ->
      (* main_shell has empty keymap: keymap _ps = [] *)
      HD.Stateful.init (module Main_shell.Page) ;

      ignore (HD.Stateful.send_key "?") ;
      ignore (HD.Stateful.idle_wait ~iterations:5 ~sleep:0.001 ()) ;

      let screen = TH.get_screen_text () in

      (* Should show global shortcuts *)
      check
        bool
        "shows global shortcuts"
        true
        (TH.contains_substring screen "Global shortcuts:") ;

      (* Should NOT show page shortcuts section (empty) *)
      check
        bool
        "does NOT show empty page section"
        false
        (TH.contains_substring screen "Page shortcuts:"))

let () =
  Alcotest.run
    "Help Modal Improvements"
    [
      ( "new_behavior",
        [
          ("shows both sections", `Quick, test_help_modal_has_both_sections);
          ("instances page shortcuts", `Quick, test_instances_page_shortcuts);
          ("diagnostics page shortcuts", `Quick, test_diagnostics_page_shortcuts);
          ("wallets uses global help", `Quick, test_wallets_uses_global_help);
          ("page without keymap", `Quick, test_page_without_keymap);
        ] );
    ]
