(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Unit tests for main_shell Back navigation behavior.

    Verifies that when a hidden page (rewards, sandbox, diagnostics) signals
    Back, the shell clears on_hidden_page and returns to the tab bar instead
    of propagating Back to the driver (which would exit the app). *)

open Alcotest
open Octez_manager_ui
module Navigation = Miaou.Core.Navigation
module Internal = Main_shell.Internal_for_tests

(** Test that Back from a hidden page clears on_hidden_page instead of exiting *)
let test_back_from_hidden_page () =
  let shell_s = Internal.make_state ~on_hidden_page:(Some "rewards") () in
  let shell_ps = Navigation.make shell_s in
  let result =
    Internal.apply_sub_nav ~shell_ps ~shell_s (Some Navigation.Back)
  in
  (* Should clear on_hidden_page and NOT have pending Back navigation *)
  check
    (option string)
    "on_hidden_page cleared"
    None
    (Internal.get_on_hidden_page (Internal.get_state result)) ;
  check
    (option (testable Fmt.nop ( = )))
    "no pending navigation"
    None
    (Navigation.pending result)

(** Test that Back from a regular tab propagates normally *)
let test_back_from_regular_tab () =
  let shell_s = Internal.make_state ~on_hidden_page:None () in
  let shell_ps = Navigation.make shell_s in
  let result =
    Internal.apply_sub_nav ~shell_ps ~shell_s (Some Navigation.Back)
  in
  (* Should have pending Back navigation *)
  check
    (option (testable Fmt.nop ( = )))
    "pending Back navigation"
    (Some Navigation.Back)
    (Navigation.pending result)

(** Test that Back from sandbox page clears on_hidden_page *)
let test_back_from_sandbox () =
  let shell_s = Internal.make_state ~on_hidden_page:(Some "sandbox") () in
  let shell_ps = Navigation.make shell_s in
  let result =
    Internal.apply_sub_nav ~shell_ps ~shell_s (Some Navigation.Back)
  in
  check
    (option string)
    "on_hidden_page cleared"
    None
    (Internal.get_on_hidden_page (Internal.get_state result)) ;
  check
    (option (testable Fmt.nop ( = )))
    "no pending navigation"
    None
    (Navigation.pending result)

(** Test that Back from diagnostics page clears on_hidden_page *)
let test_back_from_diagnostics () =
  let shell_s = Internal.make_state ~on_hidden_page:(Some "diagnostics") () in
  let shell_ps = Navigation.make shell_s in
  let result =
    Internal.apply_sub_nav ~shell_ps ~shell_s (Some Navigation.Back)
  in
  check
    (option string)
    "on_hidden_page cleared"
    None
    (Internal.get_on_hidden_page (Internal.get_state result)) ;
  check
    (option (testable Fmt.nop ( = )))
    "no pending navigation"
    None
    (Navigation.pending result)

(** Test that None navigation result is handled correctly *)
let test_none_navigation () =
  let shell_s = Internal.make_state ~on_hidden_page:(Some "rewards") () in
  let shell_ps = Navigation.make shell_s in
  let result = Internal.apply_sub_nav ~shell_ps ~shell_s None in
  (* Should preserve on_hidden_page and have no pending navigation *)
  check
    (option string)
    "on_hidden_page preserved"
    (Some "rewards")
    (Internal.get_on_hidden_page (Internal.get_state result)) ;
  check
    (option (testable Fmt.nop ( = )))
    "no pending navigation"
    None
    (Navigation.pending result)

(** Test that Quit propagates regardless of hidden page state *)
let test_quit_from_hidden_page () =
  let shell_s = Internal.make_state ~on_hidden_page:(Some "rewards") () in
  let shell_ps = Navigation.make shell_s in
  let result =
    Internal.apply_sub_nav ~shell_ps ~shell_s (Some Navigation.Quit)
  in
  (* Should have pending Quit navigation *)
  check
    (option (testable Fmt.nop ( = )))
    "pending Quit navigation"
    (Some Navigation.Quit)
    (Navigation.pending result)

let () =
  run
    "Main_shell Back navigation"
    [
      ( "apply_sub_nav",
        [
          test_case
            "Back from hidden page (rewards) clears on_hidden_page"
            `Quick
            test_back_from_hidden_page;
          test_case
            "Back from regular tab propagates normally"
            `Quick
            test_back_from_regular_tab;
          test_case
            "Back from sandbox clears on_hidden_page"
            `Quick
            test_back_from_sandbox;
          test_case
            "Back from diagnostics clears on_hidden_page"
            `Quick
            test_back_from_diagnostics;
          test_case
            "None navigation preserves state"
            `Quick
            test_none_navigation;
          test_case
            "Quit from hidden page propagates"
            `Quick
            test_quit_from_hidden_page;
        ] );
    ]
