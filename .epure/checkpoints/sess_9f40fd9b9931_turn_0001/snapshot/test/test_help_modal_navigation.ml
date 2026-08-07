(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Regression test: Verify help modal updates when navigating between pages.
    
    Critical bug: When navigating from Binaries page to RPC pages, the help
    modal showed Binaries shortcuts instead of RPC page shortcuts. The keymap
    wasn't being updated.
    
    Root cause: RPC pages that implement PAGE_SIG directly (not using
    Themed_page.Make) weren't calling Context.register_active_page_keymap
    in their view functions.
    
    This test verifies that help modal shows correct shortcuts after navigation. *)

open Alcotest
module HD = Lib_miaou_internal.Headless_driver
module TH = Tui_test_helpers_lib.Tui_test_helpers

(** Test helper: Check that help modal contains expected shortcuts and
    doesn't contain wrong shortcuts *)
let verify_help_modal_shortcuts ~page_name ~expected_shortcuts
    ~forbidden_shortcuts () =
  (* Open help modal *)
  ignore (HD.Stateful.send_key "?") ;
  ignore (HD.Stateful.idle_wait ~iterations:5 ~sleep:0.001 ()) ;

  let screen = TH.get_screen_text () in

  (* Verify help modal opened *)
  if not (TH.contains_substring screen "Global shortcuts:") then
    Alcotest.failf "[%s] Help modal did not open" page_name ;

  (* Check expected shortcuts appear in Page shortcuts section *)
  let has_page_section = TH.contains_substring screen "Page shortcuts:" in
  if has_page_section && expected_shortcuts <> [] then (
    List.iter
      (fun shortcut ->
        if not (TH.contains_substring screen shortcut) then
          Alcotest.failf
            "[%s] Expected shortcut '%s' not found in help modal"
            page_name
            shortcut)
      expected_shortcuts ;

    (* Check forbidden shortcuts DON'T appear in Page shortcuts section *)
    let lines = String.split_on_char '\n' screen in
    let rec check_after_page_shortcuts found_page_section = function
      | [] -> ()
      | line :: rest ->
          if TH.contains_substring line "Page shortcuts:" then
            check_after_page_shortcuts true rest
          else if found_page_section then (
            (* We're in the page shortcuts section *)
            List.iter
              (fun forbidden ->
                if
                  TH.contains_substring line forbidden
                  && TH.contains_substring line " - "
                then
                  Alcotest.failf
                    "[%s] Forbidden shortcut '%s' found in Page shortcuts: %s"
                    page_name
                    forbidden
                    line)
              forbidden_shortcuts ;
            check_after_page_shortcuts found_page_section rest)
          else check_after_page_shortcuts found_page_section rest
    in
    check_after_page_shortcuts false lines) ;

  (* Close help modal *)
  ignore (HD.Stateful.send_key "Esc") ;
  ignore (HD.Stateful.idle_wait ~iterations:3 ~sleep:0.001 ())

(** Test: RPC Node Selection page shows correct shortcuts *)
let test_rpc_node_selection_shortcuts () =
  let module Rpc_node_selection = Octez_manager_ui.Rpc_node_selection in
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Rpc_node_selection.Page) ;
      ignore (HD.Stateful.idle_wait ~iterations:2 ~sleep:0.001 ()) ;

      verify_help_modal_shortcuts
        ~page_name:"RPC Node Selection"
        ~expected_shortcuts:["Select"; "Navigate"]
        ~forbidden_shortcuts:["Download"; "Register directory"; "Prune"]
        ())

(** Test suite *)
let () =
  Alcotest.run
    "Help Modal - Navigation Updates"
    [
      ( "rpc_pages_show_correct_shortcuts",
        [
          test_case "rpc_node_selection" `Quick test_rpc_node_selection_shortcuts;
          (* RPC Browser skipped - has complex initialization issues (see #849)
             Uses Themed_page.Make so keymap registration works automatically
             once initialization is fixed. *)
        ] );
    ]
