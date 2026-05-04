(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Regression tests for global shortcuts (?, C-t, K) in pages that are
    navigated to directly without going through main_shell.

    Bug: RPC browser and log viewer did not call Global_shortcuts.handle in
    their handle_key functions, so '?', 'C-t', and 'K' were silently swallowed
    instead of opening the help modal / theme picker / key bindings modal. *)

open Alcotest
module HD = Lib_miaou_internal.Headless_driver
module TH = Tui_test_helpers_lib.Tui_test_helpers
module Rpc_browser = Octez_manager_ui.Rpc_browser
module Log_viewer = Octez_manager_ui.Log_viewer_page

(* ============================================================ *)
(* RPC Browser — global shortcuts                               *)
(* ============================================================ *)

let test_rpc_browser_help_modal () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Rpc_browser.Page) ;

      (* RPC browser may open a modal on init (e.g. no instances available).
         Dismiss it with Escape before testing global shortcuts. *)
      if Miaou.Core.Modal_manager.has_active () then (
        ignore (HD.Stateful.send_key "Escape") ;
        ignore (TH.wait_until_no_modal ())) ;

      (* Press '?' — global shortcut, must open help modal *)
      ignore (HD.Stateful.send_key "?") ;
      ignore (TH.wait_until_modal_active ()) ;

      check
        bool
        "help modal opens with '?' in rpc browser"
        true
        (Miaou.Core.Modal_manager.has_active ()))

(* ============================================================ *)
(* Log viewer — global shortcuts (pager not in input mode)      *)
(* ============================================================ *)

let test_log_viewer_help_modal () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Log_viewer.Page) ;

      check
        bool
        "no modal initially"
        false
        (Miaou.Core.Modal_manager.has_active ()) ;

      (* Press '?' — must open help modal when pager is not in search mode *)
      ignore (HD.Stateful.send_key "?") ;
      ignore (TH.wait_until_modal_active ()) ;

      check
        bool
        "help modal opens with '?' in log viewer"
        true
        (Miaou.Core.Modal_manager.has_active ()))

(* ============================================================ *)
(* Test Suite                                                   *)
(* ============================================================ *)

let rpc_browser_tests =
  [("help modal opens with '?'", `Quick, test_rpc_browser_help_modal)]

let log_viewer_tests =
  [("help modal opens with '?'", `Quick, test_log_viewer_help_modal)]

let () =
  Alcotest.run
    "Global shortcuts in direct-navigation pages"
    [("rpc_browser", rpc_browser_tests); ("log_viewer", log_viewer_tests)]
