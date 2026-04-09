(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Regression test: Verify help modal shows ONLY shortcuts, no extra text.
    
    User reported that the help modal was showing footer hint text above the
    "Global shortcuts" section. This extra text came from Help_hint being
    read by show_help_modal.
    
    Example of unwanted text:
    "1: new instance  ←/h: By Role  →/l: By Group  g: Toggle view  K: Wa..."
    
    Fix: Removed Help_hint reading from show_help_modal. Help_hint is for:
    - Form field descriptions (contextual)
    - Choice modal item descriptions
    - Footer bar hints
    
    The help modal should ONLY show keyboard shortcuts. *)

open Alcotest
module HD = Lib_miaou_internal.Headless_driver
module TH = Tui_test_helpers_lib.Tui_test_helpers

(** Test helper: Verify help modal doesn't contain footer hint text *)
let test_help_modal_clean_content page_module page_name ~forbidden_text () =
  TH.with_test_env (fun () ->
      HD.Stateful.init page_module ;
      ignore (HD.Stateful.idle_wait ~iterations:2 ~sleep:0.001 ()) ;

      (* Open help modal *)
      ignore (HD.Stateful.send_key "?") ;
      ignore (HD.Stateful.idle_wait ~iterations:5 ~sleep:0.001 ()) ;

      let screen = TH.get_screen_text () in

      (* Verify help modal opened *)
      if not (TH.contains_substring screen "Global shortcuts:") then
        Alcotest.failf "[%s] Help modal did not open" page_name ;

      (* Verify the forbidden text (footer hints) does NOT appear *)
      List.iter
        (fun text ->
          if TH.contains_substring screen text then
            Alcotest.failf
              "[%s] Found forbidden footer hint text in help modal: %S"
              page_name
              text)
        forbidden_text)

(** Test instances page - reported to show footer hints like "new instance", "By Role", etc. *)
let test_instances_page_no_extra_text () =
  let module Instances = Octez_manager_ui.Instances in
  test_help_modal_clean_content
    (module Instances.Page)
    "Instances"
    ~forbidden_text:
      [
        "new instance";
        "By Role";
        "By Group";
        "Toggle view";
        (* These are footer hints that should NOT appear in help modal *)
      ]
    ()

(** Test binaries page - also uses Help_hint for footer *)
let test_binaries_page_no_extra_text () =
  let module Binaries_page = Octez_manager_ui.Binaries_page in
  test_help_modal_clean_content
    (module Binaries_page.Page)
    "Binaries"
    ~forbidden_text:[]
    ()

(** Test wallets page - another common page *)
let test_wallets_page_no_extra_text () =
  let module Wallets_page = Octez_manager_ui.Wallets_page in
  test_help_modal_clean_content
    (module Wallets_page.Page)
    "Wallets"
    ~forbidden_text:[]
    ()

(** Test suite *)
let () =
  Alcotest.run
    "Help Modal - Clean Content"
    [
      ( "no_extra_text_before_shortcuts",
        [
          test_case "instances" `Quick test_instances_page_no_extra_text;
          test_case "binaries" `Quick test_binaries_page_no_extra_text;
          test_case "wallets" `Quick test_wallets_page_no_extra_text;
        ] );
    ]
