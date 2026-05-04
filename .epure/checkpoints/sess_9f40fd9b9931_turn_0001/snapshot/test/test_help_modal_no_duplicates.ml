(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Regression test: Verify help modal never shows duplicate shortcuts.
    
    This test ensures that no shortcut key appears in BOTH the "Global
    shortcuts" section AND the "Page shortcuts" section. This was a UX
    issue where pages defined global shortcuts (Esc, ?) in their keymaps,
    causing duplicates.
    
    The fix is two-pronged:
    1. Filter in modal_helpers.ml - remove page shortcuts matching global keys
    2. Clean page keymaps - pages shouldn't define global shortcuts
    
    This test verifies (1) works correctly even if pages violate (2). *)

open Alcotest
module HD = Lib_miaou_internal.Headless_driver
module TH = Tui_test_helpers_lib.Tui_test_helpers

(** Test helper: verifies a page has no duplicate shortcuts by checking
    that common global shortcuts (Esc, ?) don't appear in Page shortcuts section *)
let test_page_no_duplicates page_module page_name () =
  TH.with_test_env (fun () ->
      HD.Stateful.init page_module ;
      ignore (HD.Stateful.idle_wait ~iterations:2 ~sleep:0.001 ()) ;

      (* Open help modal *)
      ignore (HD.Stateful.send_key "?") ;
      ignore (HD.Stateful.idle_wait ~iterations:5 ~sleep:0.001 ()) ;

      let screen = TH.get_screen_text () in

      (* Verify help modal opened *)
      if not (TH.contains_substring screen "Global shortcuts:") then
        Alcotest.fail (Printf.sprintf "[%s] Help modal did not open" page_name) ;

      (* Check for Page shortcuts section containing Esc or ? *)
      let lines = String.split_on_char '\n' screen in
      let rec check_after_page_shortcuts found_page_section = function
        | [] -> ()
        | line :: rest ->
            if TH.contains_substring line "Page shortcuts:" then
              check_after_page_shortcuts true rest
            else if found_page_section then
              (* We're in the page shortcuts section - check for duplicates *)
              if
                TH.contains_substring line "Esc"
                && TH.contains_substring line " - "
              then
                Alcotest.failf
                  "[%s] Found 'Esc' in Page shortcuts section (duplicate)"
                  page_name
              else if
                TH.contains_substring line "?"
                && TH.contains_substring line " - "
              then
                Alcotest.failf
                  "[%s] Found '?' in Page shortcuts section (duplicate)"
                  page_name
              else check_after_page_shortcuts found_page_section rest
            else check_after_page_shortcuts found_page_section rest
      in
      check_after_page_shortcuts false lines)

(** Test pages that previously had duplicate shortcuts *)
let test_wallets_no_duplicates () =
  let module Wallets_page = Octez_manager_ui.Wallets_page in
  test_page_no_duplicates (module Wallets_page.Page) "Wallets" ()

let test_instance_details_no_duplicates () =
  let module Instance_details = Octez_manager_ui.Instance_details in
  test_page_no_duplicates (module Instance_details.Page) "Instance Details" ()

let test_snapshots_no_duplicates () =
  let module Snapshots = Octez_manager_ui.Snapshots in
  test_page_no_duplicates (module Snapshots.Page) "Snapshots" ()

let test_topology_no_duplicates () =
  let module Topology_page = Octez_manager_ui.Topology_page in
  test_page_no_duplicates (module Topology_page.Page) "Topology" ()

let test_binaries_no_duplicates () =
  let module Binaries_page = Octez_manager_ui.Binaries_page in
  test_page_no_duplicates (module Binaries_page.Page) "Binaries" ()

let test_diagnostics_no_duplicates () =
  let module Diagnostics_page = Octez_manager_ui.Diagnostics_page in
  test_page_no_duplicates (module Diagnostics_page.Page) "Diagnostics" ()

let test_instances_no_duplicates () =
  let module Instances = Octez_manager_ui.Instances in
  test_page_no_duplicates (module Instances.Page) "Instances" ()

(** Test suite *)
let () =
  Alcotest.run
    "Help Modal - No Duplicates"
    [
      ( "pages_previously_with_duplicates",
        [
          test_case "wallets" `Quick test_wallets_no_duplicates;
          test_case
            "instance_details"
            `Quick
            test_instance_details_no_duplicates;
          test_case "snapshots" `Quick test_snapshots_no_duplicates;
          test_case "topology" `Quick test_topology_no_duplicates;
          test_case "binaries" `Quick test_binaries_no_duplicates;
          test_case "diagnostics" `Quick test_diagnostics_no_duplicates;
          test_case "instances" `Quick test_instances_no_duplicates;
        ] );
    ]
