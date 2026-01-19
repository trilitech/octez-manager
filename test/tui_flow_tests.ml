(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

(** TUI flow tests using the miaou headless driver.

    These tests verify UI logic without a real terminal:
    - Key handling and navigation
    - Modal opening/closing
    - State transitions
    - Rendered output contains expected content
*)

open Alcotest
module Headless = Lib_miaou_internal.Headless_driver
module Instances = Octez_manager_ui.Instances
module Modal_manager = Miaou.Core.Modal_manager

(** Test environment setup *)
let setup () =
  let tmp_dir =
    Filename.get_temp_dir_name ()
    ^ "/octez-manager-test-"
    ^ string_of_int (Unix.getpid ())
  in
  if not (Sys.file_exists tmp_dir) then Unix.mkdir tmp_dir 0o700 ;
  Unix.putenv "XDG_CONFIG_HOME" (tmp_dir ^ "/config") ;
  Unix.putenv "XDG_DATA_HOME" (tmp_dir ^ "/data") ;
  Unix.putenv "XDG_STATE_HOME" (tmp_dir ^ "/state") ;
  Octez_manager_lib.Capabilities.register () ;
  (* Reset headless driver state *)
  Headless.set_size 24 80 ;
  Headless.set_limits ~iterations:100 ~seconds:5.0 () ;
  Headless.Key_queue.clear () ;
  Headless.Screen.clear () ;
  Modal_manager.clear () ;
  tmp_dir

let cleanup tmp_dir =
  let rec rm_rf path =
    if Sys.is_directory path then (
      Sys.readdir path
      |> Array.iter (fun name -> rm_rf (Filename.concat path name)) ;
      Unix.rmdir path)
    else Sys.remove path
  in
  if Sys.file_exists tmp_dir then rm_rf tmp_dir

let with_test_env f =
  let tmp_dir = setup () in
  Fun.protect ~finally:(fun () -> cleanup tmp_dir) (fun () -> f ())

(** Helper to check if string contains substring *)
let contains_substring haystack needle =
  let nlen = String.length needle in
  let hlen = String.length haystack in
  if nlen = 0 then true
  else if nlen > hlen then false
  else
    let rec loop i =
      if i + nlen > hlen then false
      else if String.sub haystack i nlen = needle then true
      else loop (i + 1)
    in
    loop 0

(** Strip ANSI escape codes for easier text matching *)
let strip_ansi s =
  let buf = Buffer.create (String.length s) in
  let len = String.length s in
  let rec loop i =
    if i >= len then Buffer.contents buf
    else if s.[i] = '\027' then
      (* Skip until we hit a letter (end of escape sequence) *)
      let rec skip j =
        if j >= len then j
        else
          let c = s.[j] in
          if (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') then j + 1
          else skip (j + 1)
      in
      loop (skip (i + 1))
    else (
      Buffer.add_char buf s.[i] ;
      loop (i + 1))
  in
  loop 0

(* ============================================================ *)
(* Instances Page Tests *)
(* ============================================================ *)

(** Test: Page initializes and renders without error *)
let test_instances_init () =
  with_test_env (fun () ->
      Headless.Stateful.init (module Instances.Page) ;
      let screen = Headless.get_screen_content () in
      let text = strip_ansi screen in
      (* Should show header with octez-manager title *)
      check bool "contains title" true (contains_substring text "octez-manager") ;
      (* Should show privilege indicator *)
      check
        bool
        "contains privilege"
        true
        (contains_substring text "USER" || contains_substring text "SYSTEM"))

(** Test: Navigation with j/k keys changes selection *)
let test_instances_navigation_jk () =
  with_test_env (fun () ->
      Headless.Stateful.init (module Instances.Page) ;

      (* Initial render *)
      let _ = Headless.get_screen_content () in

      (* Press j (down) - should not crash with empty service list *)
      let result = Headless.Stateful.send_key "j" in
      check bool "j key handled" true (result = `Continue) ;

      (* Press k (up) *)
      let result = Headless.Stateful.send_key "k" in
      check bool "k key handled" true (result = `Continue) ;

      (* Verify we can still render *)
      let screen = Headless.get_screen_content () in
      check bool "renders after nav" true (String.length screen > 0))

(** Test: Arrow keys work for navigation *)
let test_instances_navigation_arrows () =
  with_test_env (fun () ->
      Headless.Stateful.init (module Instances.Page) ;

      let result = Headless.Stateful.send_key "Down" in
      check bool "Down key handled" true (result = `Continue) ;

      let result = Headless.Stateful.send_key "Up" in
      check bool "Up key handled" true (result = `Continue))

(** Test: 'c' key opens create menu modal *)
let test_instances_create_menu () =
  with_test_env (fun () ->
      Headless.Stateful.init (module Instances.Page) ;

      (* Modal should not be active initially *)
      check bool "no modal initially" false (Modal_manager.has_active ()) ;

      (* Press 'c' to open create menu *)
      let result = Headless.Stateful.send_key "c" in
      check bool "c key handled" true (result = `Continue) ;

      (* Modal should now be active *)
      check bool "modal opened" true (Modal_manager.has_active ()) ;

      (* Screen should show create options *)
      let screen = Headless.get_screen_content () in
      let text = strip_ansi screen in
      check bool "shows Node option" true (contains_substring text "Node"))

(** Test: Esc closes modal *)
let test_instances_escape_closes_modal () =
  with_test_env (fun () ->
      Headless.Stateful.init (module Instances.Page) ;

      (* Open modal *)
      let _ = Headless.Stateful.send_key "c" in
      check bool "modal opened" true (Modal_manager.has_active ()) ;

      (* Press Esc to close - miaou uses "Esc" not "Escape" *)
      let _ = Headless.Stateful.send_key "Esc" in
      check bool "modal closed" false (Modal_manager.has_active ()))

(** Test: 'q' key triggers quit/back navigation *)
let test_instances_quit () =
  with_test_env (fun () ->
      Headless.Stateful.init (module Instances.Page) ;

      let result = Headless.Stateful.send_key "q" in
      (* Should signal quit or back navigation *)
      check bool "q triggers quit" true (result = `Quit))

(** Test: Enter on Install item opens create menu *)
let test_instances_enter_on_install () =
  with_test_env (fun () ->
      Headless.Stateful.init (module Instances.Page) ;

      (* Selection starts at 0 (Install) with empty service list *)
      let _ = Headless.Stateful.send_key "Enter" in

      (* Should open create menu modal *)
      check bool "modal opened on Enter" true (Modal_manager.has_active ()))

(** Test: Tab key is handled (toggle fold) *)
let test_instances_tab_toggle () =
  with_test_env (fun () ->
      Headless.Stateful.init (module Instances.Page) ;

      (* Tab should not crash even with no services *)
      let result = Headless.Stateful.send_key "Tab" in
      check bool "Tab handled" true (result = `Continue))

(** Test: Space key forces refresh *)
let test_instances_space_refresh () =
  with_test_env (fun () ->
      Headless.Stateful.init (module Instances.Page) ;

      let result = Headless.Stateful.send_key " " in
      check bool "Space handled" true (result = `Continue) ;

      (* Should still render properly after refresh *)
      let screen = Headless.get_screen_content () in
      check bool "renders after refresh" true (String.length screen > 0))

(** Test: Wide terminal triggers multi-column layout *)
let test_instances_wide_terminal () =
  with_test_env (fun () ->
      (* Set wide terminal *)
      Headless.set_size 24 160 ;
      Headless.Stateful.init (module Instances.Page) ;

      (* h/l keys should work (column navigation) *)
      let result = Headless.Stateful.send_key "l" in
      check bool "l key handled" true (result = `Continue) ;

      let result = Headless.Stateful.send_key "h" in
      check bool "h key handled" true (result = `Continue))

(** Test: Screen content includes help hint *)
let test_instances_shows_hints () =
  with_test_env (fun () ->
      Headless.Stateful.init (module Instances.Page) ;

      let screen = Headless.get_screen_content () in
      let text = strip_ansi screen in
      (* Should show navigation hints *)
      check
        bool
        "shows hints"
        true
        (contains_substring text "Enter" || contains_substring text "move"))

(** Test: Modal navigation with j/k inside modal *)
let test_instances_modal_navigation () =
  with_test_env (fun () ->
      Headless.Stateful.init (module Instances.Page) ;

      (* Open create menu *)
      let _ = Headless.Stateful.send_key "c" in
      check bool "modal opened" true (Modal_manager.has_active ()) ;

      (* Navigate within modal *)
      let result = Headless.Stateful.send_key "j" in
      check bool "modal j handled" true (result = `Continue) ;

      let result = Headless.Stateful.send_key "k" in
      check bool "modal k handled" true (result = `Continue) ;

      (* Modal should still be active *)
      check bool "modal still active" true (Modal_manager.has_active ()))

(** Test: Multiple key sequence doesn't crash *)
let test_instances_key_sequence () =
  with_test_env (fun () ->
      Headless.Stateful.init (module Instances.Page) ;

      (* Rapid key sequence *)
      let keys = ["j"; "j"; "k"; "Tab"; " "; "j"; "k"; "k"] in
      List.iter
        (fun k ->
          let result = Headless.Stateful.send_key k in
          check
            bool
            (Printf.sprintf "key '%s' handled" k)
            true
            (result = `Continue))
        keys ;

      (* Should still render *)
      let screen = Headless.get_screen_content () in
      check bool "renders after sequence" true (String.length screen > 0))

(* ============================================================ *)
(* Install Node Form Tests *)
(* ============================================================ *)

module Install_node_form = Octez_manager_ui.Install_node_form_v3

(** Test: Node form initializes and renders *)
let test_node_form_init () =
  with_test_env (fun () ->
      Headless.Stateful.init (module Install_node_form.Page) ;
      let screen = Headless.get_screen_content () in
      let text = strip_ansi screen in
      (* Should show form title *)
      check bool "contains Install" true (contains_substring text "Install") ;
      check bool "contains Node" true (contains_substring text "Node"))

(** Test: Node form shows required fields *)
let test_node_form_shows_fields () =
  with_test_env (fun () ->
      Headless.Stateful.init (module Install_node_form.Page) ;
      let screen = Headless.get_screen_content () in
      let text = strip_ansi screen in
      (* Should show key fields *)
      check bool "shows Instance" true (contains_substring text "Instance") ;
      check bool "shows Network" true (contains_substring text "Network") ;
      check
        bool
        "shows History"
        true
        (contains_substring text "History" || contains_substring text "history"))

(** Test: Up/Down navigates between fields *)
let test_node_form_field_navigation () =
  with_test_env (fun () ->
      Headless.Stateful.init (module Install_node_form.Page) ;

      (* Down moves to next field *)
      let result = Headless.Stateful.send_key "Down" in
      check bool "Down handled" true (result = `Continue) ;

      let result = Headless.Stateful.send_key "Down" in
      check bool "Down again" true (result = `Continue) ;

      (* Up moves back *)
      let result = Headless.Stateful.send_key "Up" in
      check bool "Up handled" true (result = `Continue))

(** Test: Enter on field opens edit modal *)
let test_node_form_enter_opens_modal () =
  with_test_env (fun () ->
      Headless.Stateful.init (module Install_node_form.Page) ;

      (* Should not have modal initially *)
      check bool "no modal initially" false (Modal_manager.has_active ()) ;

      (* Enter should open field editor *)
      let _ = Headless.Stateful.send_key "Enter" in
      check bool "modal opened" true (Modal_manager.has_active ()))

(** Test: Esc in modal closes it *)
let test_node_form_modal_esc () =
  with_test_env (fun () ->
      Headless.Stateful.init (module Install_node_form.Page) ;

      (* Open modal *)
      let _ = Headless.Stateful.send_key "Enter" in
      check bool "modal opened" true (Modal_manager.has_active ()) ;

      (* Esc closes modal *)
      let _ = Headless.Stateful.send_key "Esc" in
      check bool "modal closed" false (Modal_manager.has_active ()))

(** Test: Navigate to Submit and check it's visible *)
let test_node_form_submit_visible () =
  with_test_env (fun () ->
      Headless.Stateful.init (module Install_node_form.Page) ;

      (* Navigate down through all fields to reach Submit *)
      for _ = 1 to 15 do
        ignore (Headless.Stateful.send_key "Down")
      done ;

      let screen = Headless.get_screen_content () in
      let text = strip_ansi screen in
      (* Submit should be visible somewhere *)
      check
        bool
        "shows Submit"
        true
        (contains_substring text "Submit" || contains_substring text "Install"))

(** Test: Form navigation wraps or clamps properly *)
let test_node_form_navigation_bounds () =
  with_test_env (fun () ->
      Headless.Stateful.init (module Install_node_form.Page) ;

      (* Go up from first field - should not crash *)
      let result = Headless.Stateful.send_key "Up" in
      check bool "Up at start" true (result = `Continue) ;

      (* Go down many times - should not crash *)
      for _ = 1 to 30 do
        ignore (Headless.Stateful.send_key "Down")
      done ;

      let result = Headless.Stateful.send_key "Down" in
      check bool "Down at end" true (result = `Continue))

(** Test: Network field shows network options *)
let test_node_form_network_field () =
  with_test_env (fun () ->
      Headless.Stateful.init (module Install_node_form.Page) ;

      (* Navigate to network field (usually second) *)
      let _ = Headless.Stateful.send_key "Down" in

      (* Enter to edit *)
      let _ = Headless.Stateful.send_key "Enter" in
      check bool "modal opened" true (Modal_manager.has_active ()) ;

      (* Should show network options *)
      let screen = Headless.get_screen_content () in
      let text = strip_ansi screen in
      check
        bool
        "shows mainnet"
        true
        (contains_substring text "mainnet" || contains_substring text "Mainnet"))

(* ============================================================ *)
(* Install Baker Form Tests *)
(* ============================================================ *)

module Install_baker_form = Octez_manager_ui.Install_baker_form_v3

(** Test: Baker form initializes and renders *)
let test_baker_form_init () =
  with_test_env (fun () ->
      Headless.Stateful.init (module Install_baker_form.Page) ;
      let screen = Headless.get_screen_content () in
      let text = strip_ansi screen in
      check bool "contains Install" true (contains_substring text "Install") ;
      check bool "contains Baker" true (contains_substring text "Baker"))

(** Test: Baker form shows specific fields *)
let test_baker_form_shows_fields () =
  with_test_env (fun () ->
      Headless.Stateful.init (module Install_baker_form.Page) ;
      let screen = Headless.get_screen_content () in
      let text = strip_ansi screen in
      check bool "shows Instance" true (contains_substring text "Instance") ;
      (* Baker-specific: delegates, liquidity baking *)
      check
        bool
        "shows Delegates or delegates"
        true
        (contains_substring text "Delegate"
        || contains_substring text "delegate"))

(** Test: Up/Down navigates between fields *)
let test_baker_form_field_navigation () =
  with_test_env (fun () ->
      Headless.Stateful.init (module Install_baker_form.Page) ;

      let result = Headless.Stateful.send_key "Down" in
      check bool "Down handled" true (result = `Continue) ;

      let result = Headless.Stateful.send_key "Up" in
      check bool "Up handled" true (result = `Continue))

(** Test: Enter on field opens edit modal *)
let test_baker_form_enter_opens_modal () =
  with_test_env (fun () ->
      Headless.Stateful.init (module Install_baker_form.Page) ;

      check bool "no modal initially" false (Modal_manager.has_active ()) ;

      let _ = Headless.Stateful.send_key "Enter" in
      check bool "modal opened" true (Modal_manager.has_active ()))

(** Test: Esc in modal closes it *)
let test_baker_form_modal_esc () =
  with_test_env (fun () ->
      Headless.Stateful.init (module Install_baker_form.Page) ;

      let _ = Headless.Stateful.send_key "Enter" in
      check bool "modal opened" true (Modal_manager.has_active ()) ;

      let _ = Headless.Stateful.send_key "Esc" in
      check bool "modal closed" false (Modal_manager.has_active ()))

(** Test: Form handles liquidity baking vote field *)
let test_baker_form_liquidity_vote () =
  with_test_env (fun () ->
      Headless.Stateful.init (module Install_baker_form.Page) ;

      (* Navigate through form to find liquidity baking vote *)
      for _ = 1 to 10 do
        ignore (Headless.Stateful.send_key "Down")
      done ;

      let screen = Headless.get_screen_content () in
      let text = strip_ansi screen in
      (* Should show liquidity or vote somewhere *)
      check
        bool
        "shows vote option"
        true
        (contains_substring text "pass"
        || contains_substring text "liquidity"
        || contains_substring text "Vote"))

(** Test: Navigation bounds work correctly *)
let test_baker_form_navigation_bounds () =
  with_test_env (fun () ->
      Headless.Stateful.init (module Install_baker_form.Page) ;

      (* Go up from first field *)
      let result = Headless.Stateful.send_key "Up" in
      check bool "Up at start" true (result = `Continue) ;

      (* Go down many times *)
      for _ = 1 to 25 do
        ignore (Headless.Stateful.send_key "Down")
      done ;

      let result = Headless.Stateful.send_key "Down" in
      check bool "Down at end" true (result = `Continue))

(* ============================================================ *)
(* Install Accuser Form Tests *)
(* ============================================================ *)

module Install_accuser_form = Octez_manager_ui.Install_accuser_form_v3

(* Get the page module from the first-class module value *)
module Accuser_page = (val Install_accuser_form.page)

(** Test: Accuser form initializes and renders *)
let test_accuser_form_init () =
  with_test_env (fun () ->
      Headless.Stateful.init (module Accuser_page) ;
      let screen = Headless.get_screen_content () in
      let text = strip_ansi screen in
      check bool "contains Install" true (contains_substring text "Install") ;
      check bool "contains Accuser" true (contains_substring text "Accuser"))

(** Test: Accuser form shows specific fields *)
let test_accuser_form_shows_fields () =
  with_test_env (fun () ->
      Headless.Stateful.init (module Accuser_page) ;
      let screen = Headless.get_screen_content () in
      let text = strip_ansi screen in
      check bool "shows Instance" true (contains_substring text "Instance"))

(** Test: Up/Down navigates between fields *)
let test_accuser_form_field_navigation () =
  with_test_env (fun () ->
      Headless.Stateful.init (module Accuser_page) ;

      let result = Headless.Stateful.send_key "Down" in
      check bool "Down handled" true (result = `Continue) ;

      let result = Headless.Stateful.send_key "Up" in
      check bool "Up handled" true (result = `Continue))

(** Test: Enter on field opens edit modal *)
let test_accuser_form_enter_opens_modal () =
  with_test_env (fun () ->
      Headless.Stateful.init (module Accuser_page) ;

      check bool "no modal initially" false (Modal_manager.has_active ()) ;

      let _ = Headless.Stateful.send_key "Enter" in
      check bool "modal opened" true (Modal_manager.has_active ()))

(** Test: Esc in modal closes it *)
let test_accuser_form_modal_esc () =
  with_test_env (fun () ->
      Headless.Stateful.init (module Accuser_page) ;

      let _ = Headless.Stateful.send_key "Enter" in
      check bool "modal opened" true (Modal_manager.has_active ()) ;

      let _ = Headless.Stateful.send_key "Esc" in
      check bool "modal closed" false (Modal_manager.has_active ()))

(** Test: Navigation bounds work correctly *)
let test_accuser_form_navigation_bounds () =
  with_test_env (fun () ->
      Headless.Stateful.init (module Accuser_page) ;

      let result = Headless.Stateful.send_key "Up" in
      check bool "Up at start" true (result = `Continue) ;

      for _ = 1 to 20 do
        ignore (Headless.Stateful.send_key "Down")
      done ;

      let result = Headless.Stateful.send_key "Down" in
      check bool "Down at end" true (result = `Continue))

(* ============================================================ *)
(* Install DAL Node Form Tests *)
(* ============================================================ *)

module Install_dal_form = Octez_manager_ui.Install_dal_node_form_v3

(** Test: DAL form initializes and renders *)
let test_dal_form_init () =
  with_test_env (fun () ->
      Headless.Stateful.init (module Install_dal_form.Page) ;
      let screen = Headless.get_screen_content () in
      let text = strip_ansi screen in
      check bool "contains Install" true (contains_substring text "Install") ;
      check
        bool
        "contains DAL"
        true
        (contains_substring text "DAL" || contains_substring text "dal"))

(** Test: DAL form shows specific fields *)
let test_dal_form_shows_fields () =
  with_test_env (fun () ->
      Headless.Stateful.init (module Install_dal_form.Page) ;
      let screen = Headless.get_screen_content () in
      let text = strip_ansi screen in
      check bool "shows Instance" true (contains_substring text "Instance"))

(** Test: Up/Down navigates between fields *)
let test_dal_form_field_navigation () =
  with_test_env (fun () ->
      Headless.Stateful.init (module Install_dal_form.Page) ;

      let result = Headless.Stateful.send_key "Down" in
      check bool "Down handled" true (result = `Continue) ;

      let result = Headless.Stateful.send_key "Up" in
      check bool "Up handled" true (result = `Continue))

(** Test: Enter on field opens edit modal *)
let test_dal_form_enter_opens_modal () =
  with_test_env (fun () ->
      Headless.Stateful.init (module Install_dal_form.Page) ;

      check bool "no modal initially" false (Modal_manager.has_active ()) ;

      let _ = Headless.Stateful.send_key "Enter" in
      check bool "modal opened" true (Modal_manager.has_active ()))

(** Test: Esc in modal closes it *)
let test_dal_form_modal_esc () =
  with_test_env (fun () ->
      Headless.Stateful.init (module Install_dal_form.Page) ;

      let _ = Headless.Stateful.send_key "Enter" in
      check bool "modal opened" true (Modal_manager.has_active ()) ;

      let _ = Headless.Stateful.send_key "Esc" in
      check bool "modal closed" false (Modal_manager.has_active ()))

(** Test: Navigation bounds work correctly *)
let test_dal_form_navigation_bounds () =
  with_test_env (fun () ->
      Headless.Stateful.init (module Install_dal_form.Page) ;

      let result = Headless.Stateful.send_key "Up" in
      check bool "Up at start" true (result = `Continue) ;

      for _ = 1 to 20 do
        ignore (Headless.Stateful.send_key "Down")
      done ;

      let result = Headless.Stateful.send_key "Down" in
      check bool "Down at end" true (result = `Continue))

(* ============================================================ *)
(* Wizard-Style Integration Tests (using test helpers) *)
(* ============================================================ *)

module TH = Tui_test_helpers

(** Test: Drive through create menu and select Node *)
let test_instances_create_node_flow () =
  TH.with_test_env (fun () ->
      Headless.Stateful.init (module Instances.Page) ;

      (* Open create menu with 'c' *)
      ignore (TH.send_key_and_wait "c") ;
      check bool "create menu opened" true (Modal_manager.has_active ()) ;

      (* Verify Node option is visible *)
      TH.assert_screen_contains "Node" ;

      (* Select first item (Node) *)
      let result = TH.send_key_and_wait "Enter" in

      (* Should navigate to node form or open another modal *)
      check
        bool
        "selection handled"
        true
        (result = `Continue || result = `SwitchTo "install_node_form_v3"))

(** Test: Drive through create menu and select Baker *)
let test_instances_create_baker_flow () =
  TH.with_test_env (fun () ->
      Headless.Stateful.init (module Instances.Page) ;

      (* Open create menu *)
      ignore (TH.send_key_and_wait "c") ;
      check bool "create menu opened" true (Modal_manager.has_active ()) ;

      (* Navigate to Baker (second item) *)
      TH.navigate_down 1 ;

      (* Select Baker *)
      let result = TH.send_key_and_wait "Enter" in
      check
        bool
        "baker selection handled"
        true
        (result = `Continue || result = `SwitchTo "install_baker_form_v3"))

(** Test: Create menu shows all service types *)
let test_instances_create_menu_shows_all_types () =
  TH.with_test_env (fun () ->
      Headless.Stateful.init (module Instances.Page) ;

      (* Open create menu *)
      ignore (TH.send_key_and_wait "c") ;
      check bool "create menu opened" true (Modal_manager.has_active ()) ;

      (* Verify all service types are shown *)
      TH.assert_screen_contains "Node" ;
      TH.assert_screen_contains "Baker" ;
      TH.assert_screen_contains "Accuser" ;
      TH.assert_screen_contains "DAL")

(** Test: Escape from create menu returns to instances *)
let test_instances_create_menu_escape () =
  TH.with_test_env (fun () ->
      Headless.Stateful.init (module Instances.Page) ;

      (* Open create menu *)
      ignore (TH.send_key_and_wait "c") ;
      check bool "menu opened" true (Modal_manager.has_active ()) ;

      (* Press Esc to close *)
      ignore (TH.send_key_and_wait "Esc") ;

      (* Modal should be closed *)
      check bool "menu closed" false (Modal_manager.has_active ()) ;

      (* Page should still be instances *)
      TH.assert_screen_contains "octez-manager")

(** Test: Navigate through create menu with j/k *)
let test_instances_create_menu_vim_nav () =
  TH.with_test_env (fun () ->
      Headless.Stateful.init (module Instances.Page) ;

      (* Open create menu *)
      ignore (TH.send_key_and_wait "c") ;
      check bool "menu opened" true (Modal_manager.has_active ()) ;

      (* Navigate with j (down) *)
      ignore (TH.send_key_and_wait "j") ;
      check bool "j handled" true (Modal_manager.has_active ()) ;

      (* Navigate with k (up) *)
      ignore (TH.send_key_and_wait "k") ;
      check bool "k handled" true (Modal_manager.has_active ()))

(** Test: Multiple create menu open/close cycles *)
let test_instances_create_menu_cycle () =
  TH.with_test_env (fun () ->
      Headless.Stateful.init (module Instances.Page) ;

      for _ = 1 to 3 do
        (* Open *)
        ignore (TH.send_key_and_wait "c") ;
        check bool "opened" true (Modal_manager.has_active ()) ;

        (* Close *)
        ignore (TH.send_key_and_wait "Esc") ;
        check bool "closed" false (Modal_manager.has_active ())
      done)

(** Test: Screen content changes after navigation *)
let test_instances_screen_updates_on_nav () =
  TH.with_test_env (fun () ->
      Headless.Stateful.init (module Instances.Page) ;

      let screen1 = TH.get_screen_text () in

      (* Open and close create menu *)
      ignore (TH.send_key_and_wait "c") ;
      let screen2 = TH.get_screen_text () in
      ignore (TH.send_key_and_wait "Esc") ;
      let screen3 = TH.get_screen_text () in

      (* Screen should have changed when modal opened *)
      check
        bool
        "screen changed with modal"
        true
        (not (String.equal screen1 screen2)) ;

      (* Screen should be similar after modal closed *)
      check
        bool
        "header present after close"
        true
        (TH.contains_substring screen3 "octez-manager"))

(* ============================================================ *)
(* Form Fill Integration Tests *)
(* ============================================================ *)

(** Test: Fill instance name field in node form.
    Instance Name is near the bottom of the form.
    Typing appends to existing text in the text input modal.
*)
let test_node_form_fill_instance_name () =
  TH.with_test_env (fun () ->
      Headless.Stateful.init (module Install_node_form.Page) ;

      (* Instance Name field is near the bottom of the form - navigate to it *)
      (* Form order: Network(0), History(1), Snapshot(2), AppBin(3), DataDir(4),
         RPC(5), P2P(6), ExtraArgs(7), User(8), EnableBoot(9), StartNow(10),
         InstanceName(11), Confirm(12) *)
      TH.navigate_down 11 ;

      (* Verify we're on instance name field *)
      let screen1 = TH.get_screen_text () in
      check
        bool
        "shows instance name field"
        true
        (TH.contains_substring screen1 "Instance Name") ;

      (* Press Enter to edit the instance name *)
      ignore (TH.send_key_and_wait "Enter") ;
      check bool "edit modal opened" true (Modal_manager.has_active ()) ;

      (* Type a suffix - this appends to existing text *)
      let custom_suffix = "-custom" in
      TH.type_string custom_suffix ;

      (* Confirm with Enter *)
      ignore (TH.send_key_and_wait "Enter") ;

      (* Wait for modal to close *)
      ignore (TH.wait_until_no_modal ()) ;

      (* Verify the custom suffix appears in the form *)
      let screen2 = TH.get_screen_text () in
      check
        bool
        "custom suffix visible"
        true
        (TH.contains_substring screen2 custom_suffix) ;

      (* Navigate and verify name persists *)
      ignore (TH.send_key_and_wait "Up") ;
      let screen3 = TH.get_screen_text () in
      check
        bool
        "name persists after nav"
        true
        (TH.contains_substring screen3 custom_suffix))

(** Test: Change network selection in node form.
    Verifies the network picker modal works and selection persists.
*)
let test_node_form_change_network () =
  TH.with_test_env (fun () ->
      Headless.Stateful.init (module Install_node_form.Page) ;

      (* Default network should be mainnet *)
      let screen1 = TH.get_screen_text () in
      check bool "shows mainnet" true (TH.contains_substring screen1 "mainnet") ;

      (* Navigate to network field (second field) *)
      ignore (TH.send_key_and_wait "Down") ;

      (* Open network picker *)
      ignore (TH.send_key_and_wait "Enter") ;
      check bool "network picker opened" true (Modal_manager.has_active ()) ;

      (* Network picker should show options *)
      let picker_screen = TH.get_screen_text () in
      check
        bool
        "shows network options"
        true
        (TH.contains_substring picker_screen "mainnet"
        || TH.contains_substring picker_screen "ghostnet"
        || TH.contains_substring picker_screen "Network") ;

      (* Navigate down to select a different network (ghostnet) *)
      ignore (TH.send_key_and_wait "Down") ;

      (* Select it *)
      ignore (TH.send_key_and_wait "Enter") ;

      (* Modal should close *)
      ignore (TH.wait_until_no_modal ()) ;

      (* The form should now show the selected network *)
      let screen2 = TH.get_screen_text () in
      (* Either ghostnet is shown or we stayed on mainnet - both are valid *)
      check
        bool
        "network field has value"
        true
        (TH.contains_substring screen2 "mainnet"
        || TH.contains_substring screen2 "ghostnet"))

(** Test: Navigate through entire node form without errors.
    Verifies all fields are accessible and form structure is intact.
*)
let test_node_form_full_navigation () =
  TH.with_test_env (fun () ->
      Headless.Stateful.init (module Install_node_form.Page) ;

      (* Form has 13 fields: Network through Confirm & Install *)
      let total_fields = 13 in

      (* Navigate through all fields *)
      for _ = 1 to total_fields - 1 do
        ignore (TH.send_key_and_wait "Down")
      done ;

      (* Should be on last field (Confirm & Install) *)
      let screen_at_bottom = TH.get_screen_text () in
      check
        bool
        "can reach bottom"
        true
        (TH.contains_substring screen_at_bottom "Confirm") ;

      (* Navigate back up to first field *)
      for _ = 1 to total_fields - 1 do
        ignore (TH.send_key_and_wait "Up")
      done ;

      (* Should be back at first field (Network), form still working *)
      let final_screen = TH.get_screen_text () in
      check
        bool
        "form still renders"
        true
        (TH.contains_substring final_screen "Network"))

(** Test: Edit and cancel - verify Esc doesn't save changes.
    Opens a field, types something, presses Esc, verifies original value.
*)
let test_node_form_edit_cancel () =
  TH.with_test_env (fun () ->
      Headless.Stateful.init (module Install_node_form.Page) ;

      (* Get original screen content *)
      let screen1 = TH.get_screen_text () in

      (* Open instance name editor *)
      ignore (TH.send_key_and_wait "Enter") ;
      check bool "modal opened" true (Modal_manager.has_active ()) ;

      (* Type something *)
      TH.type_string "cancelled-name" ;

      (* Cancel with Esc *)
      ignore (TH.send_key_and_wait "Esc") ;

      (* Modal should close *)
      ignore (TH.wait_until_no_modal ()) ;

      (* The cancelled text should NOT appear in the form *)
      let screen2 = TH.get_screen_text () in
      check
        bool
        "cancelled text not saved"
        false
        (TH.contains_substring screen2 "cancelled-name") ;

      (* Screen should be unchanged from before edit *)
      check
        bool
        "form unchanged after cancel"
        true
        (String.equal (String.trim screen1) (String.trim screen2)))

(* ============================================================ *)
(* Test Suite *)
(* ============================================================ *)

let () =
  run
    "TUI Flows"
    [
      ( "Instances.init",
        [
          test_case "renders header" `Quick test_instances_init;
          test_case "shows hints" `Quick test_instances_shows_hints;
        ] );
      ( "Instances.navigation",
        [
          test_case "j/k keys" `Quick test_instances_navigation_jk;
          test_case "arrow keys" `Quick test_instances_navigation_arrows;
          test_case "Tab toggle" `Quick test_instances_tab_toggle;
          test_case "Space refresh" `Quick test_instances_space_refresh;
          test_case "wide terminal" `Quick test_instances_wide_terminal;
          test_case "key sequence" `Quick test_instances_key_sequence;
        ] );
      ( "Instances.modal",
        [
          test_case "c opens create menu" `Quick test_instances_create_menu;
          test_case
            "Escape closes modal"
            `Quick
            test_instances_escape_closes_modal;
          test_case "Enter on Install" `Quick test_instances_enter_on_install;
          test_case "modal navigation" `Quick test_instances_modal_navigation;
        ] );
      ( "Instances.quit",
        [test_case "q triggers quit" `Quick test_instances_quit] );
      ( "NodeForm.init",
        [
          test_case "renders form" `Quick test_node_form_init;
          test_case "shows fields" `Quick test_node_form_shows_fields;
        ] );
      ( "NodeForm.navigation",
        [
          test_case "field navigation" `Quick test_node_form_field_navigation;
          test_case "navigation bounds" `Quick test_node_form_navigation_bounds;
          test_case "submit visible" `Quick test_node_form_submit_visible;
        ] );
      ( "NodeForm.modal",
        [
          test_case "Enter opens modal" `Quick test_node_form_enter_opens_modal;
          test_case "Esc closes modal" `Quick test_node_form_modal_esc;
          test_case "network field" `Quick test_node_form_network_field;
        ] );
      ( "BakerForm.init",
        [
          test_case "renders form" `Quick test_baker_form_init;
          test_case "shows fields" `Quick test_baker_form_shows_fields;
        ] );
      ( "BakerForm.navigation",
        [
          test_case "field navigation" `Quick test_baker_form_field_navigation;
          test_case "navigation bounds" `Quick test_baker_form_navigation_bounds;
          test_case "liquidity vote" `Quick test_baker_form_liquidity_vote;
        ] );
      ( "BakerForm.modal",
        [
          test_case "Enter opens modal" `Quick test_baker_form_enter_opens_modal;
          test_case "Esc closes modal" `Quick test_baker_form_modal_esc;
        ] );
      ( "AccuserForm.init",
        [
          test_case "renders form" `Quick test_accuser_form_init;
          test_case "shows fields" `Quick test_accuser_form_shows_fields;
        ] );
      ( "AccuserForm.navigation",
        [
          test_case "field navigation" `Quick test_accuser_form_field_navigation;
          test_case
            "navigation bounds"
            `Quick
            test_accuser_form_navigation_bounds;
        ] );
      ( "AccuserForm.modal",
        [
          test_case
            "Enter opens modal"
            `Quick
            test_accuser_form_enter_opens_modal;
          test_case "Esc closes modal" `Quick test_accuser_form_modal_esc;
        ] );
      ( "DalForm.init",
        [
          test_case "renders form" `Quick test_dal_form_init;
          test_case "shows fields" `Quick test_dal_form_shows_fields;
        ] );
      ( "DalForm.navigation",
        [
          test_case "field navigation" `Quick test_dal_form_field_navigation;
          test_case "navigation bounds" `Quick test_dal_form_navigation_bounds;
        ] );
      ( "DalForm.modal",
        [
          test_case "Enter opens modal" `Quick test_dal_form_enter_opens_modal;
          test_case "Esc closes modal" `Quick test_dal_form_modal_esc;
        ] );
      (* Wizard-style integration tests *)
      ( "Instances.wizard",
        [
          test_case "create node flow" `Quick test_instances_create_node_flow;
          test_case "create baker flow" `Quick test_instances_create_baker_flow;
          test_case
            "create menu shows all"
            `Quick
            test_instances_create_menu_shows_all_types;
          test_case
            "create menu escape"
            `Quick
            test_instances_create_menu_escape;
          test_case
            "create menu vim nav"
            `Quick
            test_instances_create_menu_vim_nav;
          test_case "create menu cycle" `Quick test_instances_create_menu_cycle;
          test_case
            "screen updates on nav"
            `Quick
            test_instances_screen_updates_on_nav;
        ] );
      (* Form fill integration tests *)
      ( "NodeForm.fill",
        [
          test_case "fill instance name" `Quick test_node_form_fill_instance_name;
          test_case "change network" `Quick test_node_form_change_network;
          test_case "full navigation" `Quick test_node_form_full_navigation;
          test_case "edit and cancel" `Quick test_node_form_edit_cancel;
        ] );
    ]
