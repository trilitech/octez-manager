(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
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
    ]
