(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Debug test to see actual help modal content *)

open Alcotest
module HD = Lib_miaou_internal.Headless_driver
module TH = Tui_test_helpers_lib.Tui_test_helpers
module Instances = Octez_manager_ui.Instances
module Diagnostics_page = Octez_manager_ui.Diagnostics_page
module Main_shell = Octez_manager_ui.Main_shell

let test_debug_instances_help () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Instances.Page) ;
      ignore (HD.Stateful.idle_wait ~iterations:2 ~sleep:0.001 ()) ;
      ignore (HD.Stateful.send_key "?") ;
      ignore (HD.Stateful.idle_wait ~iterations:5 ~sleep:0.001 ()) ;

      let screen = TH.get_screen_text () in
      Printf.printf
        "\n=== INSTANCES HELP (initial) ===\n%s\n=== END ===\n%!"
        screen ;

      (* Scroll down to see more content *)
      ignore (HD.Stateful.send_key "Down") ;
      ignore (HD.Stateful.send_key "Down") ;
      ignore (HD.Stateful.send_key "Down") ;
      ignore (HD.Stateful.send_key "Down") ;
      ignore (HD.Stateful.send_key "Down") ;
      ignore (HD.Stateful.idle_wait ~iterations:3 ~sleep:0.001 ()) ;

      let screen2 = TH.get_screen_text () in
      Printf.printf
        "\n=== INSTANCES HELP (scrolled) ===\n%s\n=== END ===\n%!"
        screen2 ;

      check bool "debug test" true true)

let test_debug_diagnostics_help () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Diagnostics_page.Page) ;
      ignore (HD.Stateful.idle_wait ~iterations:2 ~sleep:0.001 ()) ;
      ignore (HD.Stateful.send_key "?") ;
      ignore (HD.Stateful.idle_wait ~iterations:5 ~sleep:0.001 ()) ;

      let screen = TH.get_screen_text () in
      Printf.printf
        "\n=== DIAGNOSTICS HELP (initial) ===\n%s\n=== END ===\n%!"
        screen ;

      check bool "debug test" true true)

let test_debug_main_shell_help () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Main_shell.Page) ;
      ignore (HD.Stateful.idle_wait ~iterations:2 ~sleep:0.001 ()) ;
      ignore (HD.Stateful.send_key "?") ;
      ignore (HD.Stateful.idle_wait ~iterations:5 ~sleep:0.001 ()) ;

      let screen = TH.get_screen_text () in
      Printf.printf
        "\n=== MAIN_SHELL HELP (initial) ===\n%s\n=== END ===\n%!"
        screen ;

      check bool "debug test" true true)

let () =
  Alcotest.run
    "Debug Help Modal"
    [
      ( "debug",
        [
          ("instances help", `Quick, test_debug_instances_help);
          ("diagnostics help", `Quick, test_debug_diagnostics_help);
          ("main_shell help", `Quick, test_debug_main_shell_help);
        ] );
    ]
