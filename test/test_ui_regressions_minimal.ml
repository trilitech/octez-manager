(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

(** Minimal UI Regression Test
    
    This test verifies the regression testing framework is working correctly
    before we add comprehensive UI tests.
*)

module URF = Ui_regression_framework_lib.Ui_regression_framework
module DM = Ui_regression_framework_lib.Deterministic_mocks
module HD = Lib_miaou_internal.Headless_driver

(* ============================================================ *)
(* Basic Framework Tests *)
(* ============================================================ *)

(** Test screen capture with actual page rendering *)
let test_page_capture () =
  DM.setup_deterministic_env () ;

  (* For now, just verify the framework can capture screen content *)
  let _content = HD.get_screen_content () in

  (* This will create a baseline on first run *)
  URF.assert_ui_regression "minimal_screen"

(** Test that environment is set up *)
let test_env_setup () =
  DM.setup_deterministic_env () ;

  let home = Unix.getenv "HOME" in
  Alcotest.(check bool)
    "HOME is in test root"
    true
    (String.starts_with ~prefix:DM.test_root home)

(** Test that time is deterministic *)
let test_fixed_time () =
  DM.setup_deterministic_env () ;

  let t1 = DM.get_time () in
  let t2 = DM.get_time () in

  Alcotest.(check (float 0.0)) "time is fixed" DM.fixed_timestamp t1 ;
  Alcotest.(check (float 0.0)) "time doesn't change" t1 t2

(** Test that random is deterministic *)
let test_deterministic_random () =
  DM.setup_deterministic_env () ;
  let r1 = Random.int 1000 in

  DM.setup_deterministic_env () ;
  let r2 = Random.int 1000 in

  Alcotest.(check int) "random is seeded identically" r1 r2

(* ============================================================ *)
(* Test Suite *)
(* ============================================================ *)

let framework_tests =
  [
    ("page_capture", `Quick, test_page_capture);
    ("env_setup", `Quick, test_env_setup);
    ("fixed_time", `Quick, test_fixed_time);
    ("deterministic_random", `Quick, test_deterministic_random);
  ]

let () = Alcotest.run "UI_Regression_Framework" [("framework", framework_tests)]
