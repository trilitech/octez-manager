(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Octez_manager_lib
open Octez_manager_ui
module FT = Log_viewer_page.For_tests

(* ============================================================ *)
(* source_label Tests                                            *)
(* ============================================================ *)

let test_journald_label () =
  Alcotest.(check string)
    "journald"
    "journald"
    (FT.source_label Log_viewer.Journald)

let test_daily_logs_label () =
  Alcotest.(check string)
    "daily logs"
    "daily logs"
    (FT.source_label Log_viewer.DailyLogs)

(* ============================================================ *)
(* name Tests                                                    *)
(* ============================================================ *)

let test_name () =
  Alcotest.(check string) "name" "log_viewer" Log_viewer_page.name

(* ============================================================ *)
(* Test Runner                                                   *)
(* ============================================================ *)

let () =
  Alcotest.run
    "Log_viewer_page (pure)"
    [
      ( "source_label",
        [
          Alcotest.test_case "journald" `Quick test_journald_label;
          Alcotest.test_case "daily logs" `Quick test_daily_logs_label;
        ] );
      ("name", [Alcotest.test_case "module name" `Quick test_name]);
    ]
