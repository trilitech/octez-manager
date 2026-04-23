(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Tests for display width utilities. *)

(* The module is part of octez-manager.ui library *)
module Display = Octez_manager_ui.Rewards_display_utils

let test_strip_ansi () =
  Alcotest.(check string)
    "strip simple ANSI"
    "hello"
    (Display.strip_ansi "\027[31mhello\027[0m") ;
  Alcotest.(check string)
    "strip multiple ANSI"
    "hello world"
    (Display.strip_ansi "\027[1m\027[31mhello\027[0m world") ;
  Alcotest.(check string)
    "no ANSI"
    "plain text"
    (Display.strip_ansi "plain text")

let test_display_width () =
  Alcotest.(check int) "ASCII" 5 (Display.display_width "hello") ;
  Alcotest.(check int) "tez symbol" 1 (Display.display_width "\xEA\x9C\xA9") ;
  Alcotest.(check int) "em dash" 1 (Display.display_width "\xE2\x80\x94") ;
  Alcotest.(check int) "indicator" 1 (Display.display_width "\xe2\x96\xb8") ;
  Alcotest.(check int) "mixed" 8 (Display.display_width "100.00 \xEA\x9C\xA9") ;
  (* "100.00 " = 7 chars + 1 for ꜩ = 8 *)
  Alcotest.(check int) "empty" 0 (Display.display_width "")

let test_pad_right () =
  Alcotest.(check string)
    "pad ASCII"
    "hello     "
    (Display.pad_right 10 "hello") ;
  Alcotest.(check string)
    "pad with tez"
    "100 \xEA\x9C\xA9     "
    (Display.pad_right 10 "100 \xEA\x9C\xA9") ;
  (* "100 ꜩ" = 5 display chars, pad to 10 = 5 spaces *)
  Alcotest.(check string)
    "pad with ANSI"
    "\027[32mpaid\027[0m      "
    (Display.pad_right 10 "\027[32mpaid\027[0m") ;
  (* "paid" = 4 display chars, pad to 10 = 6 spaces *)
  Alcotest.(check string)
    "no padding needed"
    "hello"
    (Display.pad_right 5 "hello") ;
  Alcotest.(check string)
    "already longer"
    "hello world"
    (Display.pad_right 5 "hello world")

let test_pad_left () =
  Alcotest.(check string) "pad ASCII" "     hello" (Display.pad_left 10 "hello") ;
  Alcotest.(check string)
    "pad with tez"
    "     100 \xEA\x9C\xA9"
    (Display.pad_left 10 "100 \xEA\x9C\xA9") ;
  Alcotest.(check string)
    "pad with ANSI"
    "      \027[32mpaid\027[0m"
    (Display.pad_left 10 "\027[32mpaid\027[0m") ;
  Alcotest.(check string)
    "no padding needed"
    "hello"
    (Display.pad_left 5 "hello") ;
  Alcotest.(check string)
    "already longer"
    "hello world"
    (Display.pad_left 5 "hello world")

let () =
  let open Alcotest in
  run
    "Display Utils"
    [
      ("strip_ansi", [test_case "strip ANSI codes" `Quick test_strip_ansi]);
      ( "display_width",
        [test_case "count display width" `Quick test_display_width] );
      ("pad_right", [test_case "pad right" `Quick test_pad_right]);
      ("pad_left", [test_case "pad left" `Quick test_pad_left]);
    ]
