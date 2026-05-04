(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Octez_manager_ui
module Lib_snapshots = Octez_manager_lib.Snapshots
module Navigation = Miaou.Core.Navigation

(* ============================================================ *)
(* Helper: create minimal entries and state                      *)
(* ============================================================ *)

let make_entry ?(label = "test") ?download_url () : Lib_snapshots.entry =
  {
    network = "mainnet";
    slug = label;
    label;
    download_url;
    history_mode = None;
    metadata = [];
  }

let make_state ?(network = "mainnet") ?(entries = []) ?(selected = 0) ?error ()
    : Snapshots.state =
  {network; entries; selected; error}

let wrap_state = Navigation.make

(* ============================================================ *)
(* move_selection Tests                                          *)
(* ============================================================ *)

let test_move_down () =
  let entries = [make_entry ~label:"a" (); make_entry ~label:"b" ()] in
  let ps = wrap_state (make_state ~entries ~selected:0 ()) in
  let ps' = Snapshots.move_selection ps 1 in
  Alcotest.(check int) "moved to 1" 1 ps'.Navigation.s.selected

let test_move_up () =
  let entries = [make_entry ~label:"a" (); make_entry ~label:"b" ()] in
  let ps = wrap_state (make_state ~entries ~selected:1 ()) in
  let ps' = Snapshots.move_selection ps (-1) in
  Alcotest.(check int) "moved to 0" 0 ps'.Navigation.s.selected

let test_move_clamp_bottom () =
  let entries = [make_entry ~label:"a" (); make_entry ~label:"b" ()] in
  let ps = wrap_state (make_state ~entries ~selected:1 ()) in
  let ps' = Snapshots.move_selection ps 1 in
  Alcotest.(check int) "clamped at 1" 1 ps'.Navigation.s.selected

let test_move_clamp_top () =
  let entries = [make_entry ~label:"a" (); make_entry ~label:"b" ()] in
  let ps = wrap_state (make_state ~entries ~selected:0 ()) in
  let ps' = Snapshots.move_selection ps (-1) in
  Alcotest.(check int) "clamped at 0" 0 ps'.Navigation.s.selected

let test_move_empty () =
  let ps = wrap_state (make_state ~entries:[] ~selected:0 ()) in
  let ps' = Snapshots.move_selection ps 1 in
  Alcotest.(check int) "stays at 0" 0 ps'.Navigation.s.selected

let test_move_single () =
  let entries = [make_entry ~label:"only" ()] in
  let ps = wrap_state (make_state ~entries ~selected:0 ()) in
  let ps' = Snapshots.move_selection ps 1 in
  Alcotest.(check int) "stays at 0" 0 ps'.Navigation.s.selected

let test_move_large_delta () =
  let entries =
    [
      make_entry ~label:"a" ();
      make_entry ~label:"b" ();
      make_entry ~label:"c" ();
    ]
  in
  let ps = wrap_state (make_state ~entries ~selected:0 ()) in
  let ps' = Snapshots.move_selection ps 100 in
  Alcotest.(check int) "clamped at max" 2 ps'.Navigation.s.selected

(* ============================================================ *)
(* header Tests                                                  *)
(* ============================================================ *)

let contains_substring = Test_string_helpers.contains_substring

let make_view_state ?(network = "mainnet") ?(entries = []) ?(selected = 0)
    ?error () : Snapshots_view.state =
  {Snapshots_view.network; entries; selected; error}

let test_header_contains_network () =
  let s = make_view_state ~network:"mainnet" () in
  let lines = Snapshots_view.header s in
  let joined = String.concat " " lines in
  Alcotest.(check bool)
    "contains network"
    true
    (contains_substring joined "mainnet")

let test_header_different_network () =
  let s = make_view_state ~network:"weeklynet" () in
  let lines = Snapshots_view.header s in
  let joined = String.concat " " lines in
  Alcotest.(check bool)
    "contains weeklynet"
    true
    (contains_substring joined "weeklynet")

let test_header_length () =
  let s = make_view_state () in
  let lines = Snapshots_view.header s in
  Alcotest.(check bool) "has lines" true (List.length lines >= 1)

(* ============================================================ *)
(* handled_keys Tests                                            *)
(* ============================================================ *)

let test_handled_keys_has_escape () =
  let keys = Snapshots.handled_keys () in
  Alcotest.(check bool) "has Escape" true (List.mem Miaou.Core.Keys.Escape keys)

(* ============================================================ *)
(* keymap Tests                                                  *)
(* ============================================================ *)

let test_keymap_not_empty () =
  let s = make_state () in
  let km = Snapshots.keymap s in
  Alcotest.(check bool) "not empty" true (List.length km >= 1)

let test_keymap_has_esc () =
  let s = make_state () in
  let km = Snapshots.keymap s in
  let keys = List.map (fun kb -> kb.Miaou.Core.Tui_page.key) km in
  Alcotest.(check bool) "has Esc" true (List.mem "Esc" keys)

(* ============================================================ *)
(* Test Runner                                                   *)
(* ============================================================ *)

let () =
  Alcotest.run
    "Snapshots page (pure)"
    [
      ( "move_selection",
        [
          Alcotest.test_case "down" `Quick test_move_down;
          Alcotest.test_case "up" `Quick test_move_up;
          Alcotest.test_case "clamp bottom" `Quick test_move_clamp_bottom;
          Alcotest.test_case "clamp top" `Quick test_move_clamp_top;
          Alcotest.test_case "empty" `Quick test_move_empty;
          Alcotest.test_case "single" `Quick test_move_single;
          Alcotest.test_case "large delta" `Quick test_move_large_delta;
        ] );
      ( "header",
        [
          Alcotest.test_case
            "contains network"
            `Quick
            test_header_contains_network;
          Alcotest.test_case
            "different network"
            `Quick
            test_header_different_network;
          Alcotest.test_case "has lines" `Quick test_header_length;
        ] );
      ( "handled_keys",
        [Alcotest.test_case "has Escape" `Quick test_handled_keys_has_escape] );
      ( "keymap",
        [
          Alcotest.test_case "not empty" `Quick test_keymap_not_empty;
          Alcotest.test_case "has Esc" `Quick test_keymap_has_esc;
        ] );
    ]
