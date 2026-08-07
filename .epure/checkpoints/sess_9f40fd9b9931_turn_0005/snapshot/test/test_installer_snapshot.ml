(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Unit tests for installer/snapshot.ml - snapshot download logic.
    
    Tests snapshot resolution, history mode matching, and error handling
    without requiring actual network calls or file downloads. *)

open Alcotest
open Octez_manager_lib
module Snapshot = Snapshot

(******************************************************************************)
(*                    HISTORY MODE MATCHING TESTS                            *)
(******************************************************************************)

(** Test: Exact match for rolling mode *)
let test_history_mode_rolling_exact () =
  let matches =
    Snapshot.history_mode_matches
      ~requested:History_mode.Rolling
      ~snapshot_mode:"rolling"
  in
  check bool "rolling matches rolling" true matches

(** Test: Exact match for full mode *)
let test_history_mode_full_exact () =
  let matches =
    Snapshot.history_mode_matches
      ~requested:History_mode.Full
      ~snapshot_mode:"full"
  in
  check bool "full matches full" true matches

(** Test: Case insensitive matching *)
let test_history_mode_case_insensitive () =
  let matches_upper =
    Snapshot.history_mode_matches
      ~requested:History_mode.Rolling
      ~snapshot_mode:"ROLLING"
  in
  check bool "rolling matches ROLLING" true matches_upper ;

  let matches_mixed =
    Snapshot.history_mode_matches
      ~requested:History_mode.Full
      ~snapshot_mode:"FuLl"
  in
  check bool "full matches FuLl" true matches_mixed

(** Test: Whitespace trimming *)
let test_history_mode_whitespace () =
  let matches_leading =
    Snapshot.history_mode_matches
      ~requested:History_mode.Rolling
      ~snapshot_mode:"  rolling"
  in
  check bool "rolling matches '  rolling'" true matches_leading ;

  let matches_trailing =
    Snapshot.history_mode_matches
      ~requested:History_mode.Full
      ~snapshot_mode:"full  "
  in
  check bool "full matches 'full  '" true matches_trailing ;

  let matches_both =
    Snapshot.history_mode_matches
      ~requested:History_mode.Rolling
      ~snapshot_mode:"  rolling  "
  in
  check bool "rolling matches '  rolling  '" true matches_both

(** Test: Mismatched modes return false *)
let test_history_mode_mismatch () =
  let matches =
    Snapshot.history_mode_matches
      ~requested:History_mode.Rolling
      ~snapshot_mode:"full"
  in
  check bool "rolling does not match full" false matches ;

  let matches2 =
    Snapshot.history_mode_matches
      ~requested:History_mode.Full
      ~snapshot_mode:"rolling"
  in
  check bool "full does not match rolling" false matches2

(** Test: Archive mode *)
let test_history_mode_archive () =
  let matches =
    Snapshot.history_mode_matches
      ~requested:History_mode.Archive
      ~snapshot_mode:"archive"
  in
  check bool "archive matches archive" true matches

(** Test: Invalid/unknown modes *)
let test_history_mode_invalid () =
  let matches =
    Snapshot.history_mode_matches
      ~requested:History_mode.Rolling
      ~snapshot_mode:"invalid"
  in
  check bool "rolling does not match invalid" false matches ;

  let matches2 =
    Snapshot.history_mode_matches ~requested:History_mode.Full ~snapshot_mode:""
  in
  check bool "full does not match empty string" false matches2

(******************************************************************************)
(*                              TEST SUITE                                   *)
(******************************************************************************)

let () =
  run
    "Installer Snapshot"
    [
      ( "History Mode Matching",
        [
          test_case "rolling exact match" `Quick test_history_mode_rolling_exact;
          test_case "full exact match" `Quick test_history_mode_full_exact;
          test_case "case insensitive" `Quick test_history_mode_case_insensitive;
          test_case "whitespace trimming" `Quick test_history_mode_whitespace;
          test_case "mode mismatch" `Quick test_history_mode_mismatch;
          test_case "archive mode" `Quick test_history_mode_archive;
          test_case "invalid modes" `Quick test_history_mode_invalid;
        ] );
    ]
