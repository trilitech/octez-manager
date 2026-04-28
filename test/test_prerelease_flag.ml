(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Tests for the [--unreleased-binaries] flag plumbing.

    Covers:
    - {!Prerelease_flag} set/get round-trip
    - {!Cli_helpers.apply_unreleased_binaries_flag} flips the global flag
    - {!Octez_index_downloader.For_tests.filter_versions} respects the
      [include_prerelease] argument *)

open Alcotest
open Octez_manager_lib

(* ============================================================ *)
(* Test isolation helpers *)
(* ============================================================ *)

(** Save/restore the flag around each test so ordering doesn't leak state. *)
let with_flag_reset f =
  let saved = Prerelease_flag.get () in
  Fun.protect ~finally:(fun () -> Prerelease_flag.set saved) f

(* ============================================================ *)
(* Prerelease_flag round-trip *)
(* ============================================================ *)

let test_default_off () =
  with_flag_reset @@ fun () ->
  Prerelease_flag.set false ;
  check bool "starts off" false (Prerelease_flag.get ())

let test_set_get_true () =
  with_flag_reset @@ fun () ->
  Prerelease_flag.set true ;
  check bool "flipped on" true (Prerelease_flag.get ())

let test_set_get_false () =
  with_flag_reset @@ fun () ->
  Prerelease_flag.set true ;
  Prerelease_flag.set false ;
  check bool "flipped back off" false (Prerelease_flag.get ())

(* ============================================================ *)
(* CLI wiring *)
(* ============================================================ *)

let test_apply_flag_true () =
  with_flag_reset @@ fun () ->
  Prerelease_flag.set false ;
  Cli_helpers.apply_unreleased_binaries_flag true ;
  check bool "apply true flips on" true (Prerelease_flag.get ())

let test_apply_flag_false () =
  with_flag_reset @@ fun () ->
  Prerelease_flag.set true ;
  Cli_helpers.apply_unreleased_binaries_flag false ;
  check bool "apply false flips off" false (Prerelease_flag.get ())

(* ============================================================ *)
(* Octez_index_downloader filter *)
(* ============================================================ *)

let mixed_index_versions : Octez_index_downloader.version_info list =
  [
    {version = "1.1.0"; release_date = None; is_prerelease = false};
    {version = "1.1.0-rc1"; release_date = None; is_prerelease = true};
    {version = "1.0.0"; release_date = None; is_prerelease = false};
    {version = "0.9.0-beta"; release_date = None; is_prerelease = true};
  ]

let test_filter_index_excludes_prerelease () =
  let filtered =
    Octez_index_downloader.For_tests.filter_versions
      ~include_prerelease:false
      mixed_index_versions
  in
  check int "stable count" 2 (List.length filtered) ;
  check
    bool
    "no prerelease kept"
    true
    (List.for_all
       (fun v -> not v.Octez_index_downloader.is_prerelease)
       filtered)

let test_filter_index_keeps_prerelease () =
  let filtered =
    Octez_index_downloader.For_tests.filter_versions
      ~include_prerelease:true
      mixed_index_versions
  in
  check int "all kept" 4 (List.length filtered) ;
  check
    bool
    "at least one prerelease"
    true
    (List.exists (fun v -> v.Octez_index_downloader.is_prerelease) filtered)

(* ============================================================ *)
(* Test Suite *)
(* ============================================================ *)

let flag_tests =
  [
    ("default state", `Quick, test_default_off);
    ("set true / get true", `Quick, test_set_get_true);
    ("set true then false", `Quick, test_set_get_false);
  ]

let cli_wiring_tests =
  [
    ("apply true flips flag on", `Quick, test_apply_flag_true);
    ("apply false flips flag off", `Quick, test_apply_flag_false);
  ]

let index_filter_tests =
  [
    ( "filter excludes prereleases by default",
      `Quick,
      test_filter_index_excludes_prerelease );
    ( "filter keeps prereleases when include_prerelease",
      `Quick,
      test_filter_index_keeps_prerelease );
  ]

let () =
  Alcotest.run
    "Prerelease_flag"
    [
      ("flag_round_trip", flag_tests);
      ("cli_wiring", cli_wiring_tests);
      ("octez_index_filter", index_filter_tests);
    ]
