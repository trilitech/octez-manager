(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Tests for Dal_health module.

    Covers status_of_string, status_to_string, and cache operations. *)

open Alcotest
module DH = Octez_manager_ui.Dal_health

(* ── status_of_string ────────────────────────────────────────── *)

let test_status_of_string_up () =
  check string "up" "up" (DH.status_to_string (DH.status_of_string "up"))

let test_status_of_string_ok () =
  check string "ok -> up" "up" (DH.status_to_string (DH.status_of_string "ok"))

let test_status_of_string_down () =
  check string "down" "down" (DH.status_to_string (DH.status_of_string "down"))

let test_status_of_string_ko () =
  check
    string
    "ko -> down"
    "down"
    (DH.status_to_string (DH.status_of_string "ko"))

let test_status_of_string_no () =
  check
    string
    "no -> down"
    "down"
    (DH.status_to_string (DH.status_of_string "no"))

let test_status_of_string_degraded () =
  check
    string
    "degraded"
    "degraded"
    (DH.status_to_string (DH.status_of_string "degraded"))

let test_status_of_string_unknown () =
  check
    string
    "unknown"
    "?"
    (DH.status_to_string (DH.status_of_string "something"))

(* ── status_to_string ────────────────────────────────────────── *)

let test_status_to_string_up () =
  check string "Up" "up" (DH.status_to_string DH.Up)

let test_status_to_string_down () =
  check string "Down" "down" (DH.status_to_string DH.Down)

let test_status_to_string_degraded () =
  check string "Degraded" "degraded" (DH.status_to_string DH.Degraded)

let test_status_to_string_unknown () =
  check string "Unknown" "?" (DH.status_to_string DH.Unknown)

(* ── Cache operations ────────────────────────────────────────── *)

let test_cache_empty () =
  DH.clear () ;
  check bool "empty" true (Option.is_none (DH.get ~instance:"test-dal"))

let test_cache_set_get () =
  DH.clear () ;
  let data : DH.t =
    {
      status = Up;
      checks = [{name = "gossipsub"; status = Up}];
      last_fetch = 1000.0;
    }
  in
  DH.set ~instance:"my-dal" data ;
  match DH.get ~instance:"my-dal" with
  | None -> fail "should find dal health"
  | Some found ->
      check string "status" "up" (DH.status_to_string found.status) ;
      check int "checks" 1 (List.length found.checks)

let test_cache_clear_instance () =
  DH.clear () ;
  let data : DH.t = {status = Up; checks = []; last_fetch = 0.0} in
  DH.set ~instance:"a" data ;
  DH.set ~instance:"b" data ;
  DH.clear_instance ~instance:"a" ;
  check bool "a removed" true (Option.is_none (DH.get ~instance:"a")) ;
  check bool "b present" true (Option.is_some (DH.get ~instance:"b"))

let test_cache_clear_all () =
  DH.clear () ;
  let data : DH.t = {status = Down; checks = []; last_fetch = 0.0} in
  DH.set ~instance:"x" data ;
  DH.set ~instance:"y" data ;
  DH.clear () ;
  check bool "x gone" true (Option.is_none (DH.get ~instance:"x")) ;
  check bool "y gone" true (Option.is_none (DH.get ~instance:"y"))

let test_cache_overwrite () =
  DH.clear () ;
  DH.set ~instance:"z" ({status = Up; checks = []; last_fetch = 1.0} : DH.t) ;
  DH.set ~instance:"z" ({status = Down; checks = []; last_fetch = 2.0} : DH.t) ;
  match DH.get ~instance:"z" with
  | None -> fail "should find"
  | Some r -> check string "overwritten" "down" (DH.status_to_string r.status)

(* ── Test suite ──────────────────────────────────────────────── *)

let () =
  Alcotest.run
    "Dal_health"
    [
      ( "status_of_string",
        [
          test_case "up" `Quick test_status_of_string_up;
          test_case "ok" `Quick test_status_of_string_ok;
          test_case "down" `Quick test_status_of_string_down;
          test_case "ko" `Quick test_status_of_string_ko;
          test_case "no" `Quick test_status_of_string_no;
          test_case "degraded" `Quick test_status_of_string_degraded;
          test_case "unknown" `Quick test_status_of_string_unknown;
        ] );
      ( "status_to_string",
        [
          test_case "Up" `Quick test_status_to_string_up;
          test_case "Down" `Quick test_status_to_string_down;
          test_case "Degraded" `Quick test_status_to_string_degraded;
          test_case "Unknown" `Quick test_status_to_string_unknown;
        ] );
      ( "cache",
        [
          test_case "empty" `Quick test_cache_empty;
          test_case "set and get" `Quick test_cache_set_get;
          test_case "clear instance" `Quick test_cache_clear_instance;
          test_case "clear all" `Quick test_cache_clear_all;
          test_case "overwrite" `Quick test_cache_overwrite;
        ] );
    ]
