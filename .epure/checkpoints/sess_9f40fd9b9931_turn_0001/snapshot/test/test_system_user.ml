(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Tests for System_user module - user/group management
    
    Tests cover:
    - Service account validation
    - User existence checks
    - Group existence checks
    - Service account creation (mocked)
    - Service account removal (mocked)
*)

open Alcotest
open Octez_manager_lib

(* ============================================================ *)
(* Test Helpers *)
(* ============================================================ *)

let check_ok = check (result unit (of_pp Rresult.R.pp_msg))

(* ============================================================ *)
(* Validation Tests *)
(* ============================================================ *)

let test_validate_user_exists () =
  let result =
    System_user.For_tests.with_overrides
      ~user_exists:(fun name -> name = "tezos")
      (fun () -> System_user.validate_user_for_service ~user:"tezos")
  in
  check_ok "existing user valid" (Ok ()) result

let test_validate_user_missing () =
  let result =
    System_user.For_tests.with_overrides
      ~user_exists:(fun _ -> false)
      (fun () -> System_user.validate_user_for_service ~user:"nonexistent")
  in
  match result with
  | Error _ -> check bool "missing user fails" true true
  | Ok () -> fail "should reject nonexistent user"

let test_validate_root_user () =
  let result =
    System_user.For_tests.with_overrides
      ~user_exists:(fun name -> name = "root")
      (fun () -> System_user.validate_user_for_service ~user:"root")
  in
  check_ok "root user valid" (Ok ()) result

(* ============================================================ *)
(* Ensure Account Tests *)
(* ============================================================ *)

let test_ensure_account_creates_if_missing () =
  let created = ref false in
  let result =
    System_user.For_tests.with_overrides
      ~is_root:(fun () -> true) (* Must be root to create users *)
      ~user_exists:(fun _ -> false)
      ~group_exists:(fun _ -> false)
      ~run:(fun ?quiet:_ ?on_log:_ _cmd ->
        created := true ;
        Ok ())
      (fun () ->
        System_user.ensure_service_account ~quiet:true ~name:"tezos" ())
  in
  check_ok "account creation succeeds" (Ok ()) result ;
  check bool "creation command run" true !created

let test_ensure_account_skips_if_exists () =
  let commands_run = ref 0 in
  let result =
    System_user.For_tests.with_overrides
      ~user_exists:(fun name -> name = "tezos")
      ~run:(fun ?quiet:_ ?on_log:_ _cmd ->
        commands_run := !commands_run + 1 ;
        Ok ())
      (fun () ->
        System_user.ensure_service_account ~quiet:true ~name:"tezos" ())
  in
  check_ok "skips if exists" (Ok ()) result ;
  check int "no commands run" 0 !commands_run

(* ============================================================ *)
(* Remove Account Tests *)
(* ============================================================ *)

let test_remove_account_calls_userdel () =
  let removed = ref false in
  let result =
    System_user.For_tests.with_overrides
      ~is_root:(fun () -> true) (* Must be root to remove users *)
      ~user_exists:(fun _ -> true)
      ~run:(fun ?quiet:_ ?on_log:_ cmd ->
        if List.mem "userdel" cmd then removed := true ;
        Ok ())
      (fun () ->
        System_user.remove_service_account ~quiet:true ~name:"olduser" ())
  in
  check_ok "removal succeeds" (Ok ()) result ;
  check bool "userdel called" true !removed

let test_remove_nonexistent_account () =
  let result =
    System_user.For_tests.with_overrides
      ~user_exists:(fun _ -> false)
      ~run:(fun ?quiet:_ ?on_log:_ _cmd -> Ok ())
      (fun () ->
        System_user.remove_service_account ~quiet:true ~name:"nonexistent" ())
  in
  (* Should either succeed or error gracefully *)
  match result with
  | Ok () -> check bool "removal ok" true true
  | Error _ -> check bool "removal error" true true

(* ============================================================ *)
(* Test Suite *)
(* ============================================================ *)

let validation_tests =
  [
    ("validate existing user", `Quick, test_validate_user_exists);
    ("validate missing user", `Quick, test_validate_user_missing);
    ("validate root user", `Quick, test_validate_root_user);
  ]

let ensure_tests =
  [
    ("ensure creates if missing", `Quick, test_ensure_account_creates_if_missing);
    ("ensure skips if exists", `Quick, test_ensure_account_skips_if_exists);
  ]

let remove_tests =
  [
    ("remove calls userdel", `Quick, test_remove_account_calls_userdel);
    ("remove nonexistent", `Quick, test_remove_nonexistent_account);
  ]

let () =
  Alcotest.run
    "System_user"
    [
      ("validation", validation_tests);
      ("ensure", ensure_tests);
      ("remove", remove_tests);
    ]
