(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Tests for Directory_registry to ensure shared directory handling works correctly.

    These tests verify the critical bug fix where multiple instances sharing
    the same directory would lose track of each other in the registry.
*)

open Octez_manager_lib

let test_name = ref ""

let assert_equal expected actual msg =
  if expected <> actual then (
    Printf.eprintf "❌ Test '%s' failed: %s\n" !test_name msg;
    Printf.eprintf "   Expected: %s\n" (String.concat ", " expected);
    Printf.eprintf "   Got: %s\n" (String.concat ", " actual);
    exit 1)

let assert_true cond msg =
  if not cond then (
    Printf.eprintf "❌ Test '%s' failed: %s\n" !test_name msg;
    exit 1)

(** Test: Adding the same directory twice should merge linked_services *)
let test_shared_directory_merge () =
  test_name := "test_shared_directory_merge";

  (* Clean up from any previous test *)
  let _ = Directory_registry.remove "/tmp/test_shared_merge" in

  (* Add first instance *)
  let _ = Directory_registry.add
    ~path:"/tmp/test_shared_merge"
    ~dir_type:Directory_registry.Client_base_dir
    ~linked_services:["accuser1"]
  in

  (* Verify first instance *)
  let result = Directory_registry.find_by_path "/tmp/test_shared_merge" in
  assert_true (Result.is_ok result) "find_by_path should succeed";
  let entry = Option.get (Result.get_ok result) in
  assert_equal ["accuser1"] entry.linked_services
    "First instance should be registered";

  (* Add second instance with SAME path *)
  let _ = Directory_registry.add
    ~path:"/tmp/test_shared_merge"
    ~dir_type:Directory_registry.Client_base_dir
    ~linked_services:["accuser2"]
  in

  (* Verify BOTH instances are present *)
  let result = Directory_registry.find_by_path "/tmp/test_shared_merge" in
  let entry = Option.get (Result.get_ok result) in
  let services = List.sort String.compare entry.linked_services in
  assert_equal ["accuser1"; "accuser2"] services
    "Both instances should be in linked_services (merged)";

  (* Cleanup *)
  let _ = Directory_registry.cleanup_for_instance ~instance:"accuser1" in
  let _ = Directory_registry.cleanup_for_instance ~instance:"accuser2" in

  print_endline "✓ test_shared_directory_merge passed"

(** Test: Cleanup should only remove directory when no instances remain *)
let test_cleanup_shared_directory () =
  test_name := "test_cleanup_shared_directory";

  (* Setup: Two instances sharing a directory *)
  let _ = Directory_registry.add
    ~path:"/tmp/test_cleanup"
    ~dir_type:Directory_registry.Client_base_dir
    ~linked_services:["accuser1"; "accuser2"]
  in

  (* Remove accuser1 *)
  let _ = Directory_registry.cleanup_for_instance ~instance:"accuser1" in

  (* Directory should STILL exist with accuser2 *)
  let result = Directory_registry.find_by_path "/tmp/test_cleanup" in
  assert_true (Result.is_ok result) "Directory should still exist after removing one instance";
  let entry = Option.get (Result.get_ok result) in
  assert_equal ["accuser2"] entry.linked_services
    "Only accuser2 should remain";

  (* Remove accuser2 *)
  let _ = Directory_registry.cleanup_for_instance ~instance:"accuser2" in

  (* NOW directory should be removed *)
  let result = Directory_registry.find_by_path "/tmp/test_cleanup" in
  assert_true (Option.is_none (Result.get_ok result))
    "Directory should be removed when no instances remain";

  print_endline "✓ test_cleanup_shared_directory passed"

(** Test: Deduplicate same instance *)
let test_deduplicate_same_instance () =
  test_name := "test_deduplicate_same_instance";

  let _ = Directory_registry.add
    ~path:"/tmp/test_dedup"
    ~dir_type:Directory_registry.Client_base_dir
    ~linked_services:["accuser1"]
  in

  (* Add accuser1 AGAIN *)
  let _ = Directory_registry.add
    ~path:"/tmp/test_dedup"
    ~dir_type:Directory_registry.Client_base_dir
    ~linked_services:["accuser1"]
  in

  (* Should only have ONE entry for accuser1 *)
  let result = Directory_registry.find_by_path "/tmp/test_dedup" in
  let entry = Option.get (Result.get_ok result) in
  assert_equal ["accuser1"] entry.linked_services
    "Should deduplicate duplicate instances";
  assert_true (List.length entry.linked_services = 1)
    "Should have exactly one instance";

  (* Cleanup *)
  let _ = Directory_registry.cleanup_for_instance ~instance:"accuser1" in

  print_endline "✓ test_deduplicate_same_instance passed"

(** Test: Complex sharing scenario *)
let test_complex_sharing () =
  test_name := "test_complex_sharing";

  (* accuser1 and accuser2 share /tmp/base1 *)
  let _ = Directory_registry.add
    ~path:"/tmp/test_complex1"
    ~dir_type:Directory_registry.Client_base_dir
    ~linked_services:["accuser1"; "accuser2"]
  in

  (* accuser2 and accuser3 share /tmp/base2 *)
  let _ = Directory_registry.add
    ~path:"/tmp/test_complex2"
    ~dir_type:Directory_registry.Client_base_dir
    ~linked_services:["accuser2"; "accuser3"]
  in

  (* Remove accuser2 (shared by both directories) *)
  let _ = Directory_registry.cleanup_for_instance ~instance:"accuser2" in

  (* base1 should only have accuser1 *)
  let base1 = Option.get (Result.get_ok (Directory_registry.find_by_path "/tmp/test_complex1")) in
  assert_equal ["accuser1"] base1.linked_services
    "base1 should only have accuser1 after removing accuser2";

  (* base2 should only have accuser3 *)
  let base2 = Option.get (Result.get_ok (Directory_registry.find_by_path "/tmp/test_complex2")) in
  assert_equal ["accuser3"] base2.linked_services
    "base2 should only have accuser3 after removing accuser2";

  (* Cleanup *)
  let _ = Directory_registry.cleanup_for_instance ~instance:"accuser1" in
  let _ = Directory_registry.cleanup_for_instance ~instance:"accuser3" in

  print_endline "✓ test_complex_sharing passed"

(** Run all tests *)
let () =
  print_endline "\n=== Running Directory Registry Tests ===";
  print_endline "Testing critical bug fix: shared directory handling\n";

  try
    test_shared_directory_merge ();
    test_cleanup_shared_directory ();
    test_deduplicate_same_instance ();
    test_complex_sharing ();

    print_endline "\n✅ All 4 tests passed!";
    print_endline "   Shared directories are correctly merged and cleaned up.\n";
    exit 0
  with e ->
    Printf.eprintf "\n❌ Test suite failed with exception: %s\n" (Printexc.to_string e);
    Printexc.print_backtrace stderr;
    exit 1
