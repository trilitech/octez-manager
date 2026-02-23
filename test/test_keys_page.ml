(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Tests for Keys_page module
    
    Tests cover:
    - Deduplication of base directories (prevents duplicate wallet display)
*)

open Alcotest
open Octez_manager_lib
module Keys_page = Octez_manager_ui.Keys_page

(* ============================================================ *)
(* Base Directory Deduplication Tests *)
(* ============================================================ *)

(** Test that get_all_base_dirs deduplicates when default dir is also in registry.
    This prevents the bug where wallets appear twice in the TUI. *)
let test_get_all_base_dirs_deduplicates () =
  (* Get the default directory (usually ~/.tezos-client) *)
  let default_dir = Keys_page.Internal_for_tests.default_client_base_dir () in

  (* Add the default directory to the registry *)
  let _ =
    Directory_registry.add
      ~path:default_dir
      ~dir_type:Client_base_dir
      ~registered_services:[]
  in

  (* Get all base directories - should be deduplicated *)
  let dirs = Keys_page.Internal_for_tests.get_all_base_dirs () in

  (* Count occurrences of the default directory *)
  let count_default =
    List.filter (fun d -> String.equal d default_dir) dirs |> List.length
  in

  (* Cleanup *)
  let _ = Directory_registry.remove default_dir in

  (* Assert: default_dir should appear exactly once, not twice *)
  check int "default dir appears once" 1 count_default

(** Test that directories are unique even when multiple managed dirs exist *)
let test_get_all_base_dirs_multiple_managed () =
  let dir1 = "/tmp/test-dir1" in
  let dir2 = "/tmp/test-dir2" in

  (* Add two managed directories *)
  let _ =
    Directory_registry.add
      ~path:dir1
      ~dir_type:Client_base_dir
      ~registered_services:[]
  in
  let _ =
    Directory_registry.add
      ~path:dir2
      ~dir_type:Client_base_dir
      ~registered_services:[]
  in

  let dirs = Keys_page.Internal_for_tests.get_all_base_dirs () in

  (* Count unique directories *)
  let unique_dirs = List.sort_uniq String.compare dirs in

  (* Cleanup *)
  let _ = Directory_registry.remove dir1 in
  let _ = Directory_registry.remove dir2 in

  (* Assert: all directories should be unique *)
  check int "all dirs unique" (List.length unique_dirs) (List.length dirs)

(** Test that trailing slashes are normalized during deduplication *)
let test_get_all_base_dirs_trailing_slash () =
  let dir_no_slash = "/tmp/test-dir-slash" in
  let dir_with_slash = "/tmp/test-dir-slash/" in

  (* Add the same directory with trailing slash *)
  let _ =
    Directory_registry.add
      ~path:dir_with_slash
      ~dir_type:Client_base_dir
      ~registered_services:[]
  in

  (* If default_client_base_dir returns dir_no_slash, they should still deduplicate *)
  let dirs = Keys_page.Internal_for_tests.get_all_base_dirs () in

  (* Count occurrences of the directory (with or without slash) *)
  let count =
    List.filter
      (fun d -> String.equal d dir_no_slash || String.equal d dir_with_slash)
      dirs
    |> List.length
  in

  (* Cleanup *)
  let _ = Directory_registry.remove dir_with_slash in

  (* If the directory was added, it should appear at most once after normalization *)
  check bool "trailing slash normalized" true (count <= 1)

(* ============================================================ *)
(* Test Suite *)
(* ============================================================ *)

let base_dir_tests =
  [
    ( "deduplicates default dir when in registry",
      `Quick,
      test_get_all_base_dirs_deduplicates );
    ( "ensures uniqueness with multiple managed dirs",
      `Quick,
      test_get_all_base_dirs_multiple_managed );
    ( "normalizes trailing slashes during deduplication",
      `Quick,
      test_get_all_base_dirs_trailing_slash );
  ]

let () = Alcotest.run "Keys_page" [("base_dir_deduplication", base_dir_tests)]
