(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Tests for Directory_registry module - JSON serialization *)

open Alcotest
open Octez_manager_lib
module DR = Directory_registry.For_test

(** {2 dir_type JSON serialization tests} *)

let test_dir_type_to_yojson_node () =
  let json = DR.dir_type_to_yojson Directory_registry.Node_data_dir in
  check
    (testable Yojson.Safe.pp Yojson.Safe.equal)
    "node_data_dir"
    (`String "node_data_dir")
    json

let test_dir_type_to_yojson_client () =
  let json = DR.dir_type_to_yojson Directory_registry.Client_base_dir in
  check
    (testable Yojson.Safe.pp Yojson.Safe.equal)
    "client_base_dir"
    (`String "client_base_dir")
    json

let test_dir_type_to_yojson_app_bin () =
  let json = DR.dir_type_to_yojson Directory_registry.App_bin_dir in
  check
    (testable Yojson.Safe.pp Yojson.Safe.equal)
    "app_bin_dir"
    (`String "app_bin_dir")
    json

let test_dir_type_of_yojson_node () =
  let result = DR.dir_type_of_yojson (`String "node_data_dir") in
  match result with
  | Ok Directory_registry.Node_data_dir -> ()
  | Ok _ -> Alcotest.fail "Expected Node_data_dir"
  | Error err -> Alcotest.fail (Printf.sprintf "Parse failed: %s" err)

let test_dir_type_of_yojson_client () =
  let result = DR.dir_type_of_yojson (`String "client_base_dir") in
  match result with
  | Ok Directory_registry.Client_base_dir -> ()
  | Ok _ -> Alcotest.fail "Expected Client_base_dir"
  | Error err -> Alcotest.fail (Printf.sprintf "Parse failed: %s" err)

let test_dir_type_of_yojson_app_bin () =
  let result = DR.dir_type_of_yojson (`String "app_bin_dir") in
  match result with
  | Ok Directory_registry.App_bin_dir -> ()
  | Ok _ -> Alcotest.fail "Expected App_bin_dir"
  | Error err -> Alcotest.fail (Printf.sprintf "Parse failed: %s" err)

let test_dir_type_of_yojson_invalid () =
  let result = DR.dir_type_of_yojson (`String "unknown_type") in
  match result with Ok _ -> Alcotest.fail "Expected Error" | Error _ -> ()

(** {2 directory_entry JSON serialization tests} *)

let test_directory_entry_to_yojson () =
  let entry : Directory_registry.directory_entry =
    {
      path = "/var/lib/tezos/node";
      dir_type = Directory_registry.Node_data_dir;
      created_at = "2026-01-01 12:00:00";
      last_used_at = "2026-01-25 10:30:00";
      linked_services = ["node-mainnet"; "node-testnet"];
    }
  in
  let json = DR.directory_entry_to_yojson entry in
  match json with
  | `Assoc fields ->
      let path = List.assoc "path" fields in
      check
        (testable Yojson.Safe.pp Yojson.Safe.equal)
        "path field"
        (`String "/var/lib/tezos/node")
        path
  | _ -> Alcotest.fail "Expected Assoc"

let test_directory_entry_of_yojson () =
  let json =
    `Assoc
      [
        ("path", `String "/var/lib/tezos/node");
        ("dir_type", `String "node_data_dir");
        ("created_at", `String "2026-01-01 12:00:00");
        ("last_used_at", `String "2026-01-25 10:30:00");
        ( "linked_services",
          `List [`String "node-mainnet"; `String "node-testnet"] );
      ]
  in
  let result = DR.directory_entry_of_yojson json in
  match result with
  | Ok entry ->
      check string "path" "/var/lib/tezos/node" entry.Directory_registry.path ;
      check
        int
        "linked_services count"
        2
        (List.length entry.Directory_registry.linked_services)
  | Error (`Msg err) -> Alcotest.fail (Printf.sprintf "Parse failed: %s" err)

let test_directory_entry_of_yojson_backward_compat () =
  (* Test backward compatibility: last_used_at defaults to created_at if missing *)
  let json =
    `Assoc
      [
        ("path", `String "/var/lib/tezos/node");
        ("dir_type", `String "node_data_dir");
        ("created_at", `String "2026-01-01 12:00:00");
        ("linked_services", `List []);
      ]
  in
  let result = DR.directory_entry_of_yojson json in
  match result with
  | Ok entry ->
      check
        string
        "last_used_at defaults to created_at"
        "2026-01-01 12:00:00"
        entry.Directory_registry.last_used_at
  | Error (`Msg err) -> Alcotest.fail (Printf.sprintf "Parse failed: %s" err)

let test_directory_entry_of_yojson_missing_path () =
  let json =
    `Assoc
      [
        ("dir_type", `String "node_data_dir");
        ("created_at", `String "2026-01-01 12:00:00");
        ("linked_services", `List []);
      ]
  in
  let result = DR.directory_entry_of_yojson json in
  match result with
  | Ok _ -> Alcotest.fail "Expected Error for missing path"
  | Error _ -> ()

let test_directory_entry_of_yojson_invalid_dir_type () =
  let json =
    `Assoc
      [
        ("path", `String "/var/lib/tezos/node");
        ("dir_type", `String "invalid_type");
        ("created_at", `String "2026-01-01 12:00:00");
        ("linked_services", `List []);
      ]
  in
  let result = DR.directory_entry_of_yojson json in
  match result with
  | Ok _ -> Alcotest.fail "Expected Error for invalid dir_type"
  | Error _ -> ()

let test_directory_entry_roundtrip () =
  let entry : Directory_registry.directory_entry =
    {
      path = "/home/user/.tezos-client";
      dir_type = Directory_registry.Client_base_dir;
      created_at = "2026-01-15 08:00:00";
      last_used_at = "2026-01-25 14:20:00";
      linked_services = ["baker-alpha"; "baker-beta"];
    }
  in
  let json = DR.directory_entry_to_yojson entry in
  let result = DR.directory_entry_of_yojson json in
  match result with
  | Ok parsed ->
      check string "path roundtrip" entry.path parsed.Directory_registry.path ;
      check
        string
        "created_at roundtrip"
        entry.created_at
        parsed.Directory_registry.created_at ;
      check
        string
        "last_used_at roundtrip"
        entry.last_used_at
        parsed.Directory_registry.last_used_at
  | Error (`Msg err) ->
      Alcotest.fail (Printf.sprintf "Roundtrip parse failed: %s" err)

(** {2 Test suite} *)

let () =
  Alcotest.run
    "Directory_registry"
    [
      ( "dir_type JSON",
        [
          test_case "to_yojson node_data_dir" `Quick test_dir_type_to_yojson_node;
          test_case
            "to_yojson client_base_dir"
            `Quick
            test_dir_type_to_yojson_client;
          test_case
            "to_yojson app_bin_dir"
            `Quick
            test_dir_type_to_yojson_app_bin;
          test_case
            "of_yojson node_data_dir"
            `Quick
            test_dir_type_of_yojson_node;
          test_case
            "of_yojson client_base_dir"
            `Quick
            test_dir_type_of_yojson_client;
          test_case
            "of_yojson app_bin_dir"
            `Quick
            test_dir_type_of_yojson_app_bin;
          test_case "of_yojson invalid" `Quick test_dir_type_of_yojson_invalid;
        ] );
      ( "directory_entry JSON",
        [
          test_case "to_yojson" `Quick test_directory_entry_to_yojson;
          test_case "of_yojson" `Quick test_directory_entry_of_yojson;
          test_case
            "of_yojson backward compat"
            `Quick
            test_directory_entry_of_yojson_backward_compat;
          test_case
            "of_yojson missing path"
            `Quick
            test_directory_entry_of_yojson_missing_path;
          test_case
            "of_yojson invalid dir_type"
            `Quick
            test_directory_entry_of_yojson_invalid_dir_type;
          test_case "roundtrip" `Quick test_directory_entry_roundtrip;
        ] );
    ]
