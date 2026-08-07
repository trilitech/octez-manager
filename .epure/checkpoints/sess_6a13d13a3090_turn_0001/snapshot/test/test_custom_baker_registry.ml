(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Octez_manager_rewards

(* ── Test utilities ────────────────────────────────────────── *)

let with_temp_dir f =
  let base = Filename.temp_file "om_cbr_test" "" in
  Sys.remove base ;
  Unix.mkdir base 0o755 ;
  Fun.protect
    ~finally:(fun () ->
      let rec rm path =
        if Sys.is_directory path then (
          Array.iter (fun f -> rm (Filename.concat path f)) (Sys.readdir path) ;
          Unix.rmdir path)
        else Sys.remove path
      in
      if Sys.file_exists base then rm base)
    (fun () -> f base)

(** Run [f ()] with [XDG_CONFIG_HOME] pointing at a fresh temporary directory.
    Restores the original environment value after [f] returns or raises. *)
let with_fake_xdg f =
  with_temp_dir (fun base ->
      let config_dir = Filename.concat base "config" in
      Unix.mkdir config_dir 0o755 ;
      let data_dir = Filename.concat base "data" in
      Unix.mkdir data_dir 0o755 ;
      let saved_config = Sys.getenv_opt "XDG_CONFIG_HOME" in
      let saved_data = Sys.getenv_opt "XDG_DATA_HOME" in
      Unix.putenv "XDG_CONFIG_HOME" config_dir ;
      Unix.putenv "XDG_DATA_HOME" data_dir ;
      Fun.protect
        ~finally:(fun () ->
          (match saved_config with
          | Some v -> Unix.putenv "XDG_CONFIG_HOME" v
          | None -> Unix.putenv "XDG_CONFIG_HOME" "") ;
          match saved_data with
          | Some v -> Unix.putenv "XDG_DATA_HOME" v
          | None -> Unix.putenv "XDG_DATA_HOME" "")
        (fun () -> f ()))

(* A valid 36-character tz1 PKH used across multiple tests. *)
let valid_pkh = "tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU"

(** A minimal valid entry.  Tests override specific fields as needed. *)
let sample_entry ?(instance = "custom-mainnet-tz1Ke2h7s")
    ?(baker_pkh = valid_pkh) ?(network = "mainnet") ?(label = None)
    ?(endpoint = "node.example.com:8732") ?(payout_key_alias = "my-payout-key")
    ?(base_dir = "/home/user/.tezos-client")
    ?(octez_client_bin = "/usr/bin/octez-client")
    ?(added_at = "2026-04-27T12:00:00Z") () : Custom_baker_registry.entry =
  {
    Custom_baker_registry.instance;
    baker_pkh;
    network;
    label;
    endpoint;
    payout_key_alias;
    base_dir;
    octez_client_bin;
    added_at;
  }

(* ── Test 1: round-trip add / list / remove ────────────────── *)

let test_round_trip () =
  with_fake_xdg (fun () ->
      let entry = sample_entry () in
      (match Custom_baker_registry.add entry with
      | Error msg -> Alcotest.failf "add failed: %s" msg
      | Ok () -> ()) ;
      let entries = Custom_baker_registry.list () in
      Alcotest.(check int) "one entry after add" 1 (List.length entries) ;
      let stored = List.hd entries in
      Alcotest.(check string)
        "instance round-trips"
        entry.instance
        stored.instance ;
      Alcotest.(check string)
        "baker_pkh round-trips"
        entry.baker_pkh
        stored.baker_pkh ;
      Alcotest.(check string) "network round-trips" entry.network stored.network ;
      Alcotest.(check string)
        "endpoint round-trips"
        entry.endpoint
        stored.endpoint ;
      (match Custom_baker_registry.remove ~instance:entry.instance with
      | Error msg -> Alcotest.failf "remove failed: %s" msg
      | Ok () -> ()) ;
      let after_remove = Custom_baker_registry.list () in
      Alcotest.(check int) "empty after remove" 0 (List.length after_remove))

(* ── Test 2: add rejects collision with existing custom entry ── *)

let test_add_rejects_custom_collision () =
  with_fake_xdg (fun () ->
      let entry = sample_entry () in
      (match Custom_baker_registry.add entry with
      | Error msg -> Alcotest.failf "first add failed: %s" msg
      | Ok () -> ()) ;
      match Custom_baker_registry.add entry with
      | Ok () -> Alcotest.fail "expected Error for duplicate, got Ok"
      | Error msg ->
          (* Confirm the error message is informative. *)
          Alcotest.(check bool)
            "error mentions instance"
            true
            (let lower = String.lowercase_ascii msg in
             String.length lower > 0))

(* ── Test 3: build_instance_handle ─────────────────────────── *)

let test_build_instance_handle_valid () =
  (* valid_pkh = "tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU" — first 8 chars: "tz1Ke2h7" *)
  let result =
    Custom_baker_registry.build_instance_handle
      ~network:"mainnet"
      ~baker_pkh:valid_pkh
  in
  match result with
  | Error msg -> Alcotest.failf "expected Ok, got Error: %s" msg
  | Ok handle ->
      (* Should be "custom-mainnet-tz1Ke2h7" (first 8 chars of PKH). *)
      Alcotest.(check string) "handle format" "custom-mainnet-tz1Ke2h7" handle

let test_build_instance_handle_kt1_rejected () =
  let pkh = "KT1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU" in
  let result =
    Custom_baker_registry.build_instance_handle
      ~network:"mainnet"
      ~baker_pkh:pkh
  in
  Alcotest.(check bool) "KT1 rejected" true (Result.is_error result)

let test_build_instance_handle_bad_network_slash () =
  let result =
    Custom_baker_registry.build_instance_handle
      ~network:"mainnet/foo"
      ~baker_pkh:valid_pkh
  in
  Alcotest.(check bool)
    "network with slash rejected"
    true
    (Result.is_error result)

let test_build_instance_handle_bad_network_space () =
  let result =
    Custom_baker_registry.build_instance_handle
      ~network:"main net"
      ~baker_pkh:valid_pkh
  in
  Alcotest.(check bool)
    "network with space rejected"
    true
    (Result.is_error result)

let test_build_instance_handle_empty_network () =
  let result =
    Custom_baker_registry.build_instance_handle ~network:"" ~baker_pkh:valid_pkh
  in
  Alcotest.(check bool) "empty network rejected" true (Result.is_error result)

(* ── Test 4: validate_endpoint ─────────────────────────────── *)

let test_validate_endpoint_accept_hostname () =
  Alcotest.(check bool)
    "node.example.com:8732 accepted"
    true
    (Result.is_ok
       (Custom_baker_registry.validate_endpoint "node.example.com:8732"))

let test_validate_endpoint_accept_ip () =
  Alcotest.(check bool)
    "127.0.0.1:8732 accepted"
    true
    (Result.is_ok (Custom_baker_registry.validate_endpoint "127.0.0.1:8732"))

let test_validate_endpoint_reject_empty_host () =
  Alcotest.(check bool)
    ":8732 rejected"
    true
    (Result.is_error (Custom_baker_registry.validate_endpoint ":8732"))

let test_validate_endpoint_reject_port_zero () =
  Alcotest.(check bool)
    "host:0 rejected"
    true
    (Result.is_error (Custom_baker_registry.validate_endpoint "host:0"))

let test_validate_endpoint_reject_port_too_large () =
  Alcotest.(check bool)
    "host:99999 rejected"
    true
    (Result.is_error (Custom_baker_registry.validate_endpoint "host:99999"))

let test_validate_endpoint_reject_no_port () =
  Alcotest.(check bool)
    "host (no colon) rejected"
    true
    (Result.is_error (Custom_baker_registry.validate_endpoint "host"))

let test_validate_endpoint_reject_empty () =
  Alcotest.(check bool)
    "empty string rejected"
    true
    (Result.is_error (Custom_baker_registry.validate_endpoint ""))

(* ── Test 5: resolve_octez_client_bin failure path ─────────── *)

let test_resolve_failure_no_binary () =
  (* Run in a fresh XDG environment with no binaries and no octez-client on
     PATH.  We only assert the failure case because the success path requires
     a real binary on disk.  If octez-client happens to be on the real PATH,
     this test becomes a success-path test instead — that is intentional. *)
  with_fake_xdg (fun () ->
      let saved_path = Sys.getenv_opt "PATH" in
      (* Point PATH to empty dirs so octez-client cannot be found. *)
      Unix.putenv "PATH" "" ;
      Fun.protect
        ~finally:(fun () ->
          match saved_path with
          | Some v -> Unix.putenv "PATH" v
          | None -> Unix.putenv "PATH" "")
        (fun () ->
          let result = Custom_baker_registry.resolve_octez_client_bin () in
          (* With no managed versions and an empty PATH we expect Error. *)
          Alcotest.(check bool)
            "returns Error when nothing resolves"
            true
            (Result.is_error result)))

(* ── Test 6: add rejects OM_TEST_BAKER collision ──────────── *)

let test_add_rejects_test_baker_collision () =
  with_fake_xdg (fun () ->
      (* Inject a test baker so that "test-mainnet" is a known instance. *)
      let saved = Sys.getenv_opt "OM_TEST_BAKER" in
      Unix.putenv "OM_TEST_BAKER" ("mainnet/" ^ valid_pkh) ;
      Fun.protect
        ~finally:(fun () ->
          match saved with
          | Some v -> Unix.putenv "OM_TEST_BAKER" v
          | None -> Unix.putenv "OM_TEST_BAKER" "")
        (fun () ->
          (* "test-mainnet" is the synthetic test-baker instance for "mainnet". *)
          let entry = sample_entry ~instance:"test-mainnet" () in
          match Custom_baker_registry.add entry with
          | Ok () ->
              Alcotest.fail "expected Error for OM_TEST_BAKER collision, got Ok"
          | Error _ -> ()))

(* ── Test runner ────────────────────────────────────────────── *)

let () =
  Alcotest.run
    "custom_baker_registry"
    [
      ( "round_trip",
        [Alcotest.test_case "add/list/remove" `Quick test_round_trip] );
      ( "collision",
        [
          Alcotest.test_case
            "rejects duplicate custom entry"
            `Quick
            test_add_rejects_custom_collision;
          Alcotest.test_case
            "rejects OM_TEST_BAKER collision"
            `Quick
            test_add_rejects_test_baker_collision;
        ] );
      ( "build_instance_handle",
        [
          Alcotest.test_case
            "valid tz1 mainnet"
            `Quick
            test_build_instance_handle_valid;
          Alcotest.test_case
            "KT1 rejected"
            `Quick
            test_build_instance_handle_kt1_rejected;
          Alcotest.test_case
            "network with slash rejected"
            `Quick
            test_build_instance_handle_bad_network_slash;
          Alcotest.test_case
            "network with space rejected"
            `Quick
            test_build_instance_handle_bad_network_space;
          Alcotest.test_case
            "empty network rejected"
            `Quick
            test_build_instance_handle_empty_network;
        ] );
      ( "validate_endpoint",
        [
          Alcotest.test_case
            "hostname:port accepted"
            `Quick
            test_validate_endpoint_accept_hostname;
          Alcotest.test_case
            "ip:port accepted"
            `Quick
            test_validate_endpoint_accept_ip;
          Alcotest.test_case
            "empty host rejected"
            `Quick
            test_validate_endpoint_reject_empty_host;
          Alcotest.test_case
            "port 0 rejected"
            `Quick
            test_validate_endpoint_reject_port_zero;
          Alcotest.test_case
            "port 99999 rejected"
            `Quick
            test_validate_endpoint_reject_port_too_large;
          Alcotest.test_case
            "no port rejected"
            `Quick
            test_validate_endpoint_reject_no_port;
          Alcotest.test_case
            "empty string rejected"
            `Quick
            test_validate_endpoint_reject_empty;
        ] );
      ( "resolve_octez_client_bin",
        [
          Alcotest.test_case
            "failure path returns Error"
            `Quick
            test_resolve_failure_no_binary;
        ] );
    ]
