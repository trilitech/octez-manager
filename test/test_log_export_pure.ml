(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Tests for Log_export pure functions.

    Covers get_instance_details, format_timestamp, and export_filename. *)

open Alcotest
module LE = Octez_manager_lib.Log_export

let make_svc ?(instance = "my-node") ?(role = "node") ?(network = "mainnet")
    ?(history_mode = Octez_manager_lib.History_mode.Rolling)
    ?(data_dir = "/var/lib/octez/my-node") ?(rpc_addr = "127.0.0.1:8732")
    ?(net_addr = "[::]:9732") ?(service_user = "tezos")
    ?(app_bin_dir = "/usr/local/bin") ?(created_at = "2026-01-15 10:30:00")
    ?(depends_on = None) ?(dependents = []) () : Octez_manager_lib.Service.t =
  {
    instance;
    role;
    network;
    history_mode;
    data_dir;
    rpc_addr = Octez_manager_lib.Rpc_addr.of_string rpc_addr;
    net_addr;
    service_user;
    app_bin_dir;
    bin_source = None;
    created_at;
    logging_mode = Octez_manager_lib.Logging_mode.Journald;
    snapshot_auto = false;
    snapshot_uri = None;
    snapshot_network_slug = None;
    snapshot_no_check = false;
    extra_args = [];
    depends_on;
    dependents;
    signer_mode = None;
    signer_uri = None;
    group = None;
    enabled_on_boot = None;
  }

let contains_substring = Test_string_helpers.contains_substring

(* ── get_instance_details ──────────────────────────────────── *)

let test_details_contains_instance () =
  let svc = make_svc ~instance:"test-node-1" () in
  let result = LE.For_tests.get_instance_details ~svc in
  check bool "contains instance" true (contains_substring result "test-node-1")

let test_details_contains_role () =
  let svc = make_svc ~role:"baker" () in
  let result = LE.For_tests.get_instance_details ~svc in
  check bool "contains role" true (contains_substring result "baker")

let test_details_contains_network () =
  let svc = make_svc ~network:"shadownet" () in
  let result = LE.For_tests.get_instance_details ~svc in
  check bool "contains network" true (contains_substring result "shadownet")

let test_details_contains_history_mode () =
  let svc = make_svc ~history_mode:Octez_manager_lib.History_mode.Full () in
  let result = LE.For_tests.get_instance_details ~svc in
  check bool "contains full" true (contains_substring result "full")

let test_details_contains_data_dir () =
  let svc = make_svc ~data_dir:"/custom/data" () in
  let result = LE.For_tests.get_instance_details ~svc in
  check bool "contains data dir" true (contains_substring result "/custom/data")

let test_details_contains_rpc_addr () =
  let svc = make_svc ~rpc_addr:"0.0.0.0:8733" () in
  let result = LE.For_tests.get_instance_details ~svc in
  check bool "contains rpc addr" true (contains_substring result "0.0.0.0:8733")

let test_details_contains_service_user () =
  let svc = make_svc ~service_user:"octez" () in
  let result = LE.For_tests.get_instance_details ~svc in
  check bool "contains user" true (contains_substring result "octez")

let test_details_no_depends_on () =
  let svc = make_svc ~depends_on:None () in
  let result = LE.For_tests.get_instance_details ~svc in
  check bool "contains none" true (contains_substring result "(none)")

let test_details_with_depends_on () =
  let svc = make_svc ~depends_on:(Some "parent-node") () in
  let result = LE.For_tests.get_instance_details ~svc in
  check bool "contains parent" true (contains_substring result "parent-node")

let test_details_no_dependents () =
  let svc = make_svc ~dependents:[] () in
  let result = LE.For_tests.get_instance_details ~svc in
  check bool "contains none for deps" true (contains_substring result "(none)")

let test_details_with_dependents () =
  let svc = make_svc ~dependents:["baker-1"; "accuser-1"] () in
  let result = LE.For_tests.get_instance_details ~svc in
  check bool "contains baker" true (contains_substring result "baker-1") ;
  check bool "contains accuser" true (contains_substring result "accuser-1")

let test_details_has_header () =
  let svc = make_svc () in
  let result = LE.For_tests.get_instance_details ~svc in
  check
    bool
    "contains header"
    true
    (contains_substring result "Instance Details")

let test_details_contains_created_at () =
  let svc = make_svc ~created_at:"2026-01-15 10:30:00" () in
  let result = LE.For_tests.get_instance_details ~svc in
  check
    bool
    "contains created_at"
    true
    (contains_substring result "2026-01-15 10:30:00")

(* ── format_timestamp ──────────────────────────────────────── *)

let test_timestamp_format () =
  (* 2026-01-15 00:00:00 UTC *)
  let ts = LE.For_tests.format_timestamp 0.0 in
  (* Should be YYYYMMDD-HHMMSS format *)
  check int "length" 15 (String.length ts) ;
  check bool "has dash" true (String.get ts 8 = '-')

let test_timestamp_nonzero () =
  (* Use a known timestamp - epoch is 1970-01-01 *)
  let ts = LE.For_tests.format_timestamp 1000000000.0 in
  check int "length" 15 (String.length ts) ;
  check bool "has dash" true (String.get ts 8 = '-')

(* ── export_filename ──────────────────────────────────────── *)

let test_export_filename_basic () =
  let result =
    LE.For_tests.export_filename
      ~instance:"my-node"
      ~timestamp:"20260115-103000"
  in
  check string "filename" "my-node-logs-20260115-103000" result

let test_export_filename_complex_instance () =
  let result =
    LE.For_tests.export_filename
      ~instance:"ghost-baker-1"
      ~timestamp:"20260220-140000"
  in
  check string "filename" "ghost-baker-1-logs-20260220-140000" result

(* ── Suite ────────────────────────────────────────────────── *)

let () =
  Alcotest.run
    "Log_export"
    [
      ( "get_instance_details",
        [
          test_case "contains instance" `Quick test_details_contains_instance;
          test_case "contains role" `Quick test_details_contains_role;
          test_case "contains network" `Quick test_details_contains_network;
          test_case
            "contains history mode"
            `Quick
            test_details_contains_history_mode;
          test_case "contains data dir" `Quick test_details_contains_data_dir;
          test_case "contains rpc addr" `Quick test_details_contains_rpc_addr;
          test_case
            "contains service user"
            `Quick
            test_details_contains_service_user;
          test_case "no depends_on" `Quick test_details_no_depends_on;
          test_case "with depends_on" `Quick test_details_with_depends_on;
          test_case "no dependents" `Quick test_details_no_dependents;
          test_case "with dependents" `Quick test_details_with_dependents;
          test_case "has header" `Quick test_details_has_header;
          test_case
            "contains created_at"
            `Quick
            test_details_contains_created_at;
        ] );
      ( "format_timestamp",
        [
          test_case "epoch" `Quick test_timestamp_format;
          test_case "nonzero" `Quick test_timestamp_nonzero;
        ] );
      ( "export_filename",
        [
          test_case "basic" `Quick test_export_filename_basic;
          test_case
            "complex instance"
            `Quick
            test_export_filename_complex_instance;
        ] );
    ]
