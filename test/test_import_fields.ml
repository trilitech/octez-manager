(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Unit tests for Import field extraction and resolution. *)

open Alcotest
module Import = Octez_manager_lib.Import
module External_service = Octez_manager_lib.External_service

(* ── extract_baker_fields ────────────────────────────────────── *)

let string_list = list string

let test_extract_empty () =
  let delegates, lb_vote, remaining =
    Import.For_tests.extract_baker_fields []
  in
  check string_list "no delegates" [] delegates ;
  check (option string) "no lb vote" None lb_vote ;
  check string_list "no remaining" [] remaining

let test_extract_tz1_address () =
  let args = ["tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU"] in
  let delegates, _, remaining = Import.For_tests.extract_baker_fields args in
  check
    string_list
    "one delegate"
    ["tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU"]
    delegates ;
  check string_list "no remaining" [] remaining

let test_extract_tz2_address () =
  let args = ["tz2BFTyPeYRzxd5aiBchbXN3WCZhx7BqbMR9"] in
  let delegates, _, _ = Import.For_tests.extract_baker_fields args in
  check string_list "tz2" ["tz2BFTyPeYRzxd5aiBchbXN3WCZhx7BqbMR9"] delegates

let test_extract_tz3_address () =
  let args = ["tz3WXYtyDUNL91qfiCJtVUX746QpNv5i5ve5"] in
  let delegates, _, _ = Import.For_tests.extract_baker_fields args in
  check string_list "tz3" ["tz3WXYtyDUNL91qfiCJtVUX746QpNv5i5ve5"] delegates

let test_extract_kt1_address () =
  let args = ["KT1ExvG3EjTrvDcAU7EqLNb77agPa5u6KvnY"] in
  let delegates, _, _ = Import.For_tests.extract_baker_fields args in
  check string_list "KT1" ["KT1ExvG3EjTrvDcAU7EqLNb77agPa5u6KvnY"] delegates

let test_extract_alias () =
  let args = ["my_baker"] in
  let delegates, _, _ = Import.For_tests.extract_baker_fields args in
  check string_list "alias" ["my_baker"] delegates

let test_extract_multiple_delegates () =
  let args =
    [
      "tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU";
      "my_baker";
      "tz2BFTyPeYRzxd5aiBchbXN3WCZhx7BqbMR9";
    ]
  in
  let delegates, _, remaining = Import.For_tests.extract_baker_fields args in
  check int "3 delegates" 3 (List.length delegates) ;
  check string_list "no remaining" [] remaining

let test_extract_lb_vote () =
  let args = ["--liquidity-baking-toggle-vote"; "pass"] in
  let _, lb_vote, remaining = Import.For_tests.extract_baker_fields args in
  check (option string) "lb vote" (Some "pass") lb_vote ;
  check string_list "no remaining" [] remaining

let test_extract_lb_vote_on () =
  let args =
    [
      "--liquidity-baking-toggle-vote";
      "on";
      "tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU";
    ]
  in
  let delegates, lb_vote, remaining =
    Import.For_tests.extract_baker_fields args
  in
  check (option string) "lb on" (Some "on") lb_vote ;
  check
    string_list
    "delegate"
    ["tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU"]
    delegates ;
  check string_list "no remaining" [] remaining

let test_extract_mixed_flags_and_delegates () =
  let args =
    [
      "--metrics-addr";
      "0.0.0.0:9932";
      "tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU";
      "--verbose";
    ]
  in
  let delegates, _, remaining = Import.For_tests.extract_baker_fields args in
  check
    string_list
    "one delegate"
    ["tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU"]
    delegates ;
  check bool "has flags" true (List.length remaining > 0)

let test_extract_flags_only () =
  let args = ["--dal-node"; "http://localhost:10732"] in
  let delegates, lb_vote, remaining =
    Import.For_tests.extract_baker_fields args
  in
  check string_list "no delegates" [] delegates ;
  check (option string) "no lb" None lb_vote ;
  check int "2 remaining" 2 (List.length remaining)

let test_extract_dashes_not_delegates () =
  let args = ["--some-flag"; "-v"] in
  let delegates, _, _ = Import.For_tests.extract_baker_fields args in
  check string_list "no delegates" [] delegates

let test_extract_path_not_delegate () =
  let args = ["/usr/bin/something"] in
  let delegates, _, _ = Import.For_tests.extract_baker_fields args in
  check string_list "no delegates" [] delegates

(* ── resolve_field ───────────────────────────────────────────── *)

let test_resolve_override_wins () =
  let detected = External_service.detected ~source:"test" "detected_val" in
  match
    Import.For_tests.resolve_field
      ~override:(Some "override_val")
      ~detected
      ~field_name:"test"
  with
  | Ok v -> check string "override" "override_val" v
  | Error (`Msg m) -> fail m

let test_resolve_detected_fallback () =
  let detected = External_service.detected ~source:"test" "detected_val" in
  match
    Import.For_tests.resolve_field ~override:None ~detected ~field_name:"test"
  with
  | Ok v -> check string "detected" "detected_val" v
  | Error (`Msg m) -> fail m

let test_resolve_missing_error () =
  let detected = External_service.unknown () in
  match
    Import.For_tests.resolve_field
      ~override:None
      ~detected
      ~field_name:"network"
  with
  | Ok _ -> fail "should error"
  | Error (`Msg m) -> check bool "has field name" true (String.length m > 0)

let test_resolve_override_with_unknown () =
  let detected = External_service.unknown () in
  match
    Import.For_tests.resolve_field
      ~override:(Some "manual")
      ~detected
      ~field_name:"test"
  with
  | Ok v -> check string "manual" "manual" v
  | Error (`Msg m) -> fail m

(* ── resolve_rpc_addr / resolve_net_addr ─────────────────────── *)

let make_external_svc ?(rpc_addr = None) ?(net_addr = None) () :
    External_service.t =
  let mk_field v =
    match v with
    | Some x -> External_service.detected ~source:"test" x
    | None -> External_service.unknown ()
  in
  let unit_state : External_service.unit_state =
    {active_state = "inactive"; sub_state = "dead"; enabled = None}
  in
  let config =
    External_service.empty_config
      ~unit_name:"test-unit"
      ~exec_start:""
      ~unit_state
  in
  {
    config =
      {config with rpc_addr = mk_field rpc_addr; net_addr = mk_field net_addr};
    suggested_instance_name = "test";
  }

let test_rpc_addr_override () =
  let svc = make_external_svc () in
  let overrides =
    {Import.empty_overrides with rpc_addr = Some "1.2.3.4:8733"}
  in
  check
    string
    "override"
    "1.2.3.4:8733"
    (Import.For_tests.resolve_rpc_addr ~overrides ~external_svc:svc)

let test_rpc_addr_detected () =
  let svc = make_external_svc ~rpc_addr:(Some "10.0.0.1:8732") () in
  let overrides = Import.empty_overrides in
  check
    string
    "detected"
    "10.0.0.1:8732"
    (Import.For_tests.resolve_rpc_addr ~overrides ~external_svc:svc)

let test_rpc_addr_default () =
  let svc = make_external_svc () in
  let overrides = Import.empty_overrides in
  check
    string
    "default"
    "127.0.0.1:8732"
    (Import.For_tests.resolve_rpc_addr ~overrides ~external_svc:svc)

let test_net_addr_override () =
  let svc = make_external_svc () in
  let overrides =
    {Import.empty_overrides with net_addr = Some "0.0.0.0:9733"}
  in
  check
    string
    "override"
    "0.0.0.0:9733"
    (Import.For_tests.resolve_net_addr ~overrides ~external_svc:svc)

let test_net_addr_detected () =
  let svc = make_external_svc ~net_addr:(Some "10.0.0.1:9732") () in
  let overrides = Import.empty_overrides in
  check
    string
    "detected"
    "10.0.0.1:9732"
    (Import.For_tests.resolve_net_addr ~overrides ~external_svc:svc)

let test_net_addr_default () =
  let svc = make_external_svc () in
  let overrides = Import.empty_overrides in
  check
    string
    "default"
    "0.0.0.0:9732"
    (Import.For_tests.resolve_net_addr ~overrides ~external_svc:svc)

(* ── Suite ───────────────────────────────────────────────────── *)

let () =
  run
    "Import_fields"
    [
      ( "extract_baker_fields",
        [
          test_case "empty args" `Quick test_extract_empty;
          test_case "tz1 address" `Quick test_extract_tz1_address;
          test_case "tz2 address" `Quick test_extract_tz2_address;
          test_case "tz3 address" `Quick test_extract_tz3_address;
          test_case "KT1 address" `Quick test_extract_kt1_address;
          test_case "alias" `Quick test_extract_alias;
          test_case "multiple delegates" `Quick test_extract_multiple_delegates;
          test_case "LB vote pass" `Quick test_extract_lb_vote;
          test_case "LB vote on" `Quick test_extract_lb_vote_on;
          test_case
            "mixed flags and delegates"
            `Quick
            test_extract_mixed_flags_and_delegates;
          test_case "flags only" `Quick test_extract_flags_only;
          test_case
            "dashes not delegates"
            `Quick
            test_extract_dashes_not_delegates;
          test_case "path not delegate" `Quick test_extract_path_not_delegate;
        ] );
      ( "resolve_field",
        [
          test_case "override wins" `Quick test_resolve_override_wins;
          test_case "detected fallback" `Quick test_resolve_detected_fallback;
          test_case "missing error" `Quick test_resolve_missing_error;
          test_case
            "override with unknown"
            `Quick
            test_resolve_override_with_unknown;
        ] );
      ( "resolve_rpc_addr",
        [
          test_case "override" `Quick test_rpc_addr_override;
          test_case "detected" `Quick test_rpc_addr_detected;
          test_case "default" `Quick test_rpc_addr_default;
        ] );
      ( "resolve_net_addr",
        [
          test_case "override" `Quick test_net_addr_override;
          test_case "detected" `Quick test_net_addr_detected;
          test_case "default" `Quick test_net_addr_default;
        ] );
    ]
