(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Unit tests for External_service_detector pure functions. *)

open Alcotest
module ESD = Octez_manager_lib.External_service_detector

(* ── contains_octez_binary ───────────────────────────────────── *)

let test_contains_node () =
  check
    bool
    "octez-node"
    true
    (ESD.For_tests.contains_octez_binary "/usr/bin/octez-node run")

let test_contains_baker () =
  check
    bool
    "octez-baker"
    true
    (ESD.For_tests.contains_octez_binary "/usr/bin/octez-baker run")

let test_contains_accuser () =
  check
    bool
    "octez-accuser"
    true
    (ESD.For_tests.contains_octez_binary "/usr/bin/octez-accuser")

let test_contains_dal_node () =
  check
    bool
    "octez-dal-node"
    true
    (ESD.For_tests.contains_octez_binary "/usr/bin/octez-dal-node run")

let test_contains_tezos_baker () =
  check
    bool
    "tezos-baker"
    true
    (ESD.For_tests.contains_octez_binary "/usr/bin/tezos-baker")

let test_contains_tezos_accuser () =
  check
    bool
    "tezos-accuser"
    true
    (ESD.For_tests.contains_octez_binary "/usr/bin/tezos-accuser")

let test_contains_unrelated () =
  check
    bool
    "nginx"
    false
    (ESD.For_tests.contains_octez_binary "/usr/sbin/nginx")

let test_contains_empty () =
  check bool "empty" false (ESD.For_tests.contains_octez_binary "")

let test_contains_case_insensitive () =
  check
    bool
    "OCTEZ-NODE"
    true
    (ESD.For_tests.contains_octez_binary "OCTEZ-NODE run")

(* ── chain_id_to_network ─────────────────────────────────────── *)

let test_chain_mainnet () =
  check
    (option string)
    "mainnet"
    (Some "mainnet")
    (ESD.For_tests.chain_id_to_network "NetXdQprcVkpaWU")

let test_chain_ghostnet () =
  check
    (option string)
    "ghostnet"
    (Some "ghostnet")
    (ESD.For_tests.chain_id_to_network "NetXnHfVqm9iesp")

let test_chain_unknown () =
  check
    (option string)
    "unknown"
    None
    (ESD.For_tests.chain_id_to_network "NetXfoo")

(* ── extract_command_from_systemd_format ─────────────────────── *)

let test_extract_simple () =
  check
    (option string)
    "simple"
    (Some "/usr/bin/octez-node run")
    (ESD.For_tests.extract_command_from_systemd_format
       "{ path=/usr/bin/octez-node ; argv[]=/usr/bin/octez-node run ; }")

let test_extract_no_argv () =
  check
    (option string)
    "no argv"
    None
    (ESD.For_tests.extract_command_from_systemd_format "some random string")

(* ── is_managed_unit_name ────────────────────────────────────── *)

let test_managed_node () =
  check
    bool
    "octez-node@"
    true
    (ESD.For_tests.is_managed_unit_name "octez-node@my-node.service")

let test_managed_baker () =
  check
    bool
    "octez-baker@"
    true
    (ESD.For_tests.is_managed_unit_name "octez-baker@mainnet.service")

let test_managed_dal () =
  check
    bool
    "octez-dal-node@"
    true
    (ESD.For_tests.is_managed_unit_name "octez-dal-node@mainnet.service")

let test_not_managed () =
  check
    bool
    "tezos-node"
    false
    (ESD.For_tests.is_managed_unit_name "tezos-node.service")

let test_not_managed_no_at () =
  check
    bool
    "octez-node (no @)"
    false
    (ESD.For_tests.is_managed_unit_name "octez-node.service")

(* ── string_contains ─────────────────────────────────────────── *)

let test_string_contains_yes () =
  check
    bool
    "found"
    true
    (ESD.For_tests.string_contains ~needle:"hello" "say hello world")

let test_string_contains_no () =
  check
    bool
    "not found"
    false
    (ESD.For_tests.string_contains ~needle:"goodbye" "say hello world")

let test_string_contains_empty_needle () =
  check
    bool
    "empty needle"
    true
    (ESD.For_tests.string_contains ~needle:"" "anything")

let test_string_contains_empty_haystack () =
  check
    bool
    "empty haystack"
    false
    (ESD.For_tests.string_contains ~needle:"x" "")

(* ── Suite ───────────────────────────────────────────────────── *)

let () =
  run
    "External_service_detector"
    [
      ( "contains_octez_binary",
        [
          test_case "node" `Quick test_contains_node;
          test_case "baker" `Quick test_contains_baker;
          test_case "accuser" `Quick test_contains_accuser;
          test_case "dal-node" `Quick test_contains_dal_node;
          test_case "tezos-baker" `Quick test_contains_tezos_baker;
          test_case "tezos-accuser" `Quick test_contains_tezos_accuser;
          test_case "unrelated" `Quick test_contains_unrelated;
          test_case "empty" `Quick test_contains_empty;
          test_case "case insensitive" `Quick test_contains_case_insensitive;
        ] );
      ( "chain_id_to_network",
        [
          test_case "mainnet" `Quick test_chain_mainnet;
          test_case "ghostnet" `Quick test_chain_ghostnet;
          test_case "unknown" `Quick test_chain_unknown;
        ] );
      ( "extract_command_from_systemd_format",
        [
          test_case "simple" `Quick test_extract_simple;
          test_case "no argv" `Quick test_extract_no_argv;
        ] );
      ( "is_managed_unit_name",
        [
          test_case "node" `Quick test_managed_node;
          test_case "baker" `Quick test_managed_baker;
          test_case "dal" `Quick test_managed_dal;
          test_case "not managed" `Quick test_not_managed;
          test_case "no at sign" `Quick test_not_managed_no_at;
        ] );
      ( "string_contains",
        [
          test_case "found" `Quick test_string_contains_yes;
          test_case "not found" `Quick test_string_contains_no;
          test_case "empty needle" `Quick test_string_contains_empty_needle;
          test_case "empty haystack" `Quick test_string_contains_empty_haystack;
        ] );
    ]
