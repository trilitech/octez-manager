(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Octez_manager_lib

let option_string = Alcotest.(option string)

(* ── Tests: extract_binary_path ──────────────────────────────────── *)

let test_extract_simple_path () =
  Alcotest.(check option_string)
    "simple binary"
    (Some "/usr/bin/octez-node")
    (Process_scanner.For_tests.extract_binary_path "/usr/bin/octez-node run")

let test_extract_bare_binary () =
  Alcotest.(check option_string)
    "bare binary"
    (Some "octez-node")
    (Process_scanner.For_tests.extract_binary_path "octez-node")

let test_extract_with_many_args () =
  Alcotest.(check option_string)
    "with args"
    (Some "/usr/local/bin/octez-baker")
    (Process_scanner.For_tests.extract_binary_path
       "/usr/local/bin/octez-baker run with local node /data \
        --liquidity-baking-toggle-vote pass")

let test_extract_empty_string () =
  Alcotest.(check option_string)
    "empty string"
    None
    (Process_scanner.For_tests.extract_binary_path "")

let test_extract_starts_with_dash () =
  Alcotest.(check option_string)
    "starts with dash"
    None
    (Process_scanner.For_tests.extract_binary_path "-bash")

let test_extract_whitespace_only () =
  Alcotest.(check option_string)
    "whitespace only"
    None
    (Process_scanner.For_tests.extract_binary_path "   ")

(* ── Tests: is_octez_binary ──────────────────────────────────────── *)

let test_is_octez_node () =
  Alcotest.(check bool)
    "octez-node"
    true
    (Process_scanner.For_tests.is_octez_binary
       "octez-node run --data-dir /var/lib/octez")

let test_is_octez_node_full_path () =
  Alcotest.(check bool)
    "full path octez-node"
    true
    (Process_scanner.For_tests.is_octez_binary "/usr/local/bin/octez-node run")

let test_is_octez_baker () =
  Alcotest.(check bool)
    "octez-baker"
    true
    (Process_scanner.For_tests.is_octez_binary
       "octez-baker run with local node /data")

let test_is_octez_accuser () =
  Alcotest.(check bool)
    "octez-accuser"
    true
    (Process_scanner.For_tests.is_octez_binary "/usr/bin/octez-accuser run")

let test_is_octez_dal_node () =
  Alcotest.(check bool)
    "octez-dal-node"
    true
    (Process_scanner.For_tests.is_octez_binary
       "octez-dal-node run --data-dir /dal")

let test_is_legacy_tezos_node () =
  Alcotest.(check bool)
    "tezos-node (legacy)"
    true
    (Process_scanner.For_tests.is_octez_binary "tezos-node run")

let test_is_legacy_tezos_baker () =
  Alcotest.(check bool)
    "tezos-baker (legacy)"
    true
    (Process_scanner.For_tests.is_octez_binary "/opt/tezos/tezos-baker run")

let test_is_legacy_tezos_accuser () =
  Alcotest.(check bool)
    "tezos-accuser (legacy)"
    true
    (Process_scanner.For_tests.is_octez_binary "tezos-accuser run")

let test_is_not_octez_validator () =
  Alcotest.(check bool)
    "octez-validator excluded"
    false
    (Process_scanner.For_tests.is_octez_binary
       "octez-validator --validation-socket")

let test_is_not_octez_validator_hypervisor () =
  Alcotest.(check bool)
    "octez-validator-hypervisor excluded"
    false
    (Process_scanner.For_tests.is_octez_binary "octez-validator-hypervisor")

let test_is_not_random_binary () =
  Alcotest.(check bool)
    "random binary excluded"
    false
    (Process_scanner.For_tests.is_octez_binary "/usr/bin/nginx -g daemon off")

let test_is_not_grep () =
  (* pgrep -f 'octez' will match 'grep octez' itself *)
  Alcotest.(check bool)
    "grep excluded"
    false
    (Process_scanner.For_tests.is_octez_binary "grep octez")

let test_is_not_empty () =
  Alcotest.(check bool)
    "empty string excluded"
    false
    (Process_scanner.For_tests.is_octez_binary "")

let test_is_not_octez_manager () =
  Alcotest.(check bool)
    "octez-manager excluded"
    false
    (Process_scanner.For_tests.is_octez_binary "octez-manager ui")

let test_is_not_octez_client () =
  Alcotest.(check bool)
    "octez-client excluded"
    false
    (Process_scanner.For_tests.is_octez_binary "octez-client transfer")

(* ── Tests: contains_substring ───────────────────────────────────── *)

let test_contains_present () =
  Alcotest.(check bool)
    "substring present"
    true
    (Process_scanner.For_tests.contains_substring "hello world" "world")

let test_contains_absent () =
  Alcotest.(check bool)
    "substring absent"
    false
    (Process_scanner.For_tests.contains_substring "hello world" "xyz")

let test_contains_empty_needle () =
  Alcotest.(check bool)
    "empty needle"
    true
    (Process_scanner.For_tests.contains_substring "hello" "")

let test_contains_exact_match () =
  Alcotest.(check bool)
    "exact match"
    true
    (Process_scanner.For_tests.contains_substring "octez-node" "octez-node")

let test_contains_service_in_cgroup () =
  Alcotest.(check bool)
    "service in cgroup path"
    true
    (Process_scanner.For_tests.contains_substring
       "0::/system.slice/octez-node@mainnet.service"
       ".service")

(* ── Test suite ──────────────────────────────────────────────────── *)

let () =
  Alcotest.run
    "Process_scanner"
    [
      ( "extract_binary_path",
        [
          Alcotest.test_case "simple path" `Quick test_extract_simple_path;
          Alcotest.test_case "bare binary" `Quick test_extract_bare_binary;
          Alcotest.test_case "with many args" `Quick test_extract_with_many_args;
          Alcotest.test_case "empty string" `Quick test_extract_empty_string;
          Alcotest.test_case
            "starts with dash"
            `Quick
            test_extract_starts_with_dash;
          Alcotest.test_case
            "whitespace only"
            `Quick
            test_extract_whitespace_only;
        ] );
      ( "is_octez_binary",
        [
          Alcotest.test_case "octez-node" `Quick test_is_octez_node;
          Alcotest.test_case "full path" `Quick test_is_octez_node_full_path;
          Alcotest.test_case "octez-baker" `Quick test_is_octez_baker;
          Alcotest.test_case "octez-accuser" `Quick test_is_octez_accuser;
          Alcotest.test_case "octez-dal-node" `Quick test_is_octez_dal_node;
          Alcotest.test_case
            "legacy tezos-node"
            `Quick
            test_is_legacy_tezos_node;
          Alcotest.test_case
            "legacy tezos-baker"
            `Quick
            test_is_legacy_tezos_baker;
          Alcotest.test_case
            "legacy tezos-accuser"
            `Quick
            test_is_legacy_tezos_accuser;
          Alcotest.test_case "not validator" `Quick test_is_not_octez_validator;
          Alcotest.test_case
            "not validator-hypervisor"
            `Quick
            test_is_not_octez_validator_hypervisor;
          Alcotest.test_case
            "not random binary"
            `Quick
            test_is_not_random_binary;
          Alcotest.test_case "not grep" `Quick test_is_not_grep;
          Alcotest.test_case "not empty" `Quick test_is_not_empty;
          Alcotest.test_case
            "not octez-manager"
            `Quick
            test_is_not_octez_manager;
          Alcotest.test_case "not octez-client" `Quick test_is_not_octez_client;
        ] );
      ( "contains_substring",
        [
          Alcotest.test_case "present" `Quick test_contains_present;
          Alcotest.test_case "absent" `Quick test_contains_absent;
          Alcotest.test_case "empty needle" `Quick test_contains_empty_needle;
          Alcotest.test_case "exact match" `Quick test_contains_exact_match;
          Alcotest.test_case
            "cgroup path"
            `Quick
            test_contains_service_in_cgroup;
        ] );
    ]
