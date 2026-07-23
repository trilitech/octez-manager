(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Octez_manager_ui

(* ============================================================ *)
(* Taquito JSON Parsing Tests                                    *)
(* ============================================================ *)

let test_parse_simple_list () =
  let json =
    {|[
      {"rpc": "https://mainnet.ecadinfra.com", "name": "ECAD Infra", "network": "mainnet"},
      {"rpc": "https://shadownet.tezos.com", "name": "Shadownet", "network": "shadownet"}
    ]|}
  in
  let nodes = Public_nodes_cache.parse_taquito_json json in
  Alcotest.(check int) "two nodes" 2 (List.length nodes) ;
  let first = List.hd nodes in
  Alcotest.(check string) "first label" "ECAD Infra" first.label ;
  Alcotest.(check string)
    "first rpc"
    "https://mainnet.ecadinfra.com"
    first.rpc_addr

let test_parse_with_rpc_url () =
  let json =
    {|[
      {"rpc_url": "https://node.example.com", "name": "Example Node"}
    ]|}
  in
  let nodes = Public_nodes_cache.parse_taquito_json json in
  Alcotest.(check int) "one node" 1 (List.length nodes) ;
  let first = List.hd nodes in
  Alcotest.(check string)
    "rpc_url parsed"
    "https://node.example.com"
    first.rpc_addr

let test_parse_taquito_format () =
  let json =
    {|{
      "providers": [
        {"id": "ecad", "name": "ECAD Infra"},
        {"id": "smartpy", "name": "SmartPy"}
      ],
      "rpc_endpoints": [
        {"url": "https://mainnet.ecadinfra.com", "provider": "ecad", "network": "mainnet"},
        {"url": "https://mainnet.smartpy.io", "provider": "smartpy", "network": "mainnet"}
      ]
    }|}
  in
  let nodes = Public_nodes_cache.parse_taquito_json json in
  Alcotest.(check int) "two nodes" 2 (List.length nodes) ;
  let first = List.hd nodes in
  (* Network is NOT included in label - stored separately *)
  Alcotest.(check string)
    "provider name used without network"
    "ECAD Infra"
    first.label

let test_parse_empty () =
  let json = "[]" in
  let nodes = Public_nodes_cache.parse_taquito_json json in
  Alcotest.(check int) "empty" 0 (List.length nodes)

let test_parse_invalid_json () =
  let json = "not valid json" in
  let nodes = Public_nodes_cache.parse_taquito_json json in
  Alcotest.(check int) "empty on invalid" 0 (List.length nodes)

let test_parse_missing_rpc () =
  let json = {|[{"name": "No RPC field"}]|} in
  let nodes = Public_nodes_cache.parse_taquito_json json in
  Alcotest.(check int) "skips entry without rpc" 0 (List.length nodes)

(* ============================================================ *)
(* Service Conversion Tests                                      *)
(* ============================================================ *)

let test_to_service () =
  let info : Public_nodes_cache.node_info =
    {
      label = "Test Node";
      rpc_addr = "https://test.node";
      network = Some "mainnet";
    }
  in
  let svc = Public_nodes_cache.to_service info in
  Alcotest.(check string)
    "instance"
    "Test Node"
    svc.Octez_manager_lib.Service.instance ;
  Alcotest.(check string)
    "network"
    "mainnet"
    svc.Octez_manager_lib.Service.network ;
  Alcotest.(check string)
    "rpc_addr"
    "https://test.node"
    (Octez_manager_lib.Rpc_addr.to_string
       svc.Octez_manager_lib.Service.rpc_addr) ;
  Alcotest.(check string)
    "data_dir empty"
    ""
    svc.Octez_manager_lib.Service.data_dir

let test_to_service_no_network () =
  let info : Public_nodes_cache.node_info =
    {
      label = "Unknown Network";
      rpc_addr = "https://unknown.node";
      network = None;
    }
  in
  let svc = Public_nodes_cache.to_service info in
  Alcotest.(check string)
    "defaults to unknown"
    "unknown"
    svc.Octez_manager_lib.Service.network

(* ============================================================ *)
(* Network extraction from URLs                                  *)
(* ============================================================ *)

(* Regression tests for #970: RPC endpoints of teztnets networks missing
   from the static known-network list (e.g. ushuaianet) were grouped under
   "Unknown" in the RPC browser. The network slug is now derived from the
   teztnets hostname itself. *)

let check_network url expected =
  Alcotest.(check (option string))
    url
    expected
    (Public_nodes_cache.extract_network_from_url url)

let test_extract_known_static_networks () =
  check_network "https://mainnet.smartpy.io" (Some "mainnet") ;
  check_network "https://tezos-shadownet.octez.io" (Some "shadownet") ;
  check_network "https://rpc.tzbeta.net" (Some "mainnet") ;
  check_network "https://rpc.tzkt.io/mainnet" (Some "mainnet")

let test_extract_teztnets_hostname_slug () =
  (* Networks absent from the static list resolve via the hostname. *)
  check_network "https://rpc.ushuaianet.teztnets.com" (Some "ushuaianet") ;
  check_network "https://rpc.bakingnet.teztnets.com" (Some "bakingnet") ;
  check_network "https://futurenet.teztnets.com" (Some "futurenet")

let test_extract_teztnets_static_list_precedence () =
  (* Networks in the static list keep their current (prefix-free) name even
     when the hostname carries a rotation suffix. *)
  check_network
    "https://rpc.weeklynet-2026-07-15.teztnets.com"
    (Some "weeklynet")

let test_extract_unknown_urls () =
  check_network "https://rpc.example.org" None ;
  (* A bare teztnets host without a network slug stays unknown. *)
  check_network "https://teztnets.com" None ;
  check_network "https://rpc.teztnets.com" None

let extraction_tests =
  [
    Alcotest.test_case
      "static networks"
      `Quick
      test_extract_known_static_networks;
    Alcotest.test_case
      "teztnets hostname slug"
      `Quick
      test_extract_teztnets_hostname_slug;
    Alcotest.test_case
      "static list precedence"
      `Quick
      test_extract_teztnets_static_list_precedence;
    Alcotest.test_case "unknown urls" `Quick test_extract_unknown_urls;
  ]

(* ============================================================ *)
(* Test Runner                                                   *)
(* ============================================================ *)

let () =
  Alcotest.run
    "Public_nodes_cache"
    [
      ( "parse_taquito_json",
        [
          Alcotest.test_case "simple list" `Quick test_parse_simple_list;
          Alcotest.test_case "rpc_url field" `Quick test_parse_with_rpc_url;
          Alcotest.test_case "taquito format" `Quick test_parse_taquito_format;
          Alcotest.test_case "empty" `Quick test_parse_empty;
          Alcotest.test_case "invalid json" `Quick test_parse_invalid_json;
          Alcotest.test_case "missing rpc" `Quick test_parse_missing_rpc;
        ] );
      ( "to_service",
        [
          Alcotest.test_case "basic" `Quick test_to_service;
          Alcotest.test_case "no network" `Quick test_to_service_no_network;
        ] );
      ("extract_network_from_url", extraction_tests);
    ]
