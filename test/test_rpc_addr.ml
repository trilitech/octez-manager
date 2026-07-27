(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Unit tests for Rpc_addr module *)

open Octez_manager_lib

let test_to_endpoint_without_scheme () =
  let addr = Rpc_addr.of_string "127.0.0.1:8732" in
  let endpoint = Rpc_addr.to_endpoint addr in
  Alcotest.(check string)
    "should prepend http:// to address without scheme"
    "http://127.0.0.1:8732"
    endpoint

let test_to_endpoint_with_http_scheme () =
  let addr = Rpc_addr.of_string "http://127.0.0.1:8732" in
  let endpoint = Rpc_addr.to_endpoint addr in
  Alcotest.(check string)
    "should not double-prefix http:// when already present"
    "http://127.0.0.1:8732"
    endpoint

let test_to_endpoint_with_https_scheme () =
  let addr = Rpc_addr.of_string "https://example.com:8732" in
  let endpoint = Rpc_addr.to_endpoint addr in
  Alcotest.(check string)
    "should preserve https:// scheme"
    "https://example.com:8732"
    endpoint

let test_to_endpoint_with_mixed_case_http () =
  let addr = Rpc_addr.of_string "HTTP://127.0.0.1:8732" in
  let endpoint = Rpc_addr.to_endpoint addr in
  Alcotest.(check string)
    "should handle HTTP:// (uppercase) correctly"
    "HTTP://127.0.0.1:8732"
    endpoint

let test_to_endpoint_with_mixed_case_https () =
  let addr = Rpc_addr.of_string "HTTPS://example.com:8732" in
  let endpoint = Rpc_addr.to_endpoint addr in
  Alcotest.(check string)
    "should handle HTTPS:// (uppercase) correctly"
    "HTTPS://example.com:8732"
    endpoint

let test_to_endpoint_empty_string () =
  let addr = Rpc_addr.of_string "" in
  let endpoint = Rpc_addr.to_endpoint addr in
  Alcotest.(check string)
    "should fall back to default for empty string"
    "http://127.0.0.1:8732"
    endpoint

let test_to_endpoint_whitespace_only () =
  let addr = Rpc_addr.of_string "   " in
  let endpoint = Rpc_addr.to_endpoint addr in
  Alcotest.(check string)
    "should fall back to default for whitespace-only string"
    "http://127.0.0.1:8732"
    endpoint

let test_to_endpoint_with_path () =
  let addr = Rpc_addr.of_string "http://127.0.0.1:8732/some/path" in
  let endpoint = Rpc_addr.to_endpoint addr in
  Alcotest.(check string)
    "should preserve path in URL"
    "http://127.0.0.1:8732/some/path"
    endpoint

(* ============================================================ *)
(* IPv6 host/port tests (see #1006) *)
(* ============================================================ *)

let test_host_bracketed_ipv6 () =
  let addr = Rpc_addr.of_string "[::1]:8732" in
  Alcotest.(check (option string))
    "host keeps its brackets"
    (Some "[::1]")
    (Rpc_addr.host addr)

let test_port_bracketed_ipv6 () =
  let addr = Rpc_addr.of_string "[::1]:8732" in
  Alcotest.(check (option int))
    "port extracted"
    (Some 8732)
    (Rpc_addr.port addr)

let test_host_bare_ipv6_rejected () =
  let addr = Rpc_addr.of_string "fe80::1:9732" in
  Alcotest.(check (option string))
    "bare IPv6 host rejected (ambiguous)"
    None
    (Rpc_addr.host addr)

let test_port_bare_ipv6_rejected () =
  let addr = Rpc_addr.of_string "fe80::1:9732" in
  Alcotest.(check (option int))
    "bare IPv6 port rejected (ambiguous)"
    None
    (Rpc_addr.port addr)

let test_round_trip_preserves_bracketed_ipv6 () =
  let raw = "[::1]:8732" in
  let addr = Rpc_addr.of_string raw in
  Alcotest.(check string)
    "of_string / to_string round-trip preserves brackets"
    raw
    (Rpc_addr.to_string addr)

let () =
  let open Alcotest in
  run
    "Rpc_addr"
    [
      ( "to_endpoint",
        [
          test_case "without scheme" `Quick test_to_endpoint_without_scheme;
          test_case
            "with http:// scheme"
            `Quick
            test_to_endpoint_with_http_scheme;
          test_case
            "with https:// scheme"
            `Quick
            test_to_endpoint_with_https_scheme;
          test_case
            "with mixed case HTTP://"
            `Quick
            test_to_endpoint_with_mixed_case_http;
          test_case
            "with mixed case HTTPS://"
            `Quick
            test_to_endpoint_with_mixed_case_https;
          test_case "empty string" `Quick test_to_endpoint_empty_string;
          test_case "whitespace only" `Quick test_to_endpoint_whitespace_only;
          test_case "with path" `Quick test_to_endpoint_with_path;
        ] );
      ( "ipv6",
        [
          test_case "host keeps brackets" `Quick test_host_bracketed_ipv6;
          test_case "port extracted" `Quick test_port_bracketed_ipv6;
          test_case "bare host rejected" `Quick test_host_bare_ipv6_rejected;
          test_case "bare port rejected" `Quick test_port_bare_ipv6_rejected;
          test_case
            "round-trip preserves brackets"
            `Quick
            test_round_trip_preserves_bracketed_ipv6;
        ] );
    ]
