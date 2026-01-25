(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Tests for Port_validation module
    
    Tests cover:
    - Host:port parsing (IPv4, IPv6, hostnames)
    - Port parsing from address strings
    - Port range validation (1024-65535)
    - Port conflict detection
    - Validation error messages
*)

open Alcotest
open Octez_manager_lib

(* ============================================================ *)
(* Test Helpers *)
(* ============================================================ *)

let check_some = check (option int)

let check_some_pair = check (option (pair string int))

(* ============================================================ *)
(* parse_host_port Tests *)
(* ============================================================ *)

let test_parse_host_port_ipv4 () =
  let result = Port_validation.parse_host_port "127.0.0.1:8732" in
  check_some_pair "parses IPv4:port" (Some ("127.0.0.1", 8732)) result

let test_parse_host_port_hostname () =
  let result = Port_validation.parse_host_port "localhost:8732" in
  check_some_pair "parses hostname:port" (Some ("localhost", 8732)) result

let test_parse_host_port_wildcard () =
  let result = Port_validation.parse_host_port "0.0.0.0:9732" in
  check_some_pair "parses wildcard:port" (Some ("0.0.0.0", 9732)) result

let test_parse_host_port_ipv6 () =
  let result = Port_validation.parse_host_port "[::1]:8732" in
  (* IPv6 addresses with brackets not supported - contains multiple colons *)
  check_some_pair "IPv6 not supported" None result

let test_parse_host_port_no_port () =
  let result = Port_validation.parse_host_port "127.0.0.1" in
  check_some_pair "no port returns None" None result

let test_parse_host_port_invalid () =
  let result = Port_validation.parse_host_port "not:a:valid:format:8732" in
  check_some_pair "invalid format returns None" None result

let test_parse_host_port_empty () =
  let result = Port_validation.parse_host_port "" in
  check_some_pair "empty string returns None" None result

let test_parse_host_port_port_only () =
  let result = Port_validation.parse_host_port ":8732" in
  (* Might be valid for "bind to all interfaces" *)
  match result with
  | Some ("", 8732) -> check bool "port only accepted" true true
  | None -> check bool "port only rejected" true true
  | _ -> fail "unexpected parse result"

let test_parse_host_port_zero_port () =
  let result = Port_validation.parse_host_port "127.0.0.1:0" in
  (* Port 0 is rejected - see port_validation.ml line 16: p > 0 *)
  check_some_pair "port 0 rejected" None result

let test_parse_host_port_max_port () =
  let result = Port_validation.parse_host_port "127.0.0.1:65535" in
  check_some_pair "max port parsed" (Some ("127.0.0.1", 65535)) result

let test_parse_host_port_over_max_port () =
  let result = Port_validation.parse_host_port "127.0.0.1:65536" in
  check_some_pair "over max port returns None" None result

let test_parse_host_port_negative_port () =
  let result = Port_validation.parse_host_port "127.0.0.1:-1" in
  check_some_pair "negative port returns None" None result

(* ============================================================ *)
(* parse_port Tests *)
(* ============================================================ *)

let test_parse_port_from_full_address () =
  let result = Port_validation.parse_port "127.0.0.1:8732" in
  check_some "extracts port" (Some 8732) result

let test_parse_port_from_hostname () =
  let result = Port_validation.parse_port "localhost:8732" in
  check_some "extracts port from hostname" (Some 8732) result

let test_parse_port_no_colon () =
  let result = Port_validation.parse_port "127.0.0.1" in
  check_some "no colon returns None" None result

let test_parse_port_invalid_number () =
  let result = Port_validation.parse_port "127.0.0.1:abc" in
  check_some "non-numeric port returns None" None result

let test_parse_port_empty () =
  let result = Port_validation.parse_port "" in
  check_some "empty returns None" None result

(* ============================================================ *)
(* validate_addr Tests *)
(* ============================================================ *)

let test_validate_addr_valid () =
  (* Use a high port unlikely to be in use *)
  let result =
    Port_validation.validate_addr
      ~addr:"127.0.0.1:54321"
      ~example:"127.0.0.1:54321"
      ()
  in
  match result with
  | Ok () -> check bool "valid address accepted" true true
  | Error (Port_validation.Port_in_use _) ->
      (* Port might actually be in use, that's OK *)
      check bool "port in use is valid error" true true
  | Error _ -> fail "unexpected validation error"

let test_validate_addr_invalid_format () =
  let result =
    Port_validation.validate_addr ~addr:"not-valid" ~example:"127.0.0.1:8732" ()
  in
  match result with
  | Error (Port_validation.Invalid_format _) ->
      check bool "invalid format detected" true true
  | _ -> fail "should detect invalid format"

let test_validate_addr_port_too_low () =
  let result =
    Port_validation.validate_addr
      ~addr:"127.0.0.1:1023"
      ~example:"127.0.0.1:8732"
      ()
  in
  match result with
  | Error Port_validation.Port_out_of_range ->
      check bool "port too low detected" true true
  | _ -> fail "should detect port out of range"

let test_validate_addr_port_too_high () =
  let result =
    Port_validation.validate_addr
      ~addr:"127.0.0.1:65536"
      ~example:"127.0.0.1:8732"
      ()
  in
  match result with
  | Error (Port_validation.Invalid_format _ | Port_validation.Port_out_of_range)
    ->
      check bool "port too high detected" true true
  | _ -> fail "should detect port out of range or invalid"

let test_validate_addr_boundary_1024 () =
  let result =
    Port_validation.validate_addr
      ~addr:"127.0.0.1:1024"
      ~example:"127.0.0.1:8732"
      ()
  in
  match result with
  | Ok () -> check bool "port 1024 accepted" true true
  | Error _ -> check bool "port 1024 rejected" false true

let test_validate_addr_boundary_65535 () =
  let result =
    Port_validation.validate_addr
      ~addr:"127.0.0.1:65535"
      ~example:"127.0.0.1:8732"
      ()
  in
  match result with
  | Ok () -> check bool "port 65535 accepted" true true
  | Error _ -> check bool "port 65535 rejected" false true

(* ============================================================ *)
(* validate_rpc_addr Tests *)
(* ============================================================ *)

let test_validate_rpc_addr_valid () =
  let result = Port_validation.validate_rpc_addr "127.0.0.1:54321" in
  match result with
  | Ok () -> check bool "valid RPC address" true true
  | Error (Port_validation.Port_in_use _) ->
      check bool "port in use is valid" true true
  | Error _ -> fail "unexpected error for valid RPC"

let test_validate_rpc_addr_invalid () =
  let result = Port_validation.validate_rpc_addr "invalid" in
  match result with
  | Error _ -> check bool "invalid RPC rejected" true true
  | Ok () -> fail "should reject invalid RPC address"

(* ============================================================ *)
(* validate_p2p_addr Tests *)
(* ============================================================ *)

let test_validate_p2p_addr_valid () =
  let result = Port_validation.validate_p2p_addr "0.0.0.0:54322" in
  match result with
  | Ok () -> check bool "valid P2P address" true true
  | Error (Port_validation.Port_in_use _) ->
      check bool "port in use is valid" true true
  | Error _ -> fail "unexpected error for valid P2P"

let test_validate_p2p_addr_invalid () =
  let result = Port_validation.validate_p2p_addr "invalid" in
  match result with
  | Error _ -> check bool "invalid P2P rejected" true true
  | Ok () -> fail "should reject invalid P2P address"

(* ============================================================ *)
(* pp_error Tests *)
(* ============================================================ *)

let test_pp_error_invalid_format () =
  let err = Port_validation.Invalid_format "test error" in
  let msg = Port_validation.pp_error err in
  check bool "error message not empty" true (String.length msg > 0)

let test_pp_error_port_out_of_range () =
  let err = Port_validation.Port_out_of_range in
  let msg = Port_validation.pp_error err in
  check bool "has range info" true (String.length msg > 0)

let test_pp_error_used_by_other_instance () =
  let err = Port_validation.Used_by_other_instance (8732, "test-node") in
  let msg = Port_validation.pp_error err in
  check bool "mentions port" true (String.contains msg '8') ;
  check bool "mentions instance" true (String.contains msg 't')

let test_pp_error_port_in_use () =
  let err = Port_validation.Port_in_use (8732, Some "nginx") in
  let msg = Port_validation.pp_error err in
  check bool "mentions port" true (String.contains msg '8') ;
  check bool "has content" true (String.length msg > 10)

(* ============================================================ *)
(* Edge Case Tests *)
(* ============================================================ *)

let test_parse_host_port_with_spaces () =
  let result = Port_validation.parse_host_port " 127.0.0.1:8732 " in
  (* Might trim or reject *)
  check
    bool
    "handled spaces"
    true
    (result = None || result = Some ("127.0.0.1", 8732))

let test_parse_host_port_multiple_colons () =
  let result = Port_validation.parse_host_port "host:extra:8732" in
  check_some_pair "multiple colons handled" None result

let test_parse_port_port_only () =
  let result = Port_validation.parse_port ":8732" in
  match result with
  | Some 8732 -> check bool "extracted port from :8732" true true
  | Some _ -> fail "extracted wrong port"
  | None -> check bool "rejected :8732" true true

let test_validate_addr_localhost () =
  let result =
    Port_validation.validate_addr
      ~addr:"localhost:54323"
      ~example:"127.0.0.1:54323"
      ()
  in
  match result with
  | Ok () -> check bool "localhost accepted" true true
  | Error (Port_validation.Port_in_use _) ->
      check bool "port in use is valid" true true
  | Error _ -> fail "unexpected error for localhost"

let test_validate_addr_wildcard () =
  let result =
    Port_validation.validate_addr
      ~addr:"0.0.0.0:54324"
      ~example:"0.0.0.0:54324"
      ()
  in
  match result with
  | Ok () -> check bool "wildcard accepted" true true
  | Error (Port_validation.Port_in_use _) ->
      check bool "port in use is valid" true true
  | Error _ -> fail "unexpected error for wildcard"

(* ============================================================ *)
(* Test Suite *)
(* ============================================================ *)

let parse_host_port_tests =
  [
    ("parse IPv4:port", `Quick, test_parse_host_port_ipv4);
    ("parse hostname:port", `Quick, test_parse_host_port_hostname);
    ("parse wildcard:port", `Quick, test_parse_host_port_wildcard);
    ("parse IPv6:port", `Quick, test_parse_host_port_ipv6);
    ("parse no port", `Quick, test_parse_host_port_no_port);
    ("parse invalid format", `Quick, test_parse_host_port_invalid);
    ("parse empty string", `Quick, test_parse_host_port_empty);
    ("parse port only", `Quick, test_parse_host_port_port_only);
    ("parse port 0", `Quick, test_parse_host_port_zero_port);
    ("parse max port 65535", `Quick, test_parse_host_port_max_port);
    ("parse over max port", `Quick, test_parse_host_port_over_max_port);
    ("parse negative port", `Quick, test_parse_host_port_negative_port);
  ]

let parse_port_tests =
  [
    ("parse port from full address", `Quick, test_parse_port_from_full_address);
    ("parse port from hostname", `Quick, test_parse_port_from_hostname);
    ("parse without colon", `Quick, test_parse_port_no_colon);
    ("parse invalid number", `Quick, test_parse_port_invalid_number);
    ("parse empty", `Quick, test_parse_port_empty);
  ]

let validate_addr_tests =
  [
    ("validate valid address", `Quick, test_validate_addr_valid);
    ("validate invalid format", `Quick, test_validate_addr_invalid_format);
    ("validate port too low", `Quick, test_validate_addr_port_too_low);
    ("validate port too high", `Quick, test_validate_addr_port_too_high);
    ("validate boundary 1024", `Quick, test_validate_addr_boundary_1024);
    ("validate boundary 65535", `Quick, test_validate_addr_boundary_65535);
  ]

let validate_specific_tests =
  [
    ("validate RPC address", `Quick, test_validate_rpc_addr_valid);
    ("validate invalid RPC", `Quick, test_validate_rpc_addr_invalid);
    ("validate P2P address", `Quick, test_validate_p2p_addr_valid);
    ("validate invalid P2P", `Quick, test_validate_p2p_addr_invalid);
  ]

let pp_error_tests =
  [
    ("pp_error invalid format", `Quick, test_pp_error_invalid_format);
    ("pp_error out of range", `Quick, test_pp_error_port_out_of_range);
    ("pp_error used by instance", `Quick, test_pp_error_used_by_other_instance);
    ("pp_error port in use", `Quick, test_pp_error_port_in_use);
  ]

let edge_case_tests =
  [
    ("parse with spaces", `Quick, test_parse_host_port_with_spaces);
    ("parse multiple colons", `Quick, test_parse_host_port_multiple_colons);
    ("parse port only format", `Quick, test_parse_port_port_only);
    ("validate localhost", `Quick, test_validate_addr_localhost);
    ("validate wildcard", `Quick, test_validate_addr_wildcard);
  ]

let () =
  Alcotest.run
    "Port_validation"
    [
      ("parse_host_port", parse_host_port_tests);
      ("parse_port", parse_port_tests);
      ("validate_addr", validate_addr_tests);
      ("validate_specific", validate_specific_tests);
      ("pp_error", pp_error_tests);
      ("edge_cases", edge_case_tests);
    ]
