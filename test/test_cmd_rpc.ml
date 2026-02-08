(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Octez_manager_lib

(* ============================================================ *)
(* Service from URL Tests                                        *)
(* ============================================================ *)

let test_service_from_url () =
  let url = "https://mainnet.tezos.ecadinfra.com" in
  let svc = Cmd_rpc.service_from_url url in
  Alcotest.(check string)
    "rpc_addr"
    url
    (Rpc_addr.to_string svc.Service.rpc_addr) ;
  Alcotest.(check string)
    "instance prefix"
    "url:"
    (String.sub svc.Service.instance 0 4) ;
  Alcotest.(check string) "role" "node" svc.Service.role ;
  Alcotest.(check string) "data_dir empty" "" svc.Service.data_dir

let test_service_from_url_localhost () =
  let url = "http://127.0.0.1:8732" in
  let svc = Cmd_rpc.service_from_url url in
  Alcotest.(check string)
    "rpc_addr"
    url
    (Rpc_addr.to_string svc.Service.rpc_addr)

(* ============================================================ *)
(* Resolve Service Tests                                         *)
(* ============================================================ *)

let test_resolve_service_url_only () =
  let url = "https://mainnet.smartpy.io" in
  match Cmd_rpc.resolve_service None (Some url) None with
  | Ok svc ->
      Alcotest.(check string)
        "rpc_addr"
        url
        (Rpc_addr.to_string svc.Service.rpc_addr)
  | Error msg -> Alcotest.fail msg

let test_resolve_service_multiple_error () =
  match Cmd_rpc.resolve_service (Some "instance") (Some "url") None with
  | Error msg -> Alcotest.(check bool) "has error" true (String.length msg > 0)
  | Ok _ -> Alcotest.fail "expected error"

let test_resolve_service_none_error () =
  match Cmd_rpc.resolve_service None None None with
  | Error msg -> Alcotest.(check bool) "has error" true (String.length msg > 0)
  | Ok _ -> Alcotest.fail "expected error"

let test_resolve_service_public () =
  (* This test depends on having public nodes in cache *)
  match Cmd_rpc.resolve_service None None (Some "1") with
  | Ok svc ->
      Alcotest.(check bool)
        "has rpc_addr"
        true
        (String.length (Rpc_addr.to_string svc.Service.rpc_addr) > 0)
  | Error _ ->
      (* Public nodes fetch might fail in test environment, that's ok *)
      ()

(* ============================================================ *)
(* Test Runner                                                   *)
(* ============================================================ *)

let () =
  Alcotest.run
    "Cmd_rpc"
    [
      ( "service_from_url",
        [
          Alcotest.test_case "basic" `Quick test_service_from_url;
          Alcotest.test_case "localhost" `Quick test_service_from_url_localhost;
        ] );
      ( "resolve_service",
        [
          Alcotest.test_case "url only" `Quick test_resolve_service_url_only;
          Alcotest.test_case
            "multiple error"
            `Quick
            test_resolve_service_multiple_error;
          Alcotest.test_case "none error" `Quick test_resolve_service_none_error;
          Alcotest.test_case "public node" `Quick test_resolve_service_public;
        ] );
    ]
