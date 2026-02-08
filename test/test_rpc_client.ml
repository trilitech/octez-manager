(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Octez_manager_lib
open Octez_manager_ui

(* Helper to create a minimal Service.t for testing *)
let make_test_service ?(rpc_addr = "127.0.0.1:8732") ?(app_bin_dir = "/usr/bin")
    () =
  Service.make
    ~instance:"test-node"
    ~role:"node"
    ~network:"mainnet"
    ~history_mode:History_mode.Full
    ~data_dir:"/tmp/test"
    ~rpc_addr:(Rpc_addr.of_string rpc_addr)
    ~net_addr:"[::]:9732"
    ~service_user:"tezos"
    ~app_bin_dir
    ~logging_mode:Logging_mode.Journald
    ()

(* ============================================================ *)
(* URL Building Tests                                            *)
(* ============================================================ *)

let test_endpoint_of_raw_address () =
  let s = make_test_service ~rpc_addr:"127.0.0.1:8732" () in
  let endpoint = Rpc_client.endpoint_of s in
  Alcotest.(check string) "raw address" "http://127.0.0.1:8732" endpoint

let test_endpoint_of_http_address () =
  let s = make_test_service ~rpc_addr:"http://localhost:8732" () in
  let endpoint = Rpc_client.endpoint_of s in
  Alcotest.(check string) "http address" "http://localhost:8732" endpoint

let test_endpoint_of_https_address () =
  let s = make_test_service ~rpc_addr:"https://rpc.example.com" () in
  let endpoint = Rpc_client.endpoint_of s in
  Alcotest.(check string) "https address" "https://rpc.example.com" endpoint

let test_absolutize_url_with_leading_slash () =
  let s = make_test_service ~rpc_addr:"127.0.0.1:8732" () in
  let url = Rpc_client.absolutize_url s "/chains/main/blocks/head" in
  Alcotest.(check string)
    "with leading slash"
    "http://127.0.0.1:8732/chains/main/blocks/head"
    url

let test_absolutize_url_without_leading_slash () =
  let s = make_test_service ~rpc_addr:"127.0.0.1:8732" () in
  let url = Rpc_client.absolutize_url s "version" in
  Alcotest.(check string)
    "without leading slash"
    "http://127.0.0.1:8732/version"
    url

let test_absolutize_url_empty_path () =
  let s = make_test_service ~rpc_addr:"127.0.0.1:8732" () in
  let url = Rpc_client.absolutize_url s "" in
  Alcotest.(check string) "empty path" "http://127.0.0.1:8732/" url

(* ============================================================ *)
(* Tool Detection Tests                                          *)
(* ============================================================ *)

let test_curl_available_cached () =
  (* Result should be consistent (cached) *)
  let has_curl1 = Rpc_client.curl_available () in
  let has_curl2 = Rpc_client.curl_available () in
  Alcotest.(check bool) "curl cached" has_curl1 has_curl2

let test_wget_available_cached () =
  (* Result should be consistent (cached) *)
  let has_wget1 = Rpc_client.wget_available () in
  let has_wget2 = Rpc_client.wget_available () in
  Alcotest.(check bool) "wget cached" has_wget1 has_wget2

let test_at_least_one_http_tool () =
  (* At least one HTTP client should be available on most systems *)
  let has_curl = Rpc_client.curl_available () in
  let has_wget = Rpc_client.wget_available () in
  Alcotest.(check bool) "at least one tool" true (has_curl || has_wget)

(* ============================================================ *)
(* Monitor Handle Tests                                          *)
(* ============================================================ *)

let test_monitor_handle_type () =
  (* Just verify the type exists and has expected fields *)
  let handle : Rpc_client.monitor_handle =
    {stop = (fun () -> ()); alive = (fun () -> true)}
  in
  Alcotest.(check bool) "alive returns true" true (handle.alive ()) ;
  handle.stop () ;
  Alcotest.(check bool) "stop doesn't crash" true true

(* ============================================================ *)
(* Error Tracking Tests                                          *)
(* ============================================================ *)

let test_rpc_last_error_initially_none () =
  let s = make_test_service () in
  Rpc_client.clear_error s ;
  let err = Rpc_client.rpc_last_error s in
  Alcotest.(check (option string)) "no error initially" None err

(* ============================================================ *)
(* try_fetch_methods Tests                                       *)
(* ============================================================ *)

let test_try_fetch_no_methods () =
  let result = Rpc_client.For_tests.try_fetch_methods None [] in
  Alcotest.(check (result string string))
    "no methods"
    (Error "no HTTP methods available")
    result

let test_try_fetch_first_succeeds () =
  let m1 () = Some (Ok "response") in
  let m2 () = failwith "should not be called" in
  Alcotest.(check (result string string))
    "first wins"
    (Ok "response")
    (Rpc_client.For_tests.try_fetch_methods None [m1; m2])

let test_try_fetch_fallback () =
  let m1 () = None in
  let m2 () = Some (Ok "fallback") in
  Alcotest.(check (result string string))
    "fallback"
    (Ok "fallback")
    (Rpc_client.For_tests.try_fetch_methods None [m1; m2])

let test_try_fetch_error_then_success () =
  let m1 () = Some (Error "fail1") in
  let m2 () = Some (Ok "recovered") in
  Alcotest.(check (result string string))
    "recovered"
    (Ok "recovered")
    (Rpc_client.For_tests.try_fetch_methods None [m1; m2])

let test_try_fetch_all_error () =
  let m1 () = Some (Error "err1") in
  let m2 () = Some (Error "err2") in
  Alcotest.(check (result string string))
    "last error"
    (Error "err2")
    (Rpc_client.For_tests.try_fetch_methods None [m1; m2])

let test_try_fetch_none_then_error () =
  let m1 () = None in
  let m2 () = Some (Error "only-error") in
  Alcotest.(check (result string string))
    "propagated"
    (Error "only-error")
    (Rpc_client.For_tests.try_fetch_methods None [m1; m2])

(* ============================================================ *)
(* octez_client_bin Tests                                        *)
(* ============================================================ *)

let test_octez_client_bin () =
  let s = make_test_service ~app_bin_dir:"/opt/octez" () in
  Alcotest.(check string)
    "bin"
    "/opt/octez/octez-client"
    (Rpc_client.For_tests.octez_client_bin s)

let test_octez_client_bin_default () =
  let s = make_test_service () in
  Alcotest.(check string)
    "default bin"
    "/usr/bin/octez-client"
    (Rpc_client.For_tests.octez_client_bin s)

(* ============================================================ *)
(* with_request_slot Tests                                       *)
(* ============================================================ *)

let test_with_request_slot_basic () =
  Alcotest.(check int)
    "basic"
    42
    (Rpc_client.For_tests.with_request_slot (fun () -> 42))

let test_with_request_slot_exception () =
  (* with_request_slot propagates exceptions from the callback *)
  Alcotest.(check bool)
    "exception propagated"
    true
    (try
       ignore
         (Rpc_client.For_tests.with_request_slot (fun () -> failwith "boom")) ;
       false
     with _ -> true)

(* ============================================================ *)
(* absolutize_url edge cases                                     *)
(* ============================================================ *)

let test_absolutize_url_https () =
  let s = make_test_service ~rpc_addr:"https://mainnet.api.tez.ie" () in
  Alcotest.(check string)
    "https"
    "https://mainnet.api.tez.ie/chains/main/blocks/head"
    (Rpc_client.absolutize_url s "/chains/main/blocks/head")

(* ============================================================ *)
(* Test Runner                                                   *)
(* ============================================================ *)

let () =
  Alcotest.run
    "Rpc_client"
    [
      ( "url_building",
        [
          Alcotest.test_case
            "endpoint_of raw address"
            `Quick
            test_endpoint_of_raw_address;
          Alcotest.test_case
            "endpoint_of http address"
            `Quick
            test_endpoint_of_http_address;
          Alcotest.test_case
            "endpoint_of https address"
            `Quick
            test_endpoint_of_https_address;
          Alcotest.test_case
            "absolutize_url with leading slash"
            `Quick
            test_absolutize_url_with_leading_slash;
          Alcotest.test_case
            "absolutize_url without leading slash"
            `Quick
            test_absolutize_url_without_leading_slash;
          Alcotest.test_case
            "absolutize_url empty path"
            `Quick
            test_absolutize_url_empty_path;
          Alcotest.test_case
            "absolutize_url https"
            `Quick
            test_absolutize_url_https;
        ] );
      ( "tool_detection",
        [
          Alcotest.test_case "curl cached" `Quick test_curl_available_cached;
          Alcotest.test_case "wget cached" `Quick test_wget_available_cached;
          Alcotest.test_case
            "at least one tool"
            `Quick
            test_at_least_one_http_tool;
        ] );
      ( "monitor_handle",
        [Alcotest.test_case "type exists" `Quick test_monitor_handle_type] );
      ( "error_tracking",
        [
          Alcotest.test_case
            "initially none"
            `Quick
            test_rpc_last_error_initially_none;
        ] );
      ( "try_fetch_methods",
        [
          Alcotest.test_case "no methods" `Quick test_try_fetch_no_methods;
          Alcotest.test_case
            "first succeeds"
            `Quick
            test_try_fetch_first_succeeds;
          Alcotest.test_case "fallback" `Quick test_try_fetch_fallback;
          Alcotest.test_case
            "error then success"
            `Quick
            test_try_fetch_error_then_success;
          Alcotest.test_case "all error" `Quick test_try_fetch_all_error;
          Alcotest.test_case
            "none then error"
            `Quick
            test_try_fetch_none_then_error;
        ] );
      ( "octez_client_bin",
        [
          Alcotest.test_case "custom path" `Quick test_octez_client_bin;
          Alcotest.test_case "default path" `Quick test_octez_client_bin_default;
        ] );
      ( "with_request_slot",
        [
          Alcotest.test_case "basic" `Quick test_with_request_slot_basic;
          Alcotest.test_case "exception" `Quick test_with_request_slot_exception;
        ] );
    ]
