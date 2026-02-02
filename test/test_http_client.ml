(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

open Octez_manager_lib

let test_build_url_raw_address () =
  let url = Http_client.build_url ~rpc_addr:"127.0.0.1:8732" ~path:"/version" in
  Alcotest.(check string) "raw address" "http://127.0.0.1:8732/version" url

let test_build_url_https_address () =
  let url =
    Http_client.build_url ~rpc_addr:"https://rpc.example.com"
      ~path:"/chains/main/blocks/head"
  in
  Alcotest.(check string)
    "https address" "https://rpc.example.com/chains/main/blocks/head" url

let test_build_url_http_address () =
  let url =
    Http_client.build_url ~rpc_addr:"http://localhost:8732" ~path:"/describe"
  in
  Alcotest.(check string)
    "http address" "http://localhost:8732/describe" url

let test_build_url_path_without_leading_slash () =
  let url =
    Http_client.build_url ~rpc_addr:"127.0.0.1:8732" ~path:"version"
  in
  Alcotest.(check string)
    "path without slash" "http://127.0.0.1:8732/version" url

let test_build_url_empty_path () =
  let url = Http_client.build_url ~rpc_addr:"127.0.0.1:8732" ~path:"" in
  Alcotest.(check string) "empty path" "http://127.0.0.1:8732/" url

let test_cache_miss () =
  Http_client.clear_cache () ;
  let result =
    Http_client.get_cached ~rpc_addr:"127.0.0.1:8732" ~path:"/test" ~ttl:1.0
  in
  Alcotest.(check (option string)) "cache miss" None result

let test_cache_hit () =
  Http_client.clear_cache () ;
  Http_client.cache_put ~rpc_addr:"127.0.0.1:8732" ~path:"/test"
    ~body:"cached result" ;
  let result =
    Http_client.get_cached ~rpc_addr:"127.0.0.1:8732" ~path:"/test" ~ttl:10.0
  in
  Alcotest.(check (option string)) "cache hit" (Some "cached result") result

let test_cache_different_paths () =
  Http_client.clear_cache () ;
  Http_client.cache_put ~rpc_addr:"127.0.0.1:8732" ~path:"/path1" ~body:"body1" ;
  Http_client.cache_put ~rpc_addr:"127.0.0.1:8732" ~path:"/path2" ~body:"body2" ;
  let result1 =
    Http_client.get_cached ~rpc_addr:"127.0.0.1:8732" ~path:"/path1" ~ttl:10.0
  in
  let result2 =
    Http_client.get_cached ~rpc_addr:"127.0.0.1:8732" ~path:"/path2" ~ttl:10.0
  in
  Alcotest.(check (option string)) "path1" (Some "body1") result1 ;
  Alcotest.(check (option string)) "path2" (Some "body2") result2

let test_cache_different_addresses () =
  Http_client.clear_cache () ;
  Http_client.cache_put ~rpc_addr:"127.0.0.1:8732" ~path:"/test" ~body:"local" ;
  Http_client.cache_put ~rpc_addr:"rpc.example.com" ~path:"/test"
    ~body:"remote" ;
  let result1 =
    Http_client.get_cached ~rpc_addr:"127.0.0.1:8732" ~path:"/test" ~ttl:10.0
  in
  let result2 =
    Http_client.get_cached ~rpc_addr:"rpc.example.com" ~path:"/test" ~ttl:10.0
  in
  Alcotest.(check (option string)) "local" (Some "local") result1 ;
  Alcotest.(check (option string)) "remote" (Some "remote") result2

let test_cache_expiry () =
  Http_client.clear_cache () ;
  Http_client.cache_put ~rpc_addr:"127.0.0.1:8732" ~path:"/test"
    ~body:"cached result" ;
  (* Immediately check with very short TTL that has already passed *)
  Unix.sleepf 0.01 ;
  let result =
    Http_client.get_cached ~rpc_addr:"127.0.0.1:8732" ~path:"/test" ~ttl:0.001
  in
  Alcotest.(check (option string)) "cache expired" None result

let test_cache_clear () =
  Http_client.clear_cache () ;
  Http_client.cache_put ~rpc_addr:"127.0.0.1:8732" ~path:"/test"
    ~body:"cached result" ;
  Http_client.clear_cache () ;
  let result =
    Http_client.get_cached ~rpc_addr:"127.0.0.1:8732" ~path:"/test" ~ttl:10.0
  in
  Alcotest.(check (option string)) "cache cleared" None result

let test_tool_detection_cached () =
  (* Tool detection should be cached - calling twice should return same result *)
  let has_curl1 = Http_client.has_curl () in
  let has_curl2 = Http_client.has_curl () in
  Alcotest.(check bool) "curl cached" has_curl1 has_curl2 ;
  let has_wget1 = Http_client.has_wget () in
  let has_wget2 = Http_client.has_wget () in
  Alcotest.(check bool) "wget cached" has_wget1 has_wget2

let test_at_least_one_tool_available () =
  (* At least one HTTP client should be available on most systems *)
  let has_curl = Http_client.has_curl () in
  let has_wget = Http_client.has_wget () in
  Alcotest.(check bool) "at least one tool" true (has_curl || has_wget)

let () =
  Alcotest.run "Http_client"
    [
      ( "build_url",
        [
          Alcotest.test_case "raw address" `Quick test_build_url_raw_address;
          Alcotest.test_case "https address" `Quick test_build_url_https_address;
          Alcotest.test_case "http address" `Quick test_build_url_http_address;
          Alcotest.test_case "path without leading slash" `Quick
            test_build_url_path_without_leading_slash;
          Alcotest.test_case "empty path" `Quick test_build_url_empty_path;
        ] );
      ( "cache",
        [
          Alcotest.test_case "cache miss" `Quick test_cache_miss;
          Alcotest.test_case "cache hit" `Quick test_cache_hit;
          Alcotest.test_case "different paths" `Quick test_cache_different_paths;
          Alcotest.test_case "different addresses" `Quick
            test_cache_different_addresses;
          Alcotest.test_case "cache expiry" `Quick test_cache_expiry;
          Alcotest.test_case "cache clear" `Quick test_cache_clear;
        ] );
      ( "tool_detection",
        [
          Alcotest.test_case "cached" `Quick test_tool_detection_cached;
          Alcotest.test_case "at least one available" `Quick
            test_at_least_one_tool_available;
        ] );
    ]
