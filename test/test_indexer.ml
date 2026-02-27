(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(* lib/common has (wrapped false), so Indexer is accessed directly. *)

(* ── Test helpers ─────────────────────────────────────────────────────── *)

(** Monotonically increasing counter for unique network names, avoiding
    inter-test state pollution without requiring cleanup coordination. *)
let test_counter = Atomic.make 0

let fresh_network () =
  Printf.sprintf "test-%d" (Atomic.fetch_and_add test_counter 1)

(** [starts_with ~prefix s] is [true] if [s] begins with [prefix]. *)
let starts_with ~prefix s =
  let plen = String.length prefix in
  String.length s >= plen && String.equal (String.sub s 0 plen) prefix

(** Create a mock HTTP function from a list of [(url_prefix, response)] pairs.
    The first matching prefix wins; unmatched URLs return [Error].
    Optionally records every called URL into [~record]. *)
let make_mock ?record responses =
 fun ~url ~timeout:_ ->
  (match record with Some r -> r := url :: !r | None -> ()) ;
  match
    List.find_opt (fun (prefix, _) -> starts_with ~prefix url) responses
  with
  | Some (_, `Ok body) -> Ok body
  | Some (_, `Error msg) -> Error (`Msg msg)
  | None -> Error (`Msg (Printf.sprintf "mock: no handler for url %s" url))

(** Run [f] with a custom HTTP mock, cleaning up state on exit. *)
let with_mock ~network ~mock f =
  Indexer.Internal_for_tests.set_http_fn mock ;
  Fun.protect
    ~finally:(fun () ->
      Indexer.Internal_for_tests.reset_http_fn () ;
      Indexer.Internal_for_tests.reset_log_fn () ;
      Indexer.clear_local ~network ;
      Indexer.set_debug_mode false)
    f

(* ── Tests ────────────────────────────────────────────────────────────── *)

let test_tzkt_base_url_mainnet () =
  Alcotest.(check string)
    "mainnet"
    "https://api.tzkt.io"
    (Indexer.tzkt_base_url ~network:"mainnet")

let test_tzkt_base_url_testnet () =
  Alcotest.(check string)
    "ghostnet"
    "https://api.ghostnet.tzkt.io"
    (Indexer.tzkt_base_url ~network:"ghostnet")

let test_register_clear_roundtrip () =
  let network = fresh_network () in
  Indexer.register_local ~network ~base_url:"http://local1" ;
  Alcotest.(check (list string))
    "one endpoint registered"
    ["http://local1"]
    (Indexer.Internal_for_tests.get_local_endpoints ~network) ;
  Indexer.clear_local ~network ;
  Alcotest.(check (list string))
    "endpoints cleared"
    []
    (Indexer.Internal_for_tests.get_local_endpoints ~network)

let test_preferred_base_wins () =
  let network = fresh_network () in
  let called = ref [] in
  Indexer.register_local ~network ~base_url:"http://mylocal" ;
  let mock =
    make_mock
      ~record:called
      [
        ("http://mypref", `Ok "pref-body");
        ("http://mylocal", `Error "should not be called");
      ]
  in
  with_mock ~network ~mock (fun () ->
      let result =
        Indexer.fetch ~network ~preferred_base:"http://mypref" "/p"
      in
      Alcotest.(check (result string string))
        "preferred wins"
        (Ok "pref-body")
        (match result with Ok b -> Ok b | Error (`Msg m) -> Error m) ;
      let local_calls =
        List.filter (fun u -> starts_with ~prefix:"http://mylocal" u) !called
      in
      Alcotest.(check bool) "local not called" true (local_calls = []))

let test_local_before_tzkt () =
  let network = fresh_network () in
  let called = ref [] in
  Indexer.register_local ~network ~base_url:"http://mylocal" ;
  let mock =
    make_mock
      ~record:called
      [
        ("http://mylocal", `Ok "local-body");
        ("https://api.", `Error "tzkt should not be called");
      ]
  in
  with_mock ~network ~mock (fun () ->
      let result = Indexer.fetch ~network "/path" in
      Alcotest.(check (result string string))
        "local wins"
        (Ok "local-body")
        (match result with Ok b -> Ok b | Error (`Msg m) -> Error m) ;
      let tzkt_calls =
        List.filter (fun u -> starts_with ~prefix:"https://api." u) !called
      in
      Alcotest.(check bool) "tzkt not called" true (tzkt_calls = []))

let test_fallback_on_local_error () =
  let network = fresh_network () in
  Indexer.register_local ~network ~base_url:"http://badlocal" ;
  let mock =
    make_mock
      [
        ("http://badlocal", `Error "local error");
        ("https://api.", `Ok "tzkt-body");
      ]
  in
  with_mock ~network ~mock (fun () ->
      let result = Indexer.fetch ~network "/path" in
      Alcotest.(check (result string string))
        "fallback to tzkt"
        (Ok "tzkt-body")
        (match result with Ok b -> Ok b | Error (`Msg m) -> Error m))

let test_fallback_on_empty_body () =
  let network = fresh_network () in
  Indexer.register_local ~network ~base_url:"http://emptylocal" ;
  let mock =
    make_mock
      [
        ("http://emptylocal", `Ok "");
        (* empty → treated as failure *)
        ("https://api.", `Ok "tzkt-body");
      ]
  in
  with_mock ~network ~mock (fun () ->
      let result = Indexer.fetch ~network "/path" in
      Alcotest.(check (result string string))
        "fallback after empty body"
        (Ok "tzkt-body")
        (match result with Ok b -> Ok b | Error (`Msg m) -> Error m))

let test_all_fail () =
  let network = fresh_network () in
  let mock = make_mock [("", `Error "everything broken")] in
  with_mock ~network ~mock (fun () ->
      let result = Indexer.fetch ~network "/path" in
      Alcotest.(check bool)
        "result is error"
        true
        (match result with Error _ -> true | Ok _ -> false))

let test_round_robin_two_locals () =
  let network = fresh_network () in
  Indexer.register_local ~network ~base_url:"http://nodeA" ;
  Indexer.register_local ~network ~base_url:"http://nodeB" ;
  let called = ref [] in
  let mock = make_mock ~record:called [("", `Ok "body")] in
  with_mock ~network ~mock (fun () ->
      for _ = 1 to 4 do
        ignore (Indexer.fetch ~network "/path")
      done ;
      let a_calls =
        List.length
          (List.filter (fun u -> starts_with ~prefix:"http://nodeA" u) !called)
      in
      let b_calls =
        List.length
          (List.filter (fun u -> starts_with ~prefix:"http://nodeB" u) !called)
      in
      Alcotest.(check bool) "nodeA called at least once" true (a_calls >= 1) ;
      Alcotest.(check bool) "nodeB called at least once" true (b_calls >= 1))

let test_dedup_preferred_equals_local () =
  let network = fresh_network () in
  let shared = "http://shared" in
  Indexer.register_local ~network ~base_url:shared ;
  let call_count = ref 0 in
  let mock =
    make_mock
      ~record:(ref []) (* unused, use call_count directly *)
      [("", `Ok "body")]
  in
  let counting_mock ~url ~timeout =
    incr call_count ;
    mock ~url ~timeout
  in
  with_mock ~network ~mock:counting_mock (fun () ->
      ignore (Indexer.fetch ~network ~preferred_base:shared "/path") ;
      Alcotest.(check int) "URL queried exactly once" 1 !call_count)

let test_debug_logs_divergence () =
  let network = fresh_network () in
  Indexer.set_debug_mode true ;
  Indexer.register_local ~network ~base_url:"http://divlocal" ;
  let logs = ref [] in
  Indexer.Internal_for_tests.set_log_fn (fun msg -> logs := msg :: !logs) ;
  let mock =
    make_mock
      [
        ("http://divlocal", `Ok "body-A");
        ("https://api.", `Ok "body-B");
        (* different from local *)
      ]
  in
  with_mock ~network ~mock (fun () ->
      ignore (Indexer.fetch ~network "/path") ;
      Alcotest.(check bool) "divergence was logged" true (List.length !logs >= 1))

let test_debug_silent_on_match () =
  let network = fresh_network () in
  Indexer.set_debug_mode true ;
  Indexer.register_local ~network ~base_url:"http://samelocal" ;
  let logs = ref [] in
  Indexer.Internal_for_tests.set_log_fn (fun msg -> logs := msg :: !logs) ;
  let mock = make_mock [("", `Ok "same-body")] in
  with_mock ~network ~mock (fun () ->
      ignore (Indexer.fetch ~network "/path") ;
      Alcotest.(check bool) "no divergence logged" true (List.length !logs = 0))

let test_query_all_returns_all_sources () =
  let network = fresh_network () in
  Indexer.register_local ~network ~base_url:"http://qa-local1" ;
  Indexer.register_local ~network ~base_url:"http://qa-local2" ;
  let mock = make_mock [("", `Ok "body")] in
  with_mock ~network ~mock (fun () ->
      let results = Indexer.query_all ~network "/path" in
      (* 2 locals + 1 TzKT = 3 distinct sources *)
      Alcotest.(check int) "3 sources returned" 3 (List.length results))

let () =
  Alcotest.run
    "indexer"
    [
      ( "tzkt_base_url",
        [
          Alcotest.test_case "mainnet" `Quick test_tzkt_base_url_mainnet;
          Alcotest.test_case "testnet" `Quick test_tzkt_base_url_testnet;
        ] );
      ( "register",
        [
          Alcotest.test_case
            "register_clear_roundtrip"
            `Quick
            test_register_clear_roundtrip;
        ] );
      ( "fetch_routing",
        [
          Alcotest.test_case
            "preferred_base_wins"
            `Quick
            test_preferred_base_wins;
          Alcotest.test_case "local_before_tzkt" `Quick test_local_before_tzkt;
          Alcotest.test_case
            "fallback_on_local_error"
            `Quick
            test_fallback_on_local_error;
          Alcotest.test_case
            "fallback_on_empty_body"
            `Quick
            test_fallback_on_empty_body;
          Alcotest.test_case "all_fail" `Quick test_all_fail;
          Alcotest.test_case
            "round_robin_two_locals"
            `Quick
            test_round_robin_two_locals;
          Alcotest.test_case
            "dedup_preferred_equals_local"
            `Quick
            test_dedup_preferred_equals_local;
        ] );
      ( "debug_mode",
        [
          Alcotest.test_case
            "debug_logs_divergence"
            `Quick
            test_debug_logs_divergence;
          Alcotest.test_case
            "debug_silent_on_match"
            `Quick
            test_debug_silent_on_match;
        ] );
      ( "query_all",
        [
          Alcotest.test_case
            "returns_all_sources"
            `Quick
            test_query_all_returns_all_sources;
        ] );
    ]
