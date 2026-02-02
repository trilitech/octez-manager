(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Octez_manager_ui

(* ============================================================ *)
(* Candidate Paths Tests                                         *)
(* ============================================================ *)

let test_candidate_paths_root () =
  let paths = Rpc_describe.candidate_paths [] in
  Alcotest.(check int) "one path for root" 1 (List.length paths) ;
  Alcotest.(check string)
    "recurse query"
    "/describe?recurse=yes"
    (List.hd paths)

let test_candidate_paths_single_segment () =
  let paths = Rpc_describe.candidate_paths ["chains"] in
  Alcotest.(check int) "two paths" 2 (List.length paths) ;
  Alcotest.(check bool)
    "has prefix form"
    true
    (List.mem "/describe/chains?recurse=yes" paths) ;
  Alcotest.(check bool)
    "has suffix form"
    true
    (List.mem "/chains/describe?recurse=yes" paths)

let test_candidate_paths_nested () =
  let paths = Rpc_describe.candidate_paths ["chains"; "main"; "blocks"] in
  Alcotest.(check int) "two paths" 2 (List.length paths) ;
  Alcotest.(check bool)
    "has prefix form"
    true
    (List.mem "/describe/chains/main/blocks?recurse=yes" paths) ;
  Alcotest.(check bool)
    "has suffix form"
    true
    (List.mem "/chains/main/blocks/describe?recurse=yes" paths)

(* ============================================================ *)
(* JSON Parsing Tests                                            *)
(* ============================================================ *)

let test_parse_get_service () =
  let json =
    Yojson.Safe.from_string
      {|{"static": {"get_service": {"description": "Returns the version"}}}|}
  in
  let entries = Rpc_describe.parse_describe_json json in
  Alcotest.(check int) "one entry" 1 (List.length entries) ;
  let entry = List.hd entries in
  Alcotest.(check string) "name is empty (GET at current path)" "" entry.name ;
  Alcotest.(check bool)
    "kind is Get"
    true
    (match entry.kind with Rpc_describe.Get -> true | _ -> false)

let test_parse_subdirs_suffixes () =
  let json =
    Yojson.Safe.from_string
      {|{"static": {"subdirs": {"suffixes": [{"name": "chains"}, {"name": "version"}]}}}|}
  in
  let entries = Rpc_describe.parse_describe_json json in
  Alcotest.(check int) "two entries" 2 (List.length entries) ;
  let names = List.map (fun e -> e.Rpc_describe.name) entries in
  Alcotest.(check bool) "has chains" true (List.mem "chains" names) ;
  Alcotest.(check bool) "has version" true (List.mem "version" names)

let test_parse_dynamic_dispatch () =
  let json =
    Yojson.Safe.from_string
      {|{"static": {"subdirs": {"dynamic_dispatch": {"arg": {"name": "block_id", "description": "A block identifier"}}}}}|}
  in
  let entries = Rpc_describe.parse_describe_json json in
  Alcotest.(check int) "one entry" 1 (List.length entries) ;
  let entry = List.hd entries in
  Alcotest.(check string) "name has angle brackets" "<block_id>" entry.name ;
  Alcotest.(check bool)
    "kind is Dyn"
    true
    (match entry.kind with Rpc_describe.Dyn "block_id" -> true | _ -> false)

let test_parse_combined () =
  let json =
    Yojson.Safe.from_string
      {|{
        "static": {
          "get_service": {"description": "Get chain info"},
          "subdirs": {
            "suffixes": [{"name": "blocks"}, {"name": "mempool"}],
            "dynamic_dispatch": {"arg": {"name": "chain_id"}}
          }
        }
      }|}
  in
  let entries = Rpc_describe.parse_describe_json json in
  Alcotest.(check int) "four entries" 4 (List.length entries) ;
  let names = List.map (fun e -> e.Rpc_describe.name) entries in
  Alcotest.(check bool) "has GET (empty name)" true (List.mem "" names) ;
  Alcotest.(check bool) "has blocks" true (List.mem "blocks" names) ;
  Alcotest.(check bool) "has mempool" true (List.mem "mempool" names) ;
  Alcotest.(check bool) "has dynamic" true (List.mem "<chain_id>" names)

let test_parse_empty_json () =
  let json = Yojson.Safe.from_string {|{}|} in
  let entries = Rpc_describe.parse_describe_json json in
  Alcotest.(check int) "no entries" 0 (List.length entries)

let test_parse_invalid_structure () =
  let json = Yojson.Safe.from_string {|{"foo": "bar"}|} in
  let entries = Rpc_describe.parse_describe_json json in
  Alcotest.(check int) "no entries" 0 (List.length entries)

(* ============================================================ *)
(* Description Parsing Tests                                     *)
(* ============================================================ *)

let test_parse_description_present () =
  let json =
    Yojson.Safe.from_string
      {|{"static": {"get_service": {"description": "Returns the current head"}}}|}
  in
  let desc = Rpc_describe.parse_description json in
  Alcotest.(check (option string))
    "has description"
    (Some "Returns the current head")
    desc

let test_parse_description_missing () =
  let json = Yojson.Safe.from_string {|{"static": {"subdirs": {}}}|} in
  let desc = Rpc_describe.parse_description json in
  Alcotest.(check (option string)) "no description" None desc

(* ============================================================ *)
(* Cache Tests                                                   *)
(* ============================================================ *)

let test_cache_clear () =
  Rpc_describe.clear_cache () ;
  (* Just verify it doesn't crash *)
  Alcotest.(check bool) "clear doesn't crash" true true

(* ============================================================ *)
(* Test Runner                                                   *)
(* ============================================================ *)

let () =
  Alcotest.run
    "Rpc_describe"
    [
      ( "candidate_paths",
        [
          Alcotest.test_case "root path" `Quick test_candidate_paths_root;
          Alcotest.test_case
            "single segment"
            `Quick
            test_candidate_paths_single_segment;
          Alcotest.test_case "nested path" `Quick test_candidate_paths_nested;
        ] );
      ( "parse_json",
        [
          Alcotest.test_case "get_service" `Quick test_parse_get_service;
          Alcotest.test_case
            "subdirs suffixes"
            `Quick
            test_parse_subdirs_suffixes;
          Alcotest.test_case
            "dynamic_dispatch"
            `Quick
            test_parse_dynamic_dispatch;
          Alcotest.test_case "combined" `Quick test_parse_combined;
          Alcotest.test_case "empty json" `Quick test_parse_empty_json;
          Alcotest.test_case
            "invalid structure"
            `Quick
            test_parse_invalid_structure;
        ] );
      ( "parse_description",
        [
          Alcotest.test_case
            "description present"
            `Quick
            test_parse_description_present;
          Alcotest.test_case
            "description missing"
            `Quick
            test_parse_description_missing;
        ] );
      ("cache", [Alcotest.test_case "clear" `Quick test_cache_clear]);
    ]
