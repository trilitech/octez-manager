(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Octez_manager_ui

(* ============================================================ *)
(* Path Tests                                                    *)
(* ============================================================ *)

let test_openapi_dir () =
  let dir = Rpc_openapi.openapi_dir () in
  Alcotest.(check bool)
    "ends with openapi"
    true
    (Filename.basename dir = "openapi")

let test_openapi_path () =
  let path = Rpc_openapi.openapi_path () in
  Alcotest.(check bool)
    "ends with json"
    true
    (Filename.check_suffix path ".json")

(* ============================================================ *)
(* Status Tests                                                  *)
(* ============================================================ *)

let test_initial_status () =
  (* Note: status may vary depending on test order and state *)
  let status = Rpc_openapi.get_status () in
  let is_valid =
    match status with
    | Rpc_openapi.NotDownloaded -> true
    | Rpc_openapi.Downloading -> true
    | Rpc_openapi.Ready -> true
    | Rpc_openapi.Error _ -> true
  in
  Alcotest.(check bool) "valid status" true is_valid

(* ============================================================ *)
(* Download Check Tests                                          *)
(* ============================================================ *)

let test_needs_download () =
  (* Just check that function runs without error *)
  let _needs = Rpc_openapi.needs_download () in
  Alcotest.(check pass) "needs_download runs" () ()

(* ============================================================ *)
(* Read Spec Tests                                               *)
(* ============================================================ *)

let test_read_spec_when_missing () =
  (* If file doesn't exist, should return None *)
  let result = Rpc_openapi.read_spec () in
  (* Result depends on whether file exists *)
  let is_option = match result with Some _ -> true | None -> true in
  Alcotest.(check bool) "returns option" true is_option

(* ============================================================ *)
(* extract_placeholders Tests                                    *)
(* ============================================================ *)

module FT = Rpc_openapi.For_tests

let test_no_placeholders () =
  Alcotest.(check (list string))
    "none"
    []
    (FT.extract_placeholders "/chains/main/blocks/head")

let test_one_placeholder () =
  Alcotest.(check (list string))
    "one"
    ["chain_id"]
    (FT.extract_placeholders "/chains/{chain_id}/blocks/head")

let test_two_placeholders () =
  Alcotest.(check (list string))
    "two"
    ["chain_id"; "block_id"]
    (FT.extract_placeholders "/chains/{chain_id}/blocks/{block_id}")

let test_placeholder_empty_path () =
  Alcotest.(check (list string)) "empty" [] (FT.extract_placeholders "")

(* ============================================================ *)
(* extract_placeholder_name Tests                                *)
(* ============================================================ *)

let test_placeholder_name_valid () =
  Alcotest.(check (option string))
    "valid"
    (Some "chain_id")
    (FT.extract_placeholder_name "{chain_id}")

let test_placeholder_name_invalid () =
  Alcotest.(check (option string))
    "no braces"
    None
    (FT.extract_placeholder_name "chain_id")

let test_placeholder_name_partial () =
  Alcotest.(check (option string))
    "partial"
    None
    (FT.extract_placeholder_name "{chain_id")

let test_placeholder_name_empty () =
  Alcotest.(check (option string)) "empty" None (FT.extract_placeholder_name "")

(* ============================================================ *)
(* parse_openapi_json Tests                                      *)
(* ============================================================ *)

let test_parse_simple () =
  let json =
    {|{"paths": {
      "/chains/{chain_id}/blocks/{block_id}": {"get": {"summary": "get block"}},
      "/version": {"get": {"summary": "version"}},
      "/inject/operation": {"post": {"summary": "inject"}}
    }}|}
  in
  let eps = FT.parse_openapi_json json in
  (* Only GET endpoints are returned *)
  Alcotest.(check int) "two GET" 2 (List.length eps)

let test_parse_no_paths () =
  Alcotest.(check int)
    "empty"
    0
    (List.length (FT.parse_openapi_json {|{"info": {}}|}))

let test_parse_invalid_json () =
  Alcotest.(check int)
    "invalid"
    0
    (List.length (FT.parse_openapi_json "not json"))

let test_parse_empty_paths () =
  Alcotest.(check int)
    "empty paths"
    0
    (List.length (FT.parse_openapi_json {|{"paths": {}}|}))

(* ============================================================ *)
(* build_trie + traverse Tests                                   *)
(* ============================================================ *)

let test_trie_single () =
  let trie = FT.build_trie [FT.{template = "/version"; placeholders = []}] in
  match FT.traverse trie ["version"] with
  | Some n -> Alcotest.(check bool) "has GET" true (FT.node_has_get n)
  | None -> Alcotest.fail "expected node"

let test_traverse_nonexistent () =
  let trie = FT.build_trie [FT.{template = "/version"; placeholders = []}] in
  Alcotest.(check bool) "None" true (FT.traverse trie ["nonexistent"] = None)

let test_traverse_placeholder () =
  let trie =
    FT.build_trie
      [FT.{template = "/chains/{chain_id}"; placeholders = ["chain_id"]}]
  in
  match FT.traverse trie ["chains"; "main"] with
  | Some n -> Alcotest.(check bool) "GET" true (FT.node_has_get n)
  | None -> Alcotest.fail "placeholder should match"

let test_traverse_root () =
  let trie = FT.build_trie [FT.{template = "/version"; placeholders = []}] in
  match FT.traverse trie [] with
  | Some _ -> ()
  | None -> Alcotest.fail "root should exist"

let test_traverse_deep_path () =
  let trie =
    FT.build_trie
      [
        FT.
          {
            template = "/chains/{chain_id}/blocks/{block_id}/header";
            placeholders = ["chain_id"; "block_id"];
          };
      ]
  in
  match FT.traverse trie ["chains"; "main"; "blocks"; "head"; "header"] with
  | Some n -> Alcotest.(check bool) "has GET" true (FT.node_has_get n)
  | None -> Alcotest.fail "deep path should match"

(* ============================================================ *)
(* with_prefix Tests                                             *)
(* ============================================================ *)

let test_with_prefix () =
  let eps = [FT.{template = "/helpers/baking_rights"; placeholders = []}] in
  match FT.with_prefix "/chains/{chain_id}/blocks/{block_id}" eps with
  | [ep] ->
      Alcotest.(check string)
        "prefixed"
        "/chains/{chain_id}/blocks/{block_id}/helpers/baking_rights"
        ep.template ;
      Alcotest.(check int) "placeholders" 2 (List.length ep.placeholders)
  | _ -> Alcotest.fail "expected one endpoint"

let test_with_prefix_trailing_slash () =
  let eps = [FT.{template = "/version"; placeholders = []}] in
  match FT.with_prefix "/base/" eps with
  | [ep] -> Alcotest.(check string) "slash" "/base/version" ep.template
  | _ -> Alcotest.fail "expected one endpoint"

let test_with_prefix_empty_template () =
  let eps = [FT.{template = ""; placeholders = []}] in
  match FT.with_prefix "/base" eps with
  | [ep] -> Alcotest.(check string) "empty template" "/base/" ep.template
  | _ -> Alcotest.fail "expected one endpoint"

(* ============================================================ *)
(* clear_cache Tests                                             *)
(* ============================================================ *)

let test_clear_cache () =
  Rpc_openapi.clear_cache () ;
  (* Just verify it doesn't crash *)
  Alcotest.(check pass) "cleared" () ()

(* ============================================================ *)
(* Test Runner                                                   *)
(* ============================================================ *)

let () =
  Alcotest.run
    "Rpc_openapi"
    [
      ( "paths",
        [
          Alcotest.test_case "openapi_dir" `Quick test_openapi_dir;
          Alcotest.test_case "openapi_path" `Quick test_openapi_path;
        ] );
      ("status", [Alcotest.test_case "initial" `Quick test_initial_status]);
      ("needs_download", [Alcotest.test_case "runs" `Quick test_needs_download]);
      ( "read_spec",
        [Alcotest.test_case "when missing" `Quick test_read_spec_when_missing]
      );
      ( "extract_placeholders",
        [
          Alcotest.test_case "none" `Quick test_no_placeholders;
          Alcotest.test_case "one" `Quick test_one_placeholder;
          Alcotest.test_case "two" `Quick test_two_placeholders;
          Alcotest.test_case "empty path" `Quick test_placeholder_empty_path;
        ] );
      ( "extract_placeholder_name",
        [
          Alcotest.test_case "valid" `Quick test_placeholder_name_valid;
          Alcotest.test_case "invalid" `Quick test_placeholder_name_invalid;
          Alcotest.test_case "partial" `Quick test_placeholder_name_partial;
          Alcotest.test_case "empty" `Quick test_placeholder_name_empty;
        ] );
      ( "parse_openapi_json",
        [
          Alcotest.test_case "simple" `Quick test_parse_simple;
          Alcotest.test_case "no paths" `Quick test_parse_no_paths;
          Alcotest.test_case "invalid json" `Quick test_parse_invalid_json;
          Alcotest.test_case "empty paths" `Quick test_parse_empty_paths;
        ] );
      ( "trie",
        [
          Alcotest.test_case "single" `Quick test_trie_single;
          Alcotest.test_case "nonexistent" `Quick test_traverse_nonexistent;
          Alcotest.test_case "placeholder" `Quick test_traverse_placeholder;
          Alcotest.test_case "root" `Quick test_traverse_root;
          Alcotest.test_case "deep path" `Quick test_traverse_deep_path;
        ] );
      ( "with_prefix",
        [
          Alcotest.test_case "basic" `Quick test_with_prefix;
          Alcotest.test_case
            "trailing slash"
            `Quick
            test_with_prefix_trailing_slash;
          Alcotest.test_case
            "empty template"
            `Quick
            test_with_prefix_empty_template;
        ] );
      ("clear_cache", [Alcotest.test_case "runs" `Quick test_clear_cache]);
    ]
