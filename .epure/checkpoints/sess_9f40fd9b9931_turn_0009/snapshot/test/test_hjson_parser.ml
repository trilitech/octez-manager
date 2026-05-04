(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Octez_manager_rewards

let parse_ok input =
  match Hjson_parser.parse input with
  | Ok json -> json
  | Error msg -> Alcotest.fail msg

(* ── Comment tests ───────────────────────────────────────── *)

let test_line_comments () =
  let input = {|{
    // This is a comment
    "key": "value"
  }|} in
  let json = parse_ok input in
  let v = Yojson.Safe.Util.(member "key" json |> to_string) in
  Alcotest.(check string) "line comment stripped" "value" v

let test_block_comments () =
  let input = {|{
    /* block comment */
    "key": "value"
  }|} in
  let json = parse_ok input in
  let v = Yojson.Safe.Util.(member "key" json |> to_string) in
  Alcotest.(check string) "block comment stripped" "value" v

let test_block_comment_multiline () =
  let input =
    {|{
    /*
     * multi-line
     * block comment
     */
    "key": 42
  }|}
  in
  let json = parse_ok input in
  let v = Yojson.Safe.Util.(member "key" json |> to_int) in
  Alcotest.(check int) "multiline block comment" 42 v

let test_hash_comments () =
  let input = {|{
    # hash comment
    "key": true
  }|} in
  let json = parse_ok input in
  let v = Yojson.Safe.Util.(member "key" json |> to_bool) in
  Alcotest.(check bool) "hash comment stripped" true v

let test_comment_in_string_preserved () =
  let input = {|{"key": "value // not a comment"}|} in
  let json = parse_ok input in
  let v = Yojson.Safe.Util.(member "key" json |> to_string) in
  Alcotest.(check string)
    "comment in string preserved"
    "value // not a comment"
    v

(* ── Unquoted key tests ──────────────────────────────────── *)

let test_unquoted_keys () =
  let input = {|{
    name: "Alice",
    age: 30
  }|} in
  let json = parse_ok input in
  let name = Yojson.Safe.Util.(member "name" json |> to_string) in
  let age = Yojson.Safe.Util.(member "age" json |> to_int) in
  Alcotest.(check string) "unquoted key name" "Alice" name ;
  Alcotest.(check int) "unquoted key age" 30 age

let test_mixed_quoted_unquoted () =
  let input = {|{
    "quoted": true,
    unquoted: false
  }|} in
  let json = parse_ok input in
  let q = Yojson.Safe.Util.(member "quoted" json |> to_bool) in
  let u = Yojson.Safe.Util.(member "unquoted" json |> to_bool) in
  Alcotest.(check bool) "quoted key" true q ;
  Alcotest.(check bool) "unquoted key" false u

(* ── Trailing comma tests ────────────────────────────────── *)

let test_trailing_comma_object () =
  let input = {|{
    "a": 1,
    "b": 2,
  }|} in
  let json = parse_ok input in
  let a = Yojson.Safe.Util.(member "a" json |> to_int) in
  let b = Yojson.Safe.Util.(member "b" json |> to_int) in
  Alcotest.(check int) "trailing comma obj a" 1 a ;
  Alcotest.(check int) "trailing comma obj b" 2 b

let test_trailing_comma_array () =
  let input = {|{"items": [1, 2, 3,]}|} in
  let json = parse_ok input in
  let items =
    Yojson.Safe.Util.(member "items" json |> to_list |> List.map to_int)
  in
  Alcotest.(check (list int)) "trailing comma array" [1; 2; 3] items

(* ── Triple-quoted string tests ──────────────────────────── *)

let test_triple_quoted_string () =
  let input = {|{
    "msg": '''
hello
world
'''
  }|} in
  let json = parse_ok input in
  let v = Yojson.Safe.Util.(member "msg" json |> to_string) in
  Alcotest.(check bool) "contains newline" true (String.contains v '\n')

(* ── Nested object tests ─────────────────────────────────── *)

let test_nested_objects () =
  let input =
    {|{
    outer: {
      inner: {
        value: 42,
      },
    },
  }|}
  in
  let json = parse_ok input in
  let v =
    Yojson.Safe.Util.(
      member "outer" json |> member "inner" |> member "value" |> to_int)
  in
  Alcotest.(check int) "nested value" 42 v

(* ── Edge cases ──────────────────────────────────────────── *)

let test_empty_object () =
  let json = parse_ok "{}" in
  match json with `Assoc [] -> () | _ -> Alcotest.fail "expected empty object"

let test_only_comments () =
  let input = {|{
    // nothing here
    /* also nothing */
  }|} in
  let json = parse_ok input in
  match json with `Assoc [] -> () | _ -> Alcotest.fail "expected empty object"

let test_boolean_values () =
  let input = {|{enabled: true, disabled: false, empty: null}|} in
  let json = parse_ok input in
  let e = Yojson.Safe.Util.(member "enabled" json |> to_bool) in
  let d = Yojson.Safe.Util.(member "disabled" json |> to_bool) in
  let n = Yojson.Safe.Util.(member "empty" json) in
  Alcotest.(check bool) "true" true e ;
  Alcotest.(check bool) "false" false d ;
  Alcotest.(check bool) "null" true (n = `Null)

let test_real_config_snippet () =
  let input =
    {|{
    config_version: 0,
    baker: "tz1Ke2h7sDMiMXmBjYi9KMGjJYkPEBEMG3Rr",
    payouts: {
      wallet_mode: "local-private-key",
      payout_mode: "ideal",
      fee: 0.05,
      baker_pays_transaction_fee: true,
      baker_pays_allocation_fee: false,
      minimum_payout_amount: 0.01,
      // Gas limits
      transaction_gas_limit_buffer: 100,
    },
    delegators: {
      requirements: {
        minimum_balance: 1.0,
      },
      ignore: [
        "tz1burnburnburnburnburnburnburjAYjjX",
      ],
    },
  }|}
  in
  let json = parse_ok input in
  let baker = Yojson.Safe.Util.(member "baker" json |> to_string) in
  Alcotest.(check string)
    "baker field"
    "tz1Ke2h7sDMiMXmBjYi9KMGjJYkPEBEMG3Rr"
    baker ;
  let fee =
    Yojson.Safe.Util.(member "payouts" json |> member "fee" |> to_number)
  in
  Alcotest.(check (float 0.001)) "fee" 0.05 fee

(* ── Test registration ───────────────────────────────────── *)

let comment_tests =
  [
    Alcotest.test_case "line comments" `Quick test_line_comments;
    Alcotest.test_case "block comments" `Quick test_block_comments;
    Alcotest.test_case
      "multiline block comments"
      `Quick
      test_block_comment_multiline;
    Alcotest.test_case "hash comments" `Quick test_hash_comments;
    Alcotest.test_case
      "comment in string preserved"
      `Quick
      test_comment_in_string_preserved;
  ]

let unquoted_key_tests =
  [
    Alcotest.test_case "unquoted keys" `Quick test_unquoted_keys;
    Alcotest.test_case "mixed quoting" `Quick test_mixed_quoted_unquoted;
  ]

let trailing_comma_tests =
  [
    Alcotest.test_case "trailing comma object" `Quick test_trailing_comma_object;
    Alcotest.test_case "trailing comma array" `Quick test_trailing_comma_array;
  ]

let multiline_tests =
  [Alcotest.test_case "triple-quoted string" `Quick test_triple_quoted_string]

let nested_tests =
  [Alcotest.test_case "nested objects" `Quick test_nested_objects]

let edge_case_tests =
  [
    Alcotest.test_case "empty object" `Quick test_empty_object;
    Alcotest.test_case "only comments" `Quick test_only_comments;
    Alcotest.test_case "boolean values" `Quick test_boolean_values;
    Alcotest.test_case "real config snippet" `Quick test_real_config_snippet;
  ]

let () =
  Alcotest.run
    "HJSON_parser"
    [
      ("comments", comment_tests);
      ("unquoted_keys", unquoted_key_tests);
      ("trailing_commas", trailing_comma_tests);
      ("multiline_strings", multiline_tests);
      ("nested_objects", nested_tests);
      ("edge_cases", edge_case_tests);
    ]
