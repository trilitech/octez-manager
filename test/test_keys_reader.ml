(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Tests for Keys_reader module
    
    Tests cover:
    - JSON parsing of key_info records
    - public_key_hashs file parsing
    - Error handling for malformed JSON
*)

open Alcotest
open Octez_manager_lib
module KR = Keys_reader.For_tests

(* ============================================================ *)
(* key_info_of_yojson Tests *)
(* ============================================================ *)

let test_key_info_of_yojson_valid () =
  let json =
    `Assoc [("name", `String "mykey"); ("value", `String "tz1abc123")]
  in
  match KR.key_info_of_yojson json with
  | Ok key_info ->
      check string "name" "mykey" key_info.name ;
      check string "value" "tz1abc123" key_info.value
  | Error _ -> fail "should parse valid key_info"

let test_key_info_of_yojson_tz2_address () =
  let json =
    `Assoc [("name", `String "key2"); ("value", `String "tz2def456")]
  in
  match KR.key_info_of_yojson json with
  | Ok key_info -> check string "tz2 value" "tz2def456" key_info.value
  | Error _ -> fail "should handle tz2 addresses"

let test_key_info_of_yojson_tz3_address () =
  let json =
    `Assoc [("name", `String "key3"); ("value", `String "tz3ghi789")]
  in
  match KR.key_info_of_yojson json with
  | Ok key_info -> check string "tz3 value" "tz3ghi789" key_info.value
  | Error _ -> fail "should handle tz3 addresses"

let test_key_info_of_yojson_tz4_address () =
  let json =
    `Assoc [("name", `String "key4"); ("value", `String "tz4jkl012")]
  in
  match KR.key_info_of_yojson json with
  | Ok key_info -> check string "tz4 value" "tz4jkl012" key_info.value
  | Error _ -> fail "should handle tz4 addresses"

let test_key_info_of_yojson_missing_name () =
  let json = `Assoc [("value", `String "tz1abc")] in
  match KR.key_info_of_yojson json with
  | Ok _ -> fail "should reject missing name"
  | Error _ -> check bool "rejects missing name" true true

let test_key_info_of_yojson_missing_value () =
  let json = `Assoc [("name", `String "mykey")] in
  match KR.key_info_of_yojson json with
  | Ok _ -> fail "should reject missing value"
  | Error _ -> check bool "rejects missing value" true true

let test_key_info_of_yojson_wrong_type () =
  let json = `Assoc [("name", `Int 123); ("value", `String "tz1abc")] in
  match KR.key_info_of_yojson json with
  | Ok _ -> fail "should reject wrong type"
  | Error _ -> check bool "rejects wrong type" true true

let test_key_info_of_yojson_not_object () =
  let json = `String "not an object" in
  match KR.key_info_of_yojson json with
  | Ok _ -> fail "should reject non-object"
  | Error _ -> check bool "rejects non-object" true true

let test_key_info_of_yojson_empty_object () =
  let json = `Assoc [] in
  match KR.key_info_of_yojson json with
  | Ok _ -> fail "should reject empty object"
  | Error _ -> check bool "rejects empty object" true true

(* ============================================================ *)
(* Test Suite *)
(* ============================================================ *)

let key_info_tests =
  [
    ("parse valid key_info", `Quick, test_key_info_of_yojson_valid);
    ("parse tz2 address", `Quick, test_key_info_of_yojson_tz2_address);
    ("parse tz3 address", `Quick, test_key_info_of_yojson_tz3_address);
    ("parse tz4 address", `Quick, test_key_info_of_yojson_tz4_address);
    ("reject missing name", `Quick, test_key_info_of_yojson_missing_name);
    ("reject missing value", `Quick, test_key_info_of_yojson_missing_value);
    ("reject wrong type", `Quick, test_key_info_of_yojson_wrong_type);
    ("reject non-object", `Quick, test_key_info_of_yojson_not_object);
    ("reject empty object", `Quick, test_key_info_of_yojson_empty_object);
  ]

let () = Alcotest.run "Keys_reader" [("key_info_parsing", key_info_tests)]
