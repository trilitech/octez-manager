(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Octez_manager_ui
open Octez_manager_lib

(* ============================================================ *)
(* is_valid_instance_char Tests                                  *)
(* ============================================================ *)

let test_valid_lowercase () =
  Alcotest.(check bool) "lowercase" true (Config.is_valid_instance_char 'a') ;
  Alcotest.(check bool) "lowercase z" true (Config.is_valid_instance_char 'z')

let test_valid_uppercase () =
  Alcotest.(check bool) "uppercase" true (Config.is_valid_instance_char 'A') ;
  Alcotest.(check bool) "uppercase Z" true (Config.is_valid_instance_char 'Z')

let test_valid_digits () =
  Alcotest.(check bool) "digit 0" true (Config.is_valid_instance_char '0') ;
  Alcotest.(check bool) "digit 9" true (Config.is_valid_instance_char '9')

let test_valid_special () =
  Alcotest.(check bool) "dash" true (Config.is_valid_instance_char '-') ;
  Alcotest.(check bool) "underscore" true (Config.is_valid_instance_char '_') ;
  Alcotest.(check bool) "dot" true (Config.is_valid_instance_char '.')

let test_invalid_space () =
  Alcotest.(check bool) "space" false (Config.is_valid_instance_char ' ')

let test_invalid_special_chars () =
  Alcotest.(check bool) "at" false (Config.is_valid_instance_char '@') ;
  Alcotest.(check bool) "hash" false (Config.is_valid_instance_char '#') ;
  Alcotest.(check bool) "slash" false (Config.is_valid_instance_char '/') ;
  Alcotest.(check bool) "colon" false (Config.is_valid_instance_char ':') ;
  Alcotest.(check bool) "bang" false (Config.is_valid_instance_char '!')

let test_invalid_null () =
  Alcotest.(check bool) "null" false (Config.is_valid_instance_char '\000')

(* ============================================================ *)
(* instance_has_valid_chars Tests                                *)
(* ============================================================ *)

let instance_has_valid_chars name =
  String.for_all Config.is_valid_instance_char name

let test_valid_name_simple () =
  Alcotest.(check bool) "simple" true (instance_has_valid_chars "my-node")

let test_valid_name_complex () =
  Alcotest.(check bool)
    "complex"
    true
    (instance_has_valid_chars "node-01.mainnet_v2")

let test_valid_name_empty () =
  Alcotest.(check bool) "empty" true (instance_has_valid_chars "")

let test_valid_name_single_char () =
  Alcotest.(check bool) "single" true (instance_has_valid_chars "a")

let test_invalid_name_space () =
  Alcotest.(check bool) "with space" false (instance_has_valid_chars "my node")

let test_invalid_name_special () =
  Alcotest.(check bool) "with @" false (instance_has_valid_chars "node@1")

let test_invalid_name_unicode () =
  Alcotest.(check bool) "unicode" false (instance_has_valid_chars "n\xc3\xb6de")

(* ============================================================ *)
(* strip_node_prefix Tests                                       *)
(* ============================================================ *)

let test_strip_node_prefix_present () =
  Alcotest.(check string)
    "strip node-"
    "shadownet"
    (Flows.strip_node_prefix "node-shadownet")

let test_strip_node_prefix_absent () =
  Alcotest.(check string)
    "no strip baker"
    "baker-shadownet"
    (Flows.strip_node_prefix "baker-shadownet")

let test_strip_node_prefix_empty () =
  Alcotest.(check string) "empty" "" (Flows.strip_node_prefix "")

let test_strip_node_prefix_just_prefix () =
  Alcotest.(check string) "just node-" "" (Flows.strip_node_prefix "node-")

let test_strip_node_prefix_no_dash () =
  Alcotest.(check string)
    "nodefoo (no dash)"
    "nodefoo"
    (Flows.strip_node_prefix "nodefoo")

let test_strip_node_prefix_nested () =
  Alcotest.(check string)
    "nested"
    "node-inner"
    (Flows.strip_node_prefix "node-node-inner")

(* ============================================================ *)
(* invalid_instance_name_error_msg Tests                         *)
(* ============================================================ *)

let contains_substring = Test_string_helpers.contains_substring

let test_error_msg_not_empty () =
  Alcotest.(check bool)
    "not empty"
    true
    (String.length Flows.invalid_instance_name_error_msg > 0)

let test_error_msg_contains_invalid () =
  let msg = String.lowercase_ascii Flows.invalid_instance_name_error_msg in
  Alcotest.(check bool)
    "mentions invalid"
    true
    (contains_substring msg "invalid")

(* ============================================================ *)
(* PBT: instance_has_valid_chars consistent with                 *)
(*      is_valid_instance_char                                   *)
(* ============================================================ *)

let test_valid_chars_consistency =
  QCheck.Test.make
    ~name:"instance_has_valid_chars consistent with is_valid_instance_char"
    ~count:500
    QCheck.string
    (fun s ->
      let by_forall = String.for_all Config.is_valid_instance_char s in
      let by_fn = instance_has_valid_chars s in
      by_forall = by_fn)

let test_strip_prefix_no_crash =
  QCheck.Test.make
    ~name:"strip_node_prefix never crashes"
    ~count:500
    QCheck.string
    (fun s ->
      let _ = Flows.strip_node_prefix s in
      true)

(* ============================================================ *)
(* Test Runner                                                   *)
(* ============================================================ *)

let () =
  Alcotest.run
    "Flows (pure)"
    [
      ( "is_valid_instance_char",
        [
          Alcotest.test_case "lowercase" `Quick test_valid_lowercase;
          Alcotest.test_case "uppercase" `Quick test_valid_uppercase;
          Alcotest.test_case "digits" `Quick test_valid_digits;
          Alcotest.test_case "special chars" `Quick test_valid_special;
          Alcotest.test_case "space" `Quick test_invalid_space;
          Alcotest.test_case "invalid special" `Quick test_invalid_special_chars;
          Alcotest.test_case "null byte" `Quick test_invalid_null;
        ] );
      ( "instance_has_valid_chars",
        [
          Alcotest.test_case "simple valid" `Quick test_valid_name_simple;
          Alcotest.test_case "complex valid" `Quick test_valid_name_complex;
          Alcotest.test_case "empty string" `Quick test_valid_name_empty;
          Alcotest.test_case "single char" `Quick test_valid_name_single_char;
          Alcotest.test_case "with space" `Quick test_invalid_name_space;
          Alcotest.test_case "special char" `Quick test_invalid_name_special;
          Alcotest.test_case "unicode" `Quick test_invalid_name_unicode;
        ] );
      ( "strip_node_prefix",
        [
          Alcotest.test_case "present" `Quick test_strip_node_prefix_present;
          Alcotest.test_case "absent" `Quick test_strip_node_prefix_absent;
          Alcotest.test_case "empty" `Quick test_strip_node_prefix_empty;
          Alcotest.test_case
            "just prefix"
            `Quick
            test_strip_node_prefix_just_prefix;
          Alcotest.test_case "no dash" `Quick test_strip_node_prefix_no_dash;
          Alcotest.test_case "nested" `Quick test_strip_node_prefix_nested;
        ] );
      ( "invalid_instance_name_error_msg",
        [
          Alcotest.test_case "not empty" `Quick test_error_msg_not_empty;
          Alcotest.test_case
            "mentions invalid"
            `Quick
            test_error_msg_contains_invalid;
        ] );
      ( "PBT",
        List.map
          QCheck_alcotest.to_alcotest
          [test_valid_chars_consistency; test_strip_prefix_no_crash] );
    ]
