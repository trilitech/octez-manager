(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Tests for Binary_registry module - JSON serialization and pure functions *)

open Alcotest
open Octez_manager_lib
module BR = Binary_registry.For_tests

(** {2 Test utilities} *)

let check_error_result actual =
  match actual with
  | Ok _ -> Alcotest.fail "Expected Error, got Ok"
  | Error _ -> ()

(** {2 bin_source_to_string tests} *)

let test_bin_source_to_string_managed () =
  let source = Binary_registry.Managed_version "24.0" in
  let result = BR.bin_source_to_string source in
  check string "managed version string" "v24.0 (managed)" result

let test_bin_source_to_string_linked () =
  let source = Binary_registry.Linked_alias "dev-build" in
  let result = BR.bin_source_to_string source in
  check string "linked alias string" "dev-build (linked)" result

let test_bin_source_to_string_raw () =
  let source = Binary_registry.Raw_path "/usr/local/bin" in
  let result = BR.bin_source_to_string source in
  check string "raw path string" "/usr/local/bin" result

(** {2 bin_source JSON serialization tests} *)

let test_bin_source_to_yojson_managed () =
  let source = Binary_registry.Managed_version "24.0" in
  let json = BR.bin_source_to_yojson source in
  let expected =
    `Assoc [("type", `String "managed"); ("version", `String "24.0")]
  in
  check (testable Yojson.Safe.pp Yojson.Safe.equal) "managed JSON" expected json

let test_bin_source_to_yojson_linked () =
  let source = Binary_registry.Linked_alias "dev-build" in
  let json = BR.bin_source_to_yojson source in
  let expected =
    `Assoc [("type", `String "linked"); ("alias", `String "dev-build")]
  in
  check (testable Yojson.Safe.pp Yojson.Safe.equal) "linked JSON" expected json

let test_bin_source_to_yojson_raw () =
  let source = Binary_registry.Raw_path "/usr/local/bin" in
  let json = BR.bin_source_to_yojson source in
  let expected =
    `Assoc [("type", `String "path"); ("path", `String "/usr/local/bin")]
  in
  check
    (testable Yojson.Safe.pp Yojson.Safe.equal)
    "raw path JSON"
    expected
    json

let test_bin_source_of_yojson_managed () =
  let json =
    `Assoc [("type", `String "managed"); ("version", `String "24.0")]
  in
  let result = BR.bin_source_of_yojson json in
  match result with
  | Ok (Binary_registry.Managed_version "24.0") -> ()
  | Ok _ -> Alcotest.fail "Expected Managed_version \"24.0\""
  | Error (`Msg err) -> Alcotest.fail (Printf.sprintf "Parse failed: %s" err)

let test_bin_source_of_yojson_linked () =
  let json =
    `Assoc [("type", `String "linked"); ("alias", `String "dev-build")]
  in
  let result = BR.bin_source_of_yojson json in
  match result with
  | Ok (Binary_registry.Linked_alias "dev-build") -> ()
  | Ok _ -> Alcotest.fail "Expected Linked_alias \"dev-build\""
  | Error (`Msg err) -> Alcotest.fail (Printf.sprintf "Parse failed: %s" err)

let test_bin_source_of_yojson_raw () =
  let json =
    `Assoc [("type", `String "path"); ("path", `String "/usr/local/bin")]
  in
  let result = BR.bin_source_of_yojson json in
  match result with
  | Ok (Binary_registry.Raw_path "/usr/local/bin") -> ()
  | Ok _ -> Alcotest.fail "Expected Raw_path \"/usr/local/bin\""
  | Error (`Msg err) -> Alcotest.fail (Printf.sprintf "Parse failed: %s" err)

let test_bin_source_of_yojson_backward_compat () =
  (* Backward compatibility: plain string should be treated as raw path *)
  let json = `String "/old/path" in
  let result = BR.bin_source_of_yojson json in
  match result with
  | Ok (Binary_registry.Raw_path "/old/path") -> ()
  | Ok _ -> Alcotest.fail "Expected Raw_path \"/old/path\""
  | Error (`Msg err) -> Alcotest.fail (Printf.sprintf "Parse failed: %s" err)

let test_bin_source_of_yojson_invalid_type () =
  let json = `Assoc [("type", `String "unknown")] in
  let result = BR.bin_source_of_yojson json in
  check_error_result result

let test_bin_source_of_yojson_invalid_format () =
  let json = `Int 42 in
  let result = BR.bin_source_of_yojson json in
  check_error_result result

let test_bin_source_of_legacy () =
  let source = BR.bin_source_of_legacy "/legacy/path" in
  match source with
  | Binary_registry.Raw_path "/legacy/path" -> ()
  | _ -> Alcotest.fail "Expected Raw_path"

(** {2 linked_dir JSON serialization tests} *)

let test_linked_dir_to_yojson () =
  let ld : Binary_registry.linked_dir =
    {alias = "dev"; path = "/home/user/dev"}
  in
  let json = BR.linked_dir_to_yojson ld in
  let expected =
    `Assoc [("alias", `String "dev"); ("path", `String "/home/user/dev")]
  in
  check
    (testable Yojson.Safe.pp Yojson.Safe.equal)
    "linked_dir JSON"
    expected
    json

let test_linked_dir_of_yojson () =
  let json =
    `Assoc [("alias", `String "dev"); ("path", `String "/home/user/dev")]
  in
  let result = BR.linked_dir_of_yojson json in
  match result with
  | Ok ld ->
      check string "alias" "dev" ld.Binary_registry.alias ;
      check string "path" "/home/user/dev" ld.Binary_registry.path
  | Error (`Msg err) -> Alcotest.fail (Printf.sprintf "Parse failed: %s" err)

let test_linked_dir_of_yojson_missing_field () =
  let json = `Assoc [("alias", `String "dev")] in
  let result = BR.linked_dir_of_yojson json in
  check_error_result result

let test_linked_dirs_to_yojson () =
  let dirs : Binary_registry.linked_dir list =
    [
      {alias = "dev"; path = "/home/user/dev"};
      {alias = "prod"; path = "/opt/octez"};
    ]
  in
  let json = BR.linked_dirs_to_yojson dirs in
  match json with
  | `List [_; _] -> ()
  | _ -> Alcotest.fail "Expected list of 2 elements"

let test_linked_dirs_of_yojson () =
  let json =
    `List
      [
        `Assoc [("alias", `String "dev"); ("path", `String "/home/user/dev")];
        `Assoc [("alias", `String "prod"); ("path", `String "/opt/octez")];
      ]
  in
  let result = BR.linked_dirs_of_yojson json in
  match result with
  | Ok dirs ->
      check int "list length" 2 (List.length dirs) ;
      check string "first alias" "dev" (List.hd dirs).Binary_registry.alias
  | Error (`Msg err) -> Alcotest.fail (Printf.sprintf "Parse failed: %s" err)

let test_linked_dirs_of_yojson_empty () =
  let json = `List [] in
  let result = BR.linked_dirs_of_yojson json in
  match result with
  | Ok dirs -> check int "empty list" 0 (List.length dirs)
  | Error (`Msg err) -> Alcotest.fail (Printf.sprintf "Parse failed: %s" err)

let test_linked_dirs_of_yojson_invalid_entry () =
  let json = `List [`Assoc [("alias", `String "dev")]] in
  let result = BR.linked_dirs_of_yojson json in
  check_error_result result

(** {2 compare_versions tests} *)

let test_compare_versions_equal () =
  let result = BR.compare_versions "24.0" "24.0" in
  check int "equal versions" 0 result

let test_compare_versions_greater () =
  let result = BR.compare_versions "24.0" "9.0" in
  check bool "24.0 > 9.0" true (result > 0)

let test_compare_versions_less () =
  let result = BR.compare_versions "9.0" "24.0" in
  check bool "9.0 < 24.0" true (result < 0)

let test_compare_versions_multi_part () =
  let result = BR.compare_versions "24.1.3" "24.1.2" in
  check bool "24.1.3 > 24.1.2" true (result > 0)

let test_compare_versions_different_lengths () =
  let result = BR.compare_versions "24.0.0" "24.0" in
  check bool "24.0.0 > 24.0" true (result > 0)

let test_compare_versions_different_lengths_reverse () =
  let result = BR.compare_versions "24.0" "24.0.0" in
  check bool "24.0 < 24.0.0" true (result < 0)

let test_compare_versions_non_numeric () =
  (* Non-numeric parts should be treated as 0 *)
  let result = BR.compare_versions "24.alpha" "24.0" in
  check int "non-numeric treated as 0" 0 result

(** {2 Test suite} *)

let () =
  Alcotest.run
    "Binary_registry"
    [
      ( "bin_source_to_string",
        [
          test_case "managed version" `Quick test_bin_source_to_string_managed;
          test_case "linked alias" `Quick test_bin_source_to_string_linked;
          test_case "raw path" `Quick test_bin_source_to_string_raw;
        ] );
      ( "bin_source JSON serialization",
        [
          test_case "to_yojson managed" `Quick test_bin_source_to_yojson_managed;
          test_case "to_yojson linked" `Quick test_bin_source_to_yojson_linked;
          test_case "to_yojson raw" `Quick test_bin_source_to_yojson_raw;
          test_case "of_yojson managed" `Quick test_bin_source_of_yojson_managed;
          test_case "of_yojson linked" `Quick test_bin_source_of_yojson_linked;
          test_case "of_yojson raw" `Quick test_bin_source_of_yojson_raw;
          test_case
            "of_yojson backward compat"
            `Quick
            test_bin_source_of_yojson_backward_compat;
          test_case
            "of_yojson invalid type"
            `Quick
            test_bin_source_of_yojson_invalid_type;
          test_case
            "of_yojson invalid format"
            `Quick
            test_bin_source_of_yojson_invalid_format;
          test_case "of_legacy" `Quick test_bin_source_of_legacy;
        ] );
      ( "linked_dir JSON serialization",
        [
          test_case "to_yojson" `Quick test_linked_dir_to_yojson;
          test_case "of_yojson" `Quick test_linked_dir_of_yojson;
          test_case
            "of_yojson missing field"
            `Quick
            test_linked_dir_of_yojson_missing_field;
          test_case "list to_yojson" `Quick test_linked_dirs_to_yojson;
          test_case "list of_yojson" `Quick test_linked_dirs_of_yojson;
          test_case
            "list of_yojson empty"
            `Quick
            test_linked_dirs_of_yojson_empty;
          test_case
            "list of_yojson invalid"
            `Quick
            test_linked_dirs_of_yojson_invalid_entry;
        ] );
      ( "compare_versions",
        [
          test_case "equal versions" `Quick test_compare_versions_equal;
          test_case "greater version" `Quick test_compare_versions_greater;
          test_case "less version" `Quick test_compare_versions_less;
          test_case
            "multi-part versions"
            `Quick
            test_compare_versions_multi_part;
          test_case
            "different lengths"
            `Quick
            test_compare_versions_different_lengths;
          test_case
            "different lengths reverse"
            `Quick
            test_compare_versions_different_lengths_reverse;
          test_case "non-numeric parts" `Quick test_compare_versions_non_numeric;
        ] );
    ]
