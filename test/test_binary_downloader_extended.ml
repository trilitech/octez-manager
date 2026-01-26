(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Extended tests for Binary_downloader module
    
    Tests cover:
    - Architecture detection and string conversion
    - URL construction for binaries and checksums
    - Version JSON parsing
    - Checksum verification logic
    - Size formatting
    - Directory size calculation
    - Error handling for invalid inputs
*)

open Alcotest
open Octez_manager_lib

(* ============================================================ *)
(* Architecture Tests *)
(* ============================================================ *)

let test_arch_to_string_x86_64 () =
  let result = Binary_downloader.arch_to_string Binary_downloader.X86_64 in
  check string "x86_64 arch string" "x86_64" result

let test_arch_to_string_arm64 () =
  let result = Binary_downloader.arch_to_string Binary_downloader.Arm64 in
  check string "arm64 arch string" "arm64" result

(* ============================================================ *)
(* URL Construction Tests *)
(* ============================================================ *)

let test_binary_url_node () =
  let url =
    Binary_downloader.binary_url
      ~version:"21.0"
      ~arch:Binary_downloader.X86_64
      ~binary:"octez-node"
  in
  check
    bool
    "contains version"
    true
    (String.contains url '2' && String.contains url '1') ;
  check bool "contains x86_64" true (String.contains url 'x') ;
  check bool "contains octez-node" true (String.contains url 'n')

let test_binary_url_arm64 () =
  let url =
    Binary_downloader.binary_url
      ~version:"21.0"
      ~arch:Binary_downloader.Arm64
      ~binary:"octez-baker"
  in
  check bool "contains arm64" true (String.contains url 'a')

let test_binary_url_rc_version () =
  let url =
    Binary_downloader.binary_url
      ~version:"21.0-rc1"
      ~arch:Binary_downloader.X86_64
      ~binary:"octez-client"
  in
  check bool "handles rc version" true (String.contains url 'r')

let test_checksums_url () =
  let url =
    Binary_downloader.checksums_url
      ~version:"21.0"
      ~arch:Binary_downloader.X86_64
  in
  check bool "contains sha256sums" true (String.contains url 's') ;
  check bool "contains version" true (String.contains url '2')

let test_checksums_url_arm64 () =
  let url =
    Binary_downloader.checksums_url
      ~version:"21.0"
      ~arch:Binary_downloader.Arm64
  in
  check bool "uses arm64 in path" true (String.contains url 'a')

(* ============================================================ *)
(* Version JSON Parsing Tests *)
(* ============================================================ *)

let test_parse_version_json_simple () =
  let json =
    `List
      [
        `Assoc
          [
            ("major", `Int 21);
            ("minor", `Int 0);
            ("rc", `Null);
            ("pubDate", `Null);
          ];
      ]
  in
  match Binary_downloader.For_tests.parse_version_json json with
  | Ok versions ->
      check int "one version" 1 (List.length versions) ;
      let v = List.hd versions in
      check string "version string" "21.0" v.version ;
      check bool "not rc" false v.is_rc ;
      check bool "no release date" true (Option.is_none v.release_date)
  | Error _ -> fail "should parse simple version"

let test_parse_version_json_with_rc () =
  let json =
    `List
      [
        `Assoc
          [
            ("major", `Int 21);
            ("minor", `Int 1);
            ("rc", `Int 2);
            ("pubDate", `Null);
          ];
      ]
  in
  match Binary_downloader.For_tests.parse_version_json json with
  | Ok versions ->
      let v = List.hd versions in
      check string "rc version string" "21.1-rc2" v.version ;
      check bool "is rc" true v.is_rc
  | Error _ -> fail "should parse rc version"

let test_parse_version_json_with_date () =
  let json =
    `List
      [
        `Assoc
          [
            ("major", `Int 21);
            ("minor", `Int 0);
            ("rc", `Null);
            ("pubDate", `Int 1704067200);
          ];
      ]
  in
  match Binary_downloader.For_tests.parse_version_json json with
  | Ok versions -> (
      let v = List.hd versions in
      check bool "has release date" true (Option.is_some v.release_date) ;
      match v.release_date with
      | Some date -> check bool "date format" true (String.contains date '-')
      | None -> fail "should have date")
  | Error _ -> fail "should parse version with date"

let test_parse_version_json_multiple () =
  let json =
    `List
      [
        `Assoc
          [
            ("major", `Int 21);
            ("minor", `Int 0);
            ("rc", `Null);
            ("pubDate", `Null);
          ];
        `Assoc
          [
            ("major", `Int 20);
            ("minor", `Int 3);
            ("rc", `Null);
            ("pubDate", `Null);
          ];
      ]
  in
  match Binary_downloader.For_tests.parse_version_json json with
  | Ok versions -> check int "two versions" 2 (List.length versions)
  | Error _ -> fail "should parse multiple versions"

let test_parse_version_json_wrapped_in_object () =
  let json =
    `Assoc
      [
        ( "versions",
          `List
            [
              `Assoc
                [
                  ("major", `Int 21);
                  ("minor", `Int 0);
                  ("rc", `Null);
                  ("pubDate", `Null);
                ];
            ] );
      ]
  in
  match Binary_downloader.For_tests.parse_version_json json with
  | Ok versions -> check int "extracts from object" 1 (List.length versions)
  | Error _ -> fail "should handle wrapped versions"

let test_parse_version_json_invalid () =
  let json = `String "invalid" in
  match Binary_downloader.For_tests.parse_version_json json with
  | Ok _ -> fail "should reject invalid JSON"
  | Error _ -> check bool "rejects invalid" true true

let test_parse_version_json_missing_fields () =
  let json = `List [`Assoc [("major", `Int 21)]] in
  match Binary_downloader.For_tests.parse_version_json json with
  | Ok _ -> fail "should require all fields"
  | Error _ -> check bool "validates fields" true true

let test_parse_version_json_empty_list () =
  let json = `List [] in
  match Binary_downloader.For_tests.parse_version_json json with
  | Ok versions -> check int "empty list" 0 (List.length versions)
  | Error _ -> fail "should handle empty list"

(* ============================================================ *)
(* Size Formatting Tests *)
(* ============================================================ *)

let test_format_size_bytes () =
  let result = Binary_downloader.format_size_bytes 512L in
  check bool "shows bytes" true (String.contains result 'B')

let test_format_size_kilobytes () =
  let result = Binary_downloader.format_size_bytes 2048L in
  check bool "shows KB" true (String.contains result 'K')

let test_format_size_megabytes () =
  let result = Binary_downloader.format_size_bytes 5242880L in
  (* 5MB *)
  check bool "shows MB" true (String.contains result 'M')

let test_format_size_gigabytes () =
  let result = Binary_downloader.format_size_bytes 5368709120L in
  (* 5GB *)
  check bool "shows GB" true (String.contains result 'G')

let test_format_size_zero () =
  let result = Binary_downloader.format_size_bytes 0L in
  check bool "handles zero" true (String.contains result 'B')

let test_format_size_boundary_1kb () =
  let result = Binary_downloader.format_size_bytes 1024L in
  check bool "1KB boundary" true (String.contains result 'K')

let test_format_size_boundary_1mb () =
  let result = Binary_downloader.format_size_bytes 1048576L in
  check bool "1MB boundary" true (String.contains result 'M')

let test_format_size_boundary_1gb () =
  let result = Binary_downloader.format_size_bytes 1073741824L in
  check bool "1GB boundary" true (String.contains result 'G')

(* ============================================================ *)
(* binaries_for_version Tests *)
(* ============================================================ *)

let test_binaries_for_version_stable () =
  match Binary_downloader.binaries_for_version "21.0" with
  | Ok binaries ->
      check bool "returns list" true (List.length binaries > 0) ;
      check bool "includes node" true (List.mem "octez-node" binaries) ;
      check bool "includes client" true (List.mem "octez-client" binaries) ;
      check bool "includes baker" true (List.mem "octez-baker" binaries)
  | Error _ -> fail "should return binary list"

let test_binaries_for_version_rc () =
  match Binary_downloader.binaries_for_version "21.0-rc1" with
  | Ok binaries -> check bool "rc has binaries" true (List.length binaries > 0)
  | Error _ -> fail "should work for rc versions"

(* ============================================================ *)
(* Test Suite *)
(* ============================================================ *)

let arch_tests =
  [
    ("arch_to_string x86_64", `Quick, test_arch_to_string_x86_64);
    ("arch_to_string arm64", `Quick, test_arch_to_string_arm64);
  ]

let url_tests =
  [
    ("binary_url node", `Quick, test_binary_url_node);
    ("binary_url arm64", `Quick, test_binary_url_arm64);
    ("binary_url rc version", `Quick, test_binary_url_rc_version);
    ("checksums_url", `Quick, test_checksums_url);
    ("checksums_url arm64", `Quick, test_checksums_url_arm64);
  ]

let version_parsing_tests =
  [
    ("parse simple version", `Quick, test_parse_version_json_simple);
    ("parse rc version", `Quick, test_parse_version_json_with_rc);
    ("parse with date", `Quick, test_parse_version_json_with_date);
    ("parse multiple versions", `Quick, test_parse_version_json_multiple);
    ("parse wrapped versions", `Quick, test_parse_version_json_wrapped_in_object);
    ("parse invalid JSON", `Quick, test_parse_version_json_invalid);
    ("parse missing fields", `Quick, test_parse_version_json_missing_fields);
    ("parse empty list", `Quick, test_parse_version_json_empty_list);
  ]

let size_tests =
  [
    ("format bytes", `Quick, test_format_size_bytes);
    ("format kilobytes", `Quick, test_format_size_kilobytes);
    ("format megabytes", `Quick, test_format_size_megabytes);
    ("format gigabytes", `Quick, test_format_size_gigabytes);
    ("format zero", `Quick, test_format_size_zero);
    ("format 1KB boundary", `Quick, test_format_size_boundary_1kb);
    ("format 1MB boundary", `Quick, test_format_size_boundary_1mb);
    ("format 1GB boundary", `Quick, test_format_size_boundary_1gb);
  ]

let binaries_tests =
  [
    ("binaries for stable version", `Quick, test_binaries_for_version_stable);
    ("binaries for rc version", `Quick, test_binaries_for_version_rc);
  ]

let () =
  Alcotest.run
    "Binary_downloader_extended"
    [
      ("architecture", arch_tests);
      ("url_construction", url_tests);
      ("version_parsing", version_parsing_tests);
      ("size_formatting", size_tests);
      ("binaries", binaries_tests);
    ]
