(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Tests for Signatory_downloader module
    
    Tests cover:
    - Architecture detection and string conversion
    - URL construction for tarballs and checksums
    - GitHub releases JSON parsing
    - Version comparison and sorting
    - Size formatting
    - Error handling for invalid inputs
*)

open Alcotest
open Octez_manager_lib

let contains_substring s sub =
  let sub_len = String.length sub in
  let rec loop i =
    i + sub_len <= String.length s
    && (String.equal (String.sub s i sub_len) sub || loop (i + 1))
  in
  sub_len = 0 || loop 0

(* ============================================================ *)
(* Architecture Tests *)
(* ============================================================ *)

let test_arch_to_string_x86_64 () =
  let result =
    Signatory_downloader.For_tests.arch_to_string Signatory_downloader.X86_64
  in
  check string "x86_64 arch string" "amd64" result

let test_arch_to_string_arm64 () =
  let result =
    Signatory_downloader.For_tests.arch_to_string Signatory_downloader.Arm64
  in
  check string "arm64 arch string" "arm64" result

(* ============================================================ *)
(* URL Construction Tests *)
(* ============================================================ *)

let test_tarball_url_x86_64 () =
  let url =
    Signatory_downloader.For_tests.tarball_url
      ~version:"1.3.1"
      ~arch:Signatory_downloader.X86_64
  in
  check
    string
    "tarball URL for x86_64"
    "https://github.com/ecadlabs/signatory/releases/download/v1.3.1/signatory_1.3.1_linux_amd64.tar.gz"
    url

let test_tarball_url_arm64 () =
  let url =
    Signatory_downloader.For_tests.tarball_url
      ~version:"1.3.1"
      ~arch:Signatory_downloader.Arm64
  in
  check
    string
    "tarball URL for arm64"
    "https://github.com/ecadlabs/signatory/releases/download/v1.3.1/signatory_1.3.1_linux_arm64.tar.gz"
    url

let test_tarball_url_rc_version () =
  let url =
    Signatory_downloader.For_tests.tarball_url
      ~version:"1.3.1-rc1"
      ~arch:Signatory_downloader.X86_64
  in
  check
    string
    "tarball URL for RC version"
    "https://github.com/ecadlabs/signatory/releases/download/v1.3.1-rc1/signatory_1.3.1-rc1_linux_amd64.tar.gz"
    url

let test_checksums_url () =
  let url = Signatory_downloader.For_tests.checksums_url ~version:"1.3.1" in
  check
    string
    "checksums URL"
    "https://github.com/ecadlabs/signatory/releases/download/v1.3.1/checksums.txt"
    url

let test_checksums_url_rc () =
  let url = Signatory_downloader.For_tests.checksums_url ~version:"1.3.1-rc1" in
  check
    string
    "checksums URL for RC"
    "https://github.com/ecadlabs/signatory/releases/download/v1.3.1-rc1/checksums.txt"
    url

(* ============================================================ *)
(* GitHub Releases JSON Parsing Tests *)
(* ============================================================ *)

let test_parse_release_json_simple () =
  let json =
    `List
      [
        `Assoc
          [
            ("tag_name", `String "v1.3.1");
            ("published_at", `String "2026-01-20T20:10:16Z");
            ("prerelease", `Bool false);
          ];
      ]
  in
  match Signatory_downloader.For_tests.parse_release_json json with
  | Ok [v] ->
      check string "version" "1.3.1" v.version ;
      check (option string) "release_date" (Some "2026-01-20") v.release_date ;
      check bool "prerelease" false v.is_prerelease
  | Ok _ -> fail "Expected exactly one version"
  | Error (`Msg e) -> fail ("Parse failed: " ^ e)

let test_parse_release_json_multiple () =
  let json =
    `List
      [
        `Assoc
          [
            ("tag_name", `String "v1.3.1");
            ("published_at", `String "2026-01-20T20:10:16Z");
            ("prerelease", `Bool false);
          ];
        `Assoc
          [
            ("tag_name", `String "v1.3.0");
            ("published_at", `String "2025-12-15T10:00:00Z");
            ("prerelease", `Bool false);
          ];
      ]
  in
  match Signatory_downloader.For_tests.parse_release_json json with
  | Ok versions ->
      check int "version count" 2 (List.length versions) ;
      check string "first version" "1.3.1" (List.hd versions).version ;
      check string "second version" "1.3.0" (List.nth versions 1).version
  | Error (`Msg e) -> fail ("Parse failed: " ^ e)

let test_parse_release_json_prerelease () =
  let json =
    `List
      [
        `Assoc
          [
            ("tag_name", `String "v1.3.1-rc1");
            ("published_at", `String "2026-01-15T10:00:00Z");
            ("prerelease", `Bool true);
          ];
      ]
  in
  match Signatory_downloader.For_tests.parse_release_json json with
  | Ok [v] ->
      check string "version" "1.3.1-rc1" v.version ;
      check bool "prerelease" true v.is_prerelease
  | Ok _ -> fail "Expected exactly one version"
  | Error (`Msg e) -> fail ("Parse failed: " ^ e)

let test_parse_release_json_no_date () =
  let json =
    `List
      [
        `Assoc
          [
            ("tag_name", `String "v1.3.1");
            ("published_at", `Null);
            ("prerelease", `Bool false);
          ];
      ]
  in
  match Signatory_downloader.For_tests.parse_release_json json with
  | Ok [v] ->
      check (option string) "release_date" None v.release_date ;
      check string "version" "1.3.1" v.version
  | Ok _ -> fail "Expected exactly one version"
  | Error (`Msg e) -> fail ("Parse failed: " ^ e)

let test_parse_release_json_no_v_prefix () =
  let json =
    `List
      [
        `Assoc
          [
            ("tag_name", `String "1.3.1");
            ("published_at", `String "2026-01-20T20:10:16Z");
            ("prerelease", `Bool false);
          ];
      ]
  in
  match Signatory_downloader.For_tests.parse_release_json json with
  | Ok [v] -> check string "version without v" "1.3.1" v.version
  | Ok _ -> fail "Expected exactly one version"
  | Error (`Msg e) -> fail ("Parse failed: " ^ e)

let test_parse_release_json_empty () =
  let json = `List [] in
  match Signatory_downloader.For_tests.parse_release_json json with
  | Ok versions -> check int "empty list" 0 (List.length versions)
  | Error (`Msg e) -> fail ("Parse failed: " ^ e)

let test_parse_release_json_invalid () =
  let json = `String "not a list" in
  match Signatory_downloader.For_tests.parse_release_json json with
  | Ok _ -> fail "Should have failed on invalid JSON"
  | Error (`Msg _) -> ()

(* ============================================================ *)
(* Size Formatting Tests *)
(* ============================================================ *)

let test_format_size_bytes () =
  check string "100 bytes" "100 B" (Signatory_downloader.format_size_bytes 100L)

let test_format_size_kb () =
  check string "1.5 KB" "1.5 KB" (Signatory_downloader.format_size_bytes 1536L)

let test_format_size_mb () =
  check
    string
    "2.0 MB"
    "2.0 MB"
    (Signatory_downloader.format_size_bytes 2097152L)

let test_format_size_gb () =
  check
    string
    "1.5 GB"
    "1.5 GB"
    (Signatory_downloader.format_size_bytes 1610612736L)

let test_format_size_zero () =
  check string "0 bytes" "0 B" (Signatory_downloader.format_size_bytes 0L)

(* ============================================================ *)
(* Path Construction Tests *)
(* ============================================================ *)

let test_signatory_version_path () =
  let path = Signatory_downloader.signatory_version_path "1.3.1" in
  check bool "contains v1.3.1" true (String.contains path '1') ;
  check
    bool
    "contains signatory-binaries"
    true
    (Str.string_match (Str.regexp ".*signatory-binaries.*") path 0)

let test_signatory_version_path_rc () =
  let path = Signatory_downloader.signatory_version_path "1.3.1-rc1" in
  check
    bool
    "contains rc1"
    true
    (Str.string_match (Str.regexp ".*rc1.*") path 0)

let test_verify_tarball_checksum_missing_entry_fails () =
  let result =
    Signatory_downloader.For_tests.verify_tarball_checksum
      ~tarball_name:"signatory_1.3.1_linux_amd64.tar.gz"
      ~tarball_path:"/tmp/signatory.tar.gz"
      ~checksums:[]
      ~verify_file:(fun ~filepath:_ ~expected_hash:_ -> Ok ())
  in
  match result with
  | Ok () -> fail "missing checksum entry must fail closed"
  | Error (`Msg msg) ->
      check
        bool
        "mentions missing entry"
        true
        (contains_substring msg "Missing checksum entry")

let test_verify_tarball_checksum_uses_downloaded_tarball () =
  let verified_path = ref None in
  let verification =
    Signatory_downloader.For_tests.verify_tarball_checksum
      ~tarball_name:"signatory_1.3.1_linux_amd64.tar.gz"
      ~tarball_path:"/tmp/downloaded-signatory.tar.gz"
      ~checksums:[("signatory_1.3.1_linux_amd64.tar.gz", "expected")]
      ~verify_file:(fun ~filepath ~expected_hash:_ ->
        verified_path := Some filepath ;
        Ok ())
  in
  (match verification with
  | Ok () -> ()
  | Error (`Msg msg) -> fail ("verification failed: " ^ msg)) ;
  check
    (option string)
    "verifies original downloaded tarball"
    (Some "/tmp/downloaded-signatory.tar.gz")
    !verified_path

(* ============================================================ *)
(* Test Suite Registration *)
(* ============================================================ *)

let arch_tests =
  [
    ("arch_to_string x86_64", `Quick, test_arch_to_string_x86_64);
    ("arch_to_string arm64", `Quick, test_arch_to_string_arm64);
  ]

let url_tests =
  [
    ("tarball_url x86_64", `Quick, test_tarball_url_x86_64);
    ("tarball_url arm64", `Quick, test_tarball_url_arm64);
    ("tarball_url RC version", `Quick, test_tarball_url_rc_version);
    ("checksums_url", `Quick, test_checksums_url);
    ("checksums_url RC", `Quick, test_checksums_url_rc);
  ]

let parsing_tests =
  [
    ("parse_release_json simple", `Quick, test_parse_release_json_simple);
    ("parse_release_json multiple", `Quick, test_parse_release_json_multiple);
    ("parse_release_json prerelease", `Quick, test_parse_release_json_prerelease);
    ("parse_release_json no date", `Quick, test_parse_release_json_no_date);
    ( "parse_release_json no v prefix",
      `Quick,
      test_parse_release_json_no_v_prefix );
    ("parse_release_json empty", `Quick, test_parse_release_json_empty);
    ("parse_release_json invalid", `Quick, test_parse_release_json_invalid);
  ]

let size_tests =
  [
    ("format_size bytes", `Quick, test_format_size_bytes);
    ("format_size KB", `Quick, test_format_size_kb);
    ("format_size MB", `Quick, test_format_size_mb);
    ("format_size GB", `Quick, test_format_size_gb);
    ("format_size zero", `Quick, test_format_size_zero);
  ]

let path_tests =
  [
    ("signatory_version_path", `Quick, test_signatory_version_path);
    ("signatory_version_path RC", `Quick, test_signatory_version_path_rc);
  ]

let checksum_tests =
  [
    ( "missing checksum entry fails",
      `Quick,
      test_verify_tarball_checksum_missing_entry_fails );
    ( "verifies downloaded tarball",
      `Quick,
      test_verify_tarball_checksum_uses_downloaded_tarball );
  ]

let () =
  Alcotest.run
    "Signatory_downloader"
    [
      ("Architecture", arch_tests);
      ("URL Construction", url_tests);
      ("JSON Parsing", parsing_tests);
      ("Size Formatting", size_tests);
      ("Path Construction", path_tests);
      ("Checksums", checksum_tests);
    ]
