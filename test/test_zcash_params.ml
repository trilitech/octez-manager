(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Unit tests for installer/zcash_params.ml - Zcash parameter detection.
    
    Tests path detection logic and checksum verification without requiring
    actual downloads or network calls. *)

open Alcotest
open Octez_manager_lib
module ZP = Zcash_params.Internal_for_tests

(******************************************************************************)
(*                    PARAMETER METADATA TESTS                                *)
(******************************************************************************)

(** Test: Required params list contains exactly 2 files *)
let test_required_params_count () =
  let count = List.length ZP.required_params in
  check int "exactly 2 parameter files" 2 count

(** Test: Required params have expected names *)
let test_required_params_names () =
  let names = List.map (fun p -> p.ZP.name) ZP.required_params in
  let expected = ["sapling-spend.params"; "sapling-output.params"] in
  check (list string) "parameter names match" expected names

(** Test: All checksums are 64-character hex strings *)
let test_checksums_format () =
  List.iter
    (fun param ->
      check
        int
        (Printf.sprintf "%s checksum length" param.ZP.name)
        64
        (String.length param.ZP.sha256) ;
      (* Verify all characters are hex digits *)
      String.iter
        (fun c ->
          check
            bool
            (Printf.sprintf "char '%c' is hex digit" c)
            true
            (match c with
            | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
            | _ -> false))
        param.ZP.sha256)
    ZP.required_params

(******************************************************************************)
(*                    SEARCH PATH TESTS                                       *)
(******************************************************************************)

(** Test: Search paths are generated correctly *)
let test_search_paths () =
  let home = "/home/testuser" in
  let paths = ZP.get_search_paths ~home_dir:home in
  let expected =
    [
      "/home/testuser/.local/share/zcash-params";
      "/home/testuser/.zcash-params";
      "/home/testuser/_opam/share/zcash-params";
      "/usr/local/share/zcash-params";
      "/usr/share/zcash-params";
    ]
  in
  check (list string) "search paths match expected" expected paths

(** Test: Search paths handle trailing slash in home dir *)
let test_search_paths_trailing_slash () =
  let home = "/home/testuser/" in
  let paths = ZP.get_search_paths ~home_dir:home in
  (* Filename.concat handles trailing slashes correctly *)
  List.iter
    (fun path ->
      check
        bool
        (Printf.sprintf "path '%s' doesn't have double slash" path)
        false
        (String.contains path '/' && String.contains path '/'
        && Str.string_match (Str.regexp ".*//.*") path 0))
    paths

(** Test: Search paths include both user and system directories *)
let test_search_paths_coverage () =
  let home = "/var/lib/octez" in
  let paths = ZP.get_search_paths ~home_dir:home in
  (* Should have user-specific paths *)
  let has_user_paths =
    List.exists (fun p -> String.starts_with ~prefix:home p) paths
  in
  (* Should have system-wide paths *)
  let has_system_paths =
    List.exists
      (fun p ->
        String.starts_with ~prefix:"/usr/local" p
        || String.starts_with ~prefix:"/usr/share" p)
      paths
  in
  check bool "has user-specific paths" true has_user_paths ;
  check bool "has system-wide paths" true has_system_paths

(******************************************************************************)
(*                    DOWNLOAD URL TESTS                                      *)
(******************************************************************************)

(** Test: Download base URL is correct *)
let test_download_base_url () =
  check
    string
    "download base URL"
    "https://download.z.cash/downloads/"
    ZP.download_base_url

(** Test: Download base URL ends with slash *)
let test_download_url_trailing_slash () =
  check
    bool
    "download URL ends with /"
    true
    (String.ends_with ~suffix:"/" ZP.download_base_url)

(******************************************************************************)
(*                    VERIFICATION TESTS                                      *)
(******************************************************************************)

(** Test: verify_params_in_dir returns false for non-existent directory *)
let test_verify_nonexistent_dir () =
  let result = ZP.verify_params_in_dir "/nonexistent/path/zcash-params" in
  check bool "non-existent directory returns false" false result

(** Test: verify_params_in_dir returns false for empty directory *)
let test_verify_empty_dir () =
  (* Create a temporary empty directory *)
  let temp_dir = Filename.temp_file "zcash_test" ".dir" in
  Unix.unlink temp_dir ;
  Unix.mkdir temp_dir 0o755 ;
  Fun.protect
    ~finally:(fun () -> try Unix.rmdir temp_dir with _ -> ())
    (fun () ->
      let result = ZP.verify_params_in_dir temp_dir in
      check bool "empty directory returns false" false result)

(******************************************************************************)
(*                    TEST SUITE                                              *)
(******************************************************************************)

let () =
  run
    "Zcash Parameters"
    [
      ( "metadata",
        [
          test_case "required params count" `Quick test_required_params_count;
          test_case "required params names" `Quick test_required_params_names;
          test_case "checksums format" `Quick test_checksums_format;
        ] );
      ( "search paths",
        [
          test_case "search paths generation" `Quick test_search_paths;
          test_case
            "trailing slash handling"
            `Quick
            test_search_paths_trailing_slash;
          test_case "path coverage" `Quick test_search_paths_coverage;
        ] );
      ( "download",
        [
          test_case "base URL" `Quick test_download_base_url;
          test_case "URL trailing slash" `Quick test_download_url_trailing_slash;
        ] );
      ( "verification",
        [
          test_case "non-existent directory" `Quick test_verify_nonexistent_dir;
          test_case "empty directory" `Quick test_verify_empty_dir;
        ] );
    ]
