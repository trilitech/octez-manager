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
    - key_kind detection from locator URIs
    - read_keys_full cross-referencing of all three key files
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
(* key_kind_of_locator Tests *)
(* ============================================================ *)

let key_kind_testable =
  let pp fmt = function
    | Keys_reader.Unencrypted -> Format.fprintf fmt "Unencrypted"
    | Keys_reader.Encrypted -> Format.fprintf fmt "Encrypted"
    | Keys_reader.Ledger s -> Format.fprintf fmt "Ledger(%s)" s
    | Keys_reader.Remote s -> Format.fprintf fmt "Remote(%s)" s
  in
  let eq a b =
    match (a, b) with
    | Keys_reader.Unencrypted, Keys_reader.Unencrypted -> true
    | Keys_reader.Encrypted, Keys_reader.Encrypted -> true
    | Keys_reader.Ledger a, Keys_reader.Ledger b -> String.equal a b
    | Keys_reader.Remote a, Keys_reader.Remote b -> String.equal a b
    | _ -> false
  in
  testable pp eq

let test_locator_unencrypted () =
  check
    key_kind_testable
    "unencrypted secret key"
    Unencrypted
    (KR.key_kind_of_locator "unencrypted:edsk3abc123")

let test_locator_encrypted () =
  check
    key_kind_testable
    "encrypted secret key"
    Encrypted
    (KR.key_kind_of_locator "encrypted:edesk1abc123")

let test_locator_ledger () =
  check
    key_kind_testable
    "ledger key"
    (Ledger "major-animal/ed25519/0h/0h")
    (KR.key_kind_of_locator "ledger://major-animal/ed25519/0h/0h")

let test_locator_ledger_triple_slash () =
  (* ledger:/// has an extra / that becomes part of the payload *)
  check
    key_kind_testable
    "ledger with triple slash"
    (Ledger "/major-animal/ed25519/0h/0h")
    (KR.key_kind_of_locator "ledger:///major-animal/ed25519/0h/0h")

let test_locator_remote () =
  check
    key_kind_testable
    "remote signer"
    (Remote "localhost:6732/tz1abc")
    (KR.key_kind_of_locator "tcp://localhost:6732/tz1abc")

let test_locator_empty () =
  check
    key_kind_testable
    "empty locator defaults to unencrypted"
    Unencrypted
    (KR.key_kind_of_locator "")

let test_locator_unknown_scheme () =
  check
    key_kind_testable
    "unknown scheme defaults to unencrypted"
    Unencrypted
    (KR.key_kind_of_locator "some_unknown_scheme:data")

(* ============================================================ *)
(* read_keys_full Tests (filesystem-based) *)
(* ============================================================ *)

(** Create a temporary directory with key files for testing. *)
let with_temp_base_dir f =
  let tmp = Filename.temp_dir "keys_reader_test" "" in
  Fun.protect ~finally:(fun () ->
      (* Clean up *)
      let files = Sys.readdir tmp in
      Array.iter (fun name -> Sys.remove (Filename.concat tmp name)) files ;
      Sys.rmdir tmp)
  @@ fun () -> f tmp

let write_json_file dir name json =
  let path = Filename.concat dir name in
  let oc = open_out path in
  Fun.protect ~finally:(fun () -> close_out oc) @@ fun () ->
  output_string oc (Yojson.Safe.to_string json)

let test_read_keys_full_basic () =
  with_temp_base_dir @@ fun base_dir ->
  (* public_key_hashs *)
  write_json_file
    base_dir
    "public_key_hashs"
    (`List
       [
         `Assoc
           [
             ("name", `String "baker1");
             ("value", `String "tz1RindtHBSbhPady1TBLmKi7CkG4o8PBoEfh");
           ];
       ]) ;
  (* public_keys *)
  write_json_file
    base_dir
    "public_keys"
    (`List
       [
         `Assoc
           [
             ("name", `String "baker1");
             ( "value",
               `Assoc
                 [
                   ("locator", `String "unencrypted:edpkABC");
                   ("key", `String "edpkABC123");
                 ] );
           ];
       ]) ;
  (* secret_keys *)
  write_json_file
    base_dir
    "secret_keys"
    (`List
       [
         `Assoc
           [
             ("name", `String "baker1");
             ("value", `String "unencrypted:edsk3XYZ");
           ];
       ]) ;
  match Keys_reader.read_keys_full ~base_dir with
  | Error (`Msg msg) -> fail (Printf.sprintf "should succeed: %s" msg)
  | Ok keys ->
      check int "one key" 1 (List.length keys) ;
      let k = List.hd keys in
      check string "alias" "baker1" k.alias ;
      check string "pkh" "tz1RindtHBSbhPady1TBLmKi7CkG4o8PBoEfh" k.pkh ;
      check (option string) "public key" (Some "edpkABC123") k.public_key ;
      check key_kind_testable "key_kind" Unencrypted k.key_kind ;
      check bool "has_secret_key" true k.has_secret_key

let test_read_keys_full_encrypted () =
  with_temp_base_dir @@ fun base_dir ->
  write_json_file
    base_dir
    "public_key_hashs"
    (`List
       [
         `Assoc
           [
             ("name", `String "enc_key");
             ("value", `String "tz1enc123456789012345678901234567890");
           ];
       ]) ;
  write_json_file
    base_dir
    "secret_keys"
    (`List
       [
         `Assoc
           [
             ("name", `String "enc_key");
             ("value", `String "encrypted:edesk1ABC");
           ];
       ]) ;
  match Keys_reader.read_keys_full ~base_dir with
  | Error (`Msg msg) -> fail (Printf.sprintf "should succeed: %s" msg)
  | Ok keys ->
      check int "one key" 1 (List.length keys) ;
      let k = List.hd keys in
      check key_kind_testable "encrypted" Encrypted k.key_kind ;
      check bool "has_secret_key" true k.has_secret_key

let test_read_keys_full_ledger () =
  with_temp_base_dir @@ fun base_dir ->
  write_json_file
    base_dir
    "public_key_hashs"
    (`List
       [
         `Assoc
           [
             ("name", `String "ledger_key");
             ("value", `String "tz1led123456789012345678901234567890");
           ];
       ]) ;
  write_json_file
    base_dir
    "public_keys"
    (`List
       [
         `Assoc
           [
             ("name", `String "ledger_key");
             ( "value",
               `Assoc
                 [
                   ("locator", `String "ledger://major-animal/ed25519/0h/0h");
                   ("key", `String "edpkLedger123");
                 ] );
           ];
       ]) ;
  (* No secret_keys entry for ledger — key_kind comes from public_keys *)
  match Keys_reader.read_keys_full ~base_dir with
  | Error (`Msg msg) -> fail (Printf.sprintf "should succeed: %s" msg)
  | Ok keys ->
      check int "one key" 1 (List.length keys) ;
      let k = List.hd keys in
      check
        key_kind_testable
        "ledger"
        (Ledger "major-animal/ed25519/0h/0h")
        k.key_kind ;
      check bool "no secret key" false k.has_secret_key ;
      check (option string) "has public key" (Some "edpkLedger123") k.public_key

let test_read_keys_full_watch_only () =
  with_temp_base_dir @@ fun base_dir ->
  (* Only public_key_hashs, no other files *)
  write_json_file
    base_dir
    "public_key_hashs"
    (`List
       [
         `Assoc
           [
             ("name", `String "watch_key");
             ("value", `String "tz1wat123456789012345678901234567890");
           ];
       ]) ;
  match Keys_reader.read_keys_full ~base_dir with
  | Error (`Msg msg) -> fail (Printf.sprintf "should succeed: %s" msg)
  | Ok keys ->
      check int "one key" 1 (List.length keys) ;
      let k = List.hd keys in
      check key_kind_testable "unencrypted (watch-only)" Unencrypted k.key_kind ;
      check bool "no secret key" false k.has_secret_key ;
      check (option string) "no public key" None k.public_key

let test_read_keys_full_empty_dir () =
  with_temp_base_dir @@ fun base_dir ->
  (* No key files at all *)
  match Keys_reader.read_keys_full ~base_dir with
  | Error (`Msg msg) -> fail (Printf.sprintf "should succeed: %s" msg)
  | Ok keys -> check int "no keys" 0 (List.length keys)

let test_read_keys_full_multiple_keys () =
  with_temp_base_dir @@ fun base_dir ->
  write_json_file
    base_dir
    "public_key_hashs"
    (`List
       [
         `Assoc
           [
             ("name", `String "key_a");
             ("value", `String "tz1aaa123456789012345678901234567890");
           ];
         `Assoc
           [
             ("name", `String "key_b");
             ("value", `String "tz2bbb123456789012345678901234567890");
           ];
         `Assoc
           [
             ("name", `String "key_c");
             ("value", `String "tz3ccc123456789012345678901234567890");
           ];
       ]) ;
  write_json_file
    base_dir
    "secret_keys"
    (`List
       [
         `Assoc
           [("name", `String "key_a"); ("value", `String "unencrypted:edskAAA")];
         `Assoc
           [("name", `String "key_b"); ("value", `String "encrypted:edeskBBB")];
         (* key_c has no secret key entry *)
       ]) ;
  match Keys_reader.read_keys_full ~base_dir with
  | Error (`Msg msg) -> fail (Printf.sprintf "should succeed: %s" msg)
  | Ok keys ->
      check int "three keys" 3 (List.length keys) ;
      let find_key alias =
        List.find
          (fun (k : Keys_reader.key_metadata) -> String.equal k.alias alias)
          keys
      in
      let ka = find_key "key_a" in
      check key_kind_testable "key_a unencrypted" Unencrypted ka.key_kind ;
      check bool "key_a has secret" true ka.has_secret_key ;
      let kb = find_key "key_b" in
      check key_kind_testable "key_b encrypted" Encrypted kb.key_kind ;
      check bool "key_b has secret" true kb.has_secret_key ;
      let kc = find_key "key_c" in
      check
        key_kind_testable
        "key_c unencrypted (watch-only)"
        Unencrypted
        kc.key_kind ;
      check bool "key_c no secret" false kc.has_secret_key

let test_read_keys_full_remote () =
  with_temp_base_dir @@ fun base_dir ->
  write_json_file
    base_dir
    "public_key_hashs"
    (`List
       [
         `Assoc
           [
             ("name", `String "remote_key");
             ("value", `String "tz1rem123456789012345678901234567890");
           ];
       ]) ;
  write_json_file
    base_dir
    "secret_keys"
    (`List
       [
         `Assoc
           [
             ("name", `String "remote_key");
             ("value", `String "tcp://localhost:6732/tz1rem");
           ];
       ]) ;
  match Keys_reader.read_keys_full ~base_dir with
  | Error (`Msg msg) -> fail (Printf.sprintf "should succeed: %s" msg)
  | Ok keys ->
      check int "one key" 1 (List.length keys) ;
      let k = List.hd keys in
      check
        key_kind_testable
        "remote"
        (Remote "localhost:6732/tz1rem")
        k.key_kind ;
      check bool "has secret key" true k.has_secret_key

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

let locator_tests =
  [
    ("unencrypted locator", `Quick, test_locator_unencrypted);
    ("encrypted locator", `Quick, test_locator_encrypted);
    ("ledger locator", `Quick, test_locator_ledger);
    ("ledger triple-slash", `Quick, test_locator_ledger_triple_slash);
    ("remote locator", `Quick, test_locator_remote);
    ("empty locator", `Quick, test_locator_empty);
    ("unknown scheme", `Quick, test_locator_unknown_scheme);
  ]

let read_keys_full_tests =
  [
    ("basic key with all files", `Quick, test_read_keys_full_basic);
    ("encrypted key", `Quick, test_read_keys_full_encrypted);
    ("ledger key", `Quick, test_read_keys_full_ledger);
    ("watch-only key", `Quick, test_read_keys_full_watch_only);
    ("empty directory", `Quick, test_read_keys_full_empty_dir);
    ("multiple keys mixed", `Quick, test_read_keys_full_multiple_keys);
    ("remote signer key", `Quick, test_read_keys_full_remote);
  ]

let () =
  Alcotest.run
    "Keys_reader"
    [
      ("key_info_parsing", key_info_tests);
      ("key_kind_of_locator", locator_tests);
      ("read_keys_full", read_keys_full_tests);
    ]
