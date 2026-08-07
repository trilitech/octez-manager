(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Alcotest
open Octez_manager_lib.Yes_wallet

let test_curve_of_address_tz1 () =
  check
    (option (testable (Fmt.of_to_string (fun _ -> "curve")) ( = )))
    "tz1 is Ed25519"
    (Some Ed25519)
    (curve_of_address "tz1RindtHBSbhPady1TBLmKi7CkG4o8PBoEfh")

let test_curve_of_address_tz2 () =
  check
    (option (testable (Fmt.of_to_string (fun _ -> "curve")) ( = )))
    "tz2 is Secp256k1"
    (Some Secp256k1)
    (curve_of_address "tz2BFTyPeYRzxd5aiBchbXN3WCZhx7BqbMR9")

let test_curve_of_address_tz3 () =
  check
    (option (testable (Fmt.of_to_string (fun _ -> "curve")) ( = )))
    "tz3 is P256"
    (Some P256)
    (curve_of_address "tz3RDC3Jdn4j15J7bBHZd29EUee9gVB1CxD9")

let test_curve_of_address_tz4 () =
  check
    (option (testable (Fmt.of_to_string (fun _ -> "curve")) ( = )))
    "tz4 is BLS"
    (Some BLS)
    (curve_of_address "tz4EECtMxAuJ9UDLaiMZH7G1fMNtWoJqvcxF")

let test_curve_of_address_invalid () =
  check
    (option (testable (Fmt.of_to_string (fun _ -> "curve")) ( = )))
    "KT1 is None"
    None
    (curve_of_address "KT1HxgqnVjGy7KsSUTEsQ6LgpD5iKSGu7QaL")

let test_curve_of_address_short () =
  check
    (option (testable (Fmt.of_to_string (fun _ -> "curve")) ( = )))
    "short string is None"
    None
    (curve_of_address "tz")

let test_keys_for_each_curve () =
  let check_key curve prefix_sk prefix_pk =
    let sk, pk = Internal_for_tests.test_keys_for_curve curve in
    check
      bool
      (Printf.sprintf "sk starts with %s" prefix_sk)
      true
      (String.starts_with ~prefix:prefix_sk sk) ;
    check
      bool
      (Printf.sprintf "pk starts with %s" prefix_pk)
      true
      (String.starts_with ~prefix:prefix_pk pk)
  in
  check_key Ed25519 "edsk" "edpk" ;
  check_key Secp256k1 "spsk" "sppk" ;
  check_key P256 "p2sk" "p2pk" ;
  check_key BLS "BLsk" "BLpk"

let sample_delegates =
  [
    {
      alias = "delegate-0";
      address = "tz1RindtHBSbhPady1TBLmKi7CkG4o8PBoEfh";
      curve = Ed25519;
    };
    {
      alias = "delegate-1";
      address = "tz2BFTyPeYRzxd5aiBchbXN3WCZhx7BqbMR9";
      curve = Secp256k1;
    };
    {
      alias = "delegate-2";
      address = "tz3RDC3Jdn4j15J7bBHZd29EUee9gVB1CxD9";
      curve = P256;
    };
  ]

let test_wallet_json_structure () =
  let pkhs, pks, sks = generate_wallet_json sample_delegates in
  (* Check pkhs is a list of 3 entries *)
  (match pkhs with
  | `List l -> check int "pkhs has 3 entries" 3 (List.length l)
  | _ -> fail "pkhs should be a JSON list") ;
  (* Check pks is a list of 3 entries *)
  (match pks with
  | `List l -> check int "pks has 3 entries" 3 (List.length l)
  | _ -> fail "pks should be a JSON list") ;
  (* Check sks is a list of 3 entries *)
  match sks with
  | `List l -> check int "sks has 3 entries" 3 (List.length l)
  | _ -> fail "sks should be a JSON list"

let test_wallet_json_pkh_values () =
  let pkhs, _, _ = generate_wallet_json sample_delegates in
  match pkhs with
  | `List (first :: _) ->
      let open Yojson.Safe.Util in
      let name = first |> member "name" |> to_string in
      let value = first |> member "value" |> to_string in
      check string "first alias" "delegate-0" name ;
      check string "first address" "tz1RindtHBSbhPady1TBLmKi7CkG4o8PBoEfh" value
  | _ -> fail "expected non-empty pkhs list"

let test_wallet_json_sk_values () =
  let _, _, sks = generate_wallet_json sample_delegates in
  match sks with
  | `List (first :: _) ->
      let open Yojson.Safe.Util in
      let value = first |> member "value" |> to_string in
      check
        bool
        "sk has unencrypted prefix"
        true
        (String.starts_with ~prefix:"unencrypted:edsk" value)
  | _ -> fail "expected non-empty sks list"

let test_wallet_json_pk_values () =
  let _, pks, _ = generate_wallet_json sample_delegates in
  match pks with
  | `List (first :: _) ->
      let open Yojson.Safe.Util in
      let value = first |> member "value" in
      let locator = value |> member "locator" |> to_string in
      let key = value |> member "key" |> to_string in
      check
        bool
        "locator has unencrypted prefix"
        true
        (String.starts_with ~prefix:"unencrypted:edpk" locator) ;
      check
        bool
        "key starts with edpk"
        true
        (String.starts_with ~prefix:"edpk" key)
  | _ -> fail "expected non-empty pks list"

let test_duplicate_handling () =
  let delegates =
    [
      {
        alias = "delegate-0";
        address = "tz1RindtHBSbhPady1TBLmKi7CkG4o8PBoEfh";
        curve = Ed25519;
      };
      {
        alias = "delegate-1";
        address = "tz1RindtHBSbhPady1TBLmKi7CkG4o8PBoEfh";
        curve = Ed25519;
      };
    ]
  in
  let pkhs, _, _ = generate_wallet_json delegates in
  match pkhs with
  | `List l -> check int "deduped to 1 entry" 1 (List.length l)
  | _ -> fail "pkhs should be a JSON list"

let test_empty_delegate_list () =
  let pkhs, pks, sks = generate_wallet_json [] in
  (match pkhs with
  | `List l -> check int "pkhs empty" 0 (List.length l)
  | _ -> fail "pkhs should be a JSON list") ;
  (match pks with
  | `List l -> check int "pks empty" 0 (List.length l)
  | _ -> fail "pks should be a JSON list") ;
  match sks with
  | `List l -> check int "sks empty" 0 (List.length l)
  | _ -> fail "sks should be a JSON list"

let () =
  run
    "Yes_wallet"
    [
      ( "curve_detection",
        [
          test_case "tz1 -> Ed25519" `Quick test_curve_of_address_tz1;
          test_case "tz2 -> Secp256k1" `Quick test_curve_of_address_tz2;
          test_case "tz3 -> P256" `Quick test_curve_of_address_tz3;
          test_case "tz4 -> BLS" `Quick test_curve_of_address_tz4;
          test_case "KT1 -> None" `Quick test_curve_of_address_invalid;
          test_case "short -> None" `Quick test_curve_of_address_short;
        ] );
      ( "test_keys",
        [test_case "keys match curve prefixes" `Quick test_keys_for_each_curve]
      );
      ( "wallet_json",
        [
          test_case "structure" `Quick test_wallet_json_structure;
          test_case "pkh values" `Quick test_wallet_json_pkh_values;
          test_case "sk values" `Quick test_wallet_json_sk_values;
          test_case "pk values" `Quick test_wallet_json_pk_values;
          test_case "duplicate handling" `Quick test_duplicate_handling;
          test_case "empty delegates" `Quick test_empty_delegate_list;
        ] );
    ]
