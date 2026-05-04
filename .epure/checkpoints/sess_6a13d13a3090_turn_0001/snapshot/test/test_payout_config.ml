(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Octez_manager_rewards

let baker_pkh = "tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU"

let valid_addr = "tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU"

let valid_kt = "KT1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU"

let default () = Payout_config.default ~baker_pkh

(* {1 Default values} *)

let test_default_values () =
  let c = default () in
  Alcotest.(check bool) "overdelegation default" true c.overdelegation_protect ;
  Alcotest.(check bool) "ignore_contracts default" false c.ignore_contracts ;
  Alcotest.(check int) "version" 1 c.version ;
  Alcotest.(check string) "baker_pkh" baker_pkh c.baker_pkh

let test_default_validates () =
  let c = default () in
  match Payout_config.validate c with
  | Ok () -> ()
  | Error msg ->
      Alcotest.fail (Printf.sprintf "default should validate: %s" msg)

(* {1 Baker fee validation} *)

let test_fee_valid_range () =
  let c = {(default ()) with baker_fee = 0.0} in
  Alcotest.(check bool) "fee 0.0" true (Result.is_ok (Payout_config.validate c)) ;
  let c = {(default ()) with baker_fee = 1.0} in
  Alcotest.(check bool) "fee 1.0" true (Result.is_ok (Payout_config.validate c)) ;
  let c = {(default ()) with baker_fee = 0.5} in
  Alcotest.(check bool) "fee 0.5" true (Result.is_ok (Payout_config.validate c))

let test_fee_invalid_range () =
  let c = {(default ()) with baker_fee = -0.01} in
  Alcotest.(check bool)
    "fee -0.01"
    true
    (Result.is_error (Payout_config.validate c)) ;
  let c = {(default ()) with baker_fee = 1.01} in
  Alcotest.(check bool)
    "fee 1.01"
    true
    (Result.is_error (Payout_config.validate c))

(* {1 Min payout/balance validation} *)

let test_min_payout_nonneg () =
  let c = {(default ()) with min_payout = 0L} in
  Alcotest.(check bool)
    "min_payout 0"
    true
    (Result.is_ok (Payout_config.validate c)) ;
  let c = {(default ()) with min_payout = 1000L} in
  Alcotest.(check bool)
    "min_payout 1000"
    true
    (Result.is_ok (Payout_config.validate c)) ;
  let c = {(default ()) with min_payout = -1L} in
  Alcotest.(check bool)
    "min_payout -1"
    true
    (Result.is_error (Payout_config.validate c))

let test_min_balance_nonneg () =
  let c = {(default ()) with min_balance = 0L} in
  Alcotest.(check bool)
    "min_balance 0"
    true
    (Result.is_ok (Payout_config.validate c)) ;
  let c = {(default ()) with min_balance = -1L} in
  Alcotest.(check bool)
    "min_balance -1"
    true
    (Result.is_error (Payout_config.validate c))

(* {1 Buffer validation} *)

let test_buffers_positive () =
  let c = {(default ()) with sim_batch_size = 0} in
  Alcotest.(check bool)
    "sim_batch_size 0"
    true
    (Result.is_error (Payout_config.validate c))

(* {1 Address validation in lists} *)

let test_whitelist_valid_addresses () =
  let c = {(default ()) with whitelist = [valid_addr]} in
  Alcotest.(check bool)
    "valid tz addr"
    true
    (Result.is_ok (Payout_config.validate c)) ;
  let c = {(default ()) with whitelist = [valid_kt]} in
  Alcotest.(check bool)
    "valid KT addr"
    true
    (Result.is_ok (Payout_config.validate c)) ;
  let c = {(default ()) with whitelist = ["bad_address"]} in
  Alcotest.(check bool)
    "invalid addr"
    true
    (Result.is_error (Payout_config.validate c))

let test_blacklist_valid_addresses () =
  let c = {(default ()) with blacklist = ["not_an_address"]} in
  Alcotest.(check bool)
    "invalid addr"
    true
    (Result.is_error (Payout_config.validate c))

(* {1 Income share validation} *)

let test_bond_recipients_sum () =
  let c =
    {(default ()) with bond_recipients = [(valid_addr, 0.5); (valid_addr, 0.5)]}
  in
  Alcotest.(check bool) "sum=1.0" true (Result.is_ok (Payout_config.validate c)) ;
  let c =
    {(default ()) with bond_recipients = [(valid_addr, 0.6); (valid_addr, 0.5)]}
  in
  Alcotest.(check bool)
    "sum>1.0"
    true
    (Result.is_error (Payout_config.validate c))

let test_fee_recipients_sum () =
  let c = {(default ()) with fee_recipients = [(valid_addr, 1.1)]} in
  Alcotest.(check bool)
    "share>1.0"
    true
    (Result.is_error (Payout_config.validate c)) ;
  let c = {(default ()) with fee_recipients = [(valid_addr, -0.1)]} in
  Alcotest.(check bool)
    "share<0"
    true
    (Result.is_error (Payout_config.validate c))

(* {1 PKH validators} *)

let test_is_valid_tz_address () =
  Alcotest.(check bool)
    "tz1 valid"
    true
    (Payout_config.is_valid_tz_address "tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU") ;
  Alcotest.(check bool)
    "KT1 invalid for is_valid_tz_address"
    false
    (Payout_config.is_valid_tz_address "KT1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU") ;
  Alcotest.(check bool)
    "empty invalid"
    false
    (Payout_config.is_valid_tz_address "")

let test_is_valid_baker_pkh_accepted () =
  Alcotest.(check bool)
    "tz1 accepted"
    true
    (Payout_config.is_valid_baker_pkh "tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU") ;
  Alcotest.(check bool)
    "tz2 accepted"
    true
    (Payout_config.is_valid_baker_pkh "tz2AaBbCcDdEeFfGgHhJjKkMmNnPpQqRrSsT") ;
  Alcotest.(check bool)
    "tz3 accepted"
    true
    (Payout_config.is_valid_baker_pkh "tz3AaBbCcDdEeFfGgHhJjKkMmNnPpQqRrSsT") ;
  Alcotest.(check bool)
    "tz4 accepted"
    true
    (Payout_config.is_valid_baker_pkh "tz4AaBbCcDdEeFfGgHhJjKkMmNnPpQqRrSsT")

let test_is_valid_baker_pkh_rejected () =
  Alcotest.(check bool)
    "KT1 rejected"
    false
    (Payout_config.is_valid_baker_pkh "KT1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU") ;
  Alcotest.(check bool)
    "empty rejected"
    false
    (Payout_config.is_valid_baker_pkh "") ;
  Alcotest.(check bool)
    "tz5 rejected"
    false
    (Payout_config.is_valid_baker_pkh "tz5Ke2h7sDdakHJQh8WX4Z372du1KChsksyU") ;
  Alcotest.(check bool)
    "tz0 rejected"
    false
    (Payout_config.is_valid_baker_pkh "tz0Ke2h7sDdakHJQh8WX4Z372du1KChsksyU") ;
  Alcotest.(check bool)
    "xyz prefix rejected"
    false
    (Payout_config.is_valid_baker_pkh "xyz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU") ;
  Alcotest.(check bool)
    "too-short tz1 prefix-only rejected"
    false
    (Payout_config.is_valid_baker_pkh "tz1")

(* {1 JSON round-trip} *)

let test_json_roundtrip () =
  let c = default () in
  let json = Payout_config.to_json c in
  match Payout_config.of_json json with
  | Ok c2 ->
      Alcotest.(check string) "baker_pkh roundtrip" c.baker_pkh c2.baker_pkh ;
      Alcotest.(check bool)
        "overdelegation roundtrip"
        c.overdelegation_protect
        c2.overdelegation_protect
  | Error msg -> Alcotest.fail (Printf.sprintf "roundtrip failed: %s" msg)

let test_json_roundtrip_with_overrides () =
  let c =
    {
      (default ()) with
      delegator_overrides =
        [
          ( valid_addr,
            {
              Rewards.redirect_to = Some valid_kt;
              custom_fee = Some 0.02;
              custom_min_balance = None;
              max_balance_cap = Some 100_000_000L;
            } );
        ];
    }
  in
  let json = Payout_config.to_json c in
  match Payout_config.of_json json with
  | Ok c2 ->
      Alcotest.(check int)
        "overrides count"
        1
        (List.length c2.delegator_overrides) ;
      let _, ov = List.hd c2.delegator_overrides in
      Alcotest.(check (option string)) "redirect" (Some valid_kt) ov.redirect_to ;
      Alcotest.(check bool) "custom_fee" true (Option.is_some ov.custom_fee)
  | Error msg ->
      Alcotest.fail (Printf.sprintf "roundtrip with overrides failed: %s" msg)

(* {1 Test runner} *)

let () =
  Alcotest.run
    "payout_config"
    [
      ( "defaults",
        [
          Alcotest.test_case "default values" `Quick test_default_values;
          Alcotest.test_case "default validates" `Quick test_default_validates;
        ] );
      ( "baker_fee",
        [
          Alcotest.test_case "valid range" `Quick test_fee_valid_range;
          Alcotest.test_case "invalid range" `Quick test_fee_invalid_range;
        ] );
      ( "min_amounts",
        [
          Alcotest.test_case "min_payout nonneg" `Quick test_min_payout_nonneg;
          Alcotest.test_case "min_balance nonneg" `Quick test_min_balance_nonneg;
        ] );
      ( "buffers",
        [Alcotest.test_case "positive buffers" `Quick test_buffers_positive] );
      ( "addresses",
        [
          Alcotest.test_case
            "whitelist validation"
            `Quick
            test_whitelist_valid_addresses;
          Alcotest.test_case
            "blacklist validation"
            `Quick
            test_blacklist_valid_addresses;
        ] );
      ( "income_shares",
        [
          Alcotest.test_case
            "bond recipients sum"
            `Quick
            test_bond_recipients_sum;
          Alcotest.test_case "fee recipients sum" `Quick test_fee_recipients_sum;
        ] );
      ( "json",
        [
          Alcotest.test_case "roundtrip" `Quick test_json_roundtrip;
          Alcotest.test_case
            "roundtrip with overrides"
            `Quick
            test_json_roundtrip_with_overrides;
        ] );
      ( "pkh_validators",
        [
          Alcotest.test_case
            "is_valid_tz_address"
            `Quick
            test_is_valid_tz_address;
          Alcotest.test_case
            "is_valid_baker_pkh accepted"
            `Quick
            test_is_valid_baker_pkh_accepted;
          Alcotest.test_case
            "is_valid_baker_pkh rejected"
            `Quick
            test_is_valid_baker_pkh_rejected;
        ] );
    ]
