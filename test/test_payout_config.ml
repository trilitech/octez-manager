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

let default () = Payout_config.default ~baker_pkh ()

(* {1 Default values} *)

let test_default_values () =
  let c = default () in
  Alcotest.(check bool) "overdelegation default" true c.overdelegation_protect ;
  Alcotest.(check bool) "baker_pays_tx default" false c.baker_pays_tx_fee ;
  Alcotest.(check bool) "baker_pays_alloc default" false c.baker_pays_alloc_fee ;
  Alcotest.(check bool) "ignore_contracts default" false c.ignore_contracts ;
  Alcotest.(check int) "version" 1 c.version ;
  Alcotest.(check string) "baker_pkh" baker_pkh c.baker_pkh ;
  Alcotest.(check string)
    "payout_mode"
    "actual"
    (Rewards.string_of_payout_mode c.payout_mode)

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
  let c = {(default ()) with gas_buffer = 0} in
  Alcotest.(check bool)
    "gas_buffer 0"
    true
    (Result.is_error (Payout_config.validate c)) ;
  let c = {(default ()) with kt_gas_buffer = 0} in
  Alcotest.(check bool)
    "kt_gas_buffer 0"
    true
    (Result.is_error (Payout_config.validate c)) ;
  let c = {(default ()) with sim_batch_size = 0} in
  Alcotest.(check bool)
    "sim_batch_size 0"
    true
    (Result.is_error (Payout_config.validate c))

(* {1 Delay blocks validation} *)

let test_delay_blocks () =
  let c = {(default ()) with min_delay_blocks = 0; max_delay_blocks = 0} in
  Alcotest.(check bool)
    "min=0 max=0"
    true
    (Result.is_ok (Payout_config.validate c)) ;
  let c = {(default ()) with min_delay_blocks = 5; max_delay_blocks = 3} in
  Alcotest.(check bool)
    "min>max"
    true
    (Result.is_error (Payout_config.validate c)) ;
  let c = {(default ()) with min_delay_blocks = -1} in
  Alcotest.(check bool)
    "min_delay -1"
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

(* {1 effective_tzkt_url} *)

let test_effective_tzkt_url_mainnet_on_testnet () =
  (* Config saved with mainnet default but baker is on tallinnnet *)
  let c = {(default ()) with tzkt_url = "https://api.tzkt.io"} in
  let url =
    Payout_config.effective_tzkt_url
      ~network:"https://teztnets.com/tallinnnet"
      c
  in
  Alcotest.(check string)
    "overrides to tallinnnet"
    "https://api.tallinnnet.tzkt.io"
    url

let test_effective_tzkt_url_mainnet_on_mainnet () =
  let c = {(default ()) with tzkt_url = "https://api.tzkt.io"} in
  let url = Payout_config.effective_tzkt_url ~network:"mainnet" c in
  Alcotest.(check string) "keeps mainnet" "https://api.tzkt.io" url

let test_effective_tzkt_url_custom () =
  (* User explicitly configured a custom TzKT instance *)
  let c = {(default ()) with tzkt_url = "http://my-local-tzkt:5000"} in
  let url =
    Payout_config.effective_tzkt_url
      ~network:"https://teztnets.com/tallinnnet"
      c
  in
  Alcotest.(check string) "preserves custom" "http://my-local-tzkt:5000" url

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
        c2.overdelegation_protect ;
      Alcotest.(check string)
        "payout_mode roundtrip"
        (Rewards.string_of_payout_mode c.payout_mode)
        (Rewards.string_of_payout_mode c2.payout_mode)
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
              baker_pays_tx_fee = Some true;
              baker_pays_alloc_fee = None;
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
      ( "delay_blocks",
        [Alcotest.test_case "delay constraints" `Quick test_delay_blocks] );
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
      ( "effective_tzkt_url",
        [
          Alcotest.test_case
            "mainnet default on testnet"
            `Quick
            test_effective_tzkt_url_mainnet_on_testnet;
          Alcotest.test_case
            "mainnet default on mainnet"
            `Quick
            test_effective_tzkt_url_mainnet_on_mainnet;
          Alcotest.test_case
            "custom url preserved"
            `Quick
            test_effective_tzkt_url_custom;
        ] );
      ( "json",
        [
          Alcotest.test_case "roundtrip" `Quick test_json_roundtrip;
          Alcotest.test_case
            "roundtrip with overrides"
            `Quick
            test_json_roundtrip_with_overrides;
        ] );
    ]
