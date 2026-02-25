(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

let curl_fetch url =
  Cmd_runner.run_out_silent ["curl"; "-fsSL"; "--max-time"; "15"; url]

let int64_of_json_field json field =
  let open Yojson.Safe.Util in
  match member field json with
  | `Int i -> Int64.of_int i
  | `Intlit s -> Int64.of_string s
  | `String s -> Int64.of_string s
  | _ -> 0L

let parse_delegator_snapshot json =
  let open Yojson.Safe.Util in
  {
    Rewards.address = member "address" json |> to_string;
    delegated_balance = int64_of_json_field json "delegatedBalance";
    staked_balance = int64_of_json_field json "stakedBalance";
  }

let parse_cycle_rewards ~baker json =
  let open Yojson.Safe.Util in
  let delegators =
    match member "delegators" json with
    | `List items -> List.map parse_delegator_snapshot items
    | _ -> []
  in
  {
    Rewards.cycle = member "cycle" json |> to_int;
    baker;
    staking_balance = int64_of_json_field json "stakingBalance";
    delegated_balance = int64_of_json_field json "delegatedBalance";
    own_staked_balance = int64_of_json_field json "ownStakedBalance";
    own_delegated_balance = int64_of_json_field json "ownDelegatedBalance";
    external_staked_balance = int64_of_json_field json "externalStakedBalance";
    external_delegated_balance =
      int64_of_json_field json "externalDelegatedBalance";
    block_rewards = int64_of_json_field json "blockRewards";
    block_fees = int64_of_json_field json "blockFees";
    delegators;
  }

let fetch_cycle ~tzkt_url ~baker ~cycle =
  let url =
    Printf.sprintf
      "%s/v1/rewards/bakers/%s/%d?delegators=true"
      tzkt_url
      baker
      cycle
  in
  match curl_fetch url with
  | Error (`Msg msg) -> Error (Printf.sprintf "TzKT fetch failed: %s" msg)
  | Ok body -> (
      match Yojson.Safe.from_string body with
      | json -> Ok (parse_cycle_rewards ~baker json)
      | exception Yojson.Json_error msg ->
          Error (Printf.sprintf "JSON parse error: %s" msg))

let fetch_recent_cycles ~tzkt_url ~baker ~limit =
  let url =
    Printf.sprintf
      "%s/v1/rewards/bakers/%s?limit=%d&sort.desc=cycle&delegators=true"
      tzkt_url
      baker
      limit
  in
  match curl_fetch url with
  | Error (`Msg msg) -> Error (Printf.sprintf "TzKT fetch failed: %s" msg)
  | Ok body -> (
      match Yojson.Safe.from_string body with
      | `List items -> Ok (List.map (parse_cycle_rewards ~baker) items)
      | _ -> Error "expected JSON array from TzKT"
      | exception Yojson.Json_error msg ->
          Error (Printf.sprintf "JSON parse error: %s" msg))

let fetch_current_cycle ~tzkt_url =
  let url = Printf.sprintf "%s/v1/head" tzkt_url in
  match curl_fetch url with
  | Error (`Msg msg) -> Error (Printf.sprintf "TzKT fetch failed: %s" msg)
  | Ok body -> (
      match Yojson.Safe.from_string body with
      | json -> (
          let open Yojson.Safe.Util in
          match member "cycle" json |> to_int_option with
          | Some c -> Ok c
          | None -> Error "no cycle field in /v1/head response")
      | exception Yojson.Json_error msg ->
          Error (Printf.sprintf "JSON parse error: %s" msg))
