(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

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

(** Sum the four reward-category sub-fields that TzKT provides for each
    reward type (Delegated / StakedOwn / StakedEdge / StakedShared). *)
let sum_reward_fields json prefix =
  List.fold_left
    (fun acc suffix ->
      Int64.add acc (int64_of_json_field json (prefix ^ suffix)))
    0L
    ["Delegated"; "StakedOwn"; "StakedEdge"; "StakedShared"]

let parse_cycle_rewards ~baker json =
  let open Yojson.Safe.Util in
  let delegators =
    match member "delegators" json with
    | `List items -> List.map parse_delegator_snapshot items
    | _ -> []
  in
  let num_delegators =
    match member "delegatorsCount" json with
    | `Int n -> n
    | _ -> List.length delegators
  in
  let block_rewards = sum_reward_fields json "blockRewards" in
  (* Attestation rewards — TzKT provides both attestation* and endorsement*
     fields with identical values for backwards compatibility.
     Use attestation* (the canonical name since Oxford). *)
  let attestation_rewards = sum_reward_fields json "attestationRewards" in
  let dal_rewards = sum_reward_fields json "dalAttestationRewards" in
  let vdf_rewards = sum_reward_fields json "vdfRevelationRewards" in
  let nonce_rewards = sum_reward_fields json "nonceRevelationRewards" in
  let other_rewards =
    List.fold_left Int64.add 0L [dal_rewards; vdf_rewards; nonce_rewards]
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
    block_rewards;
    attestation_rewards;
    other_rewards;
    block_fees = int64_of_json_field json "blockFees";
    num_delegators;
    delegators;
  }

let page_size = 10000

(** Fetch additional delegator pages from the split endpoint.
    Returns the concatenation of all delegator snapshots from
    [offset] onward. Stops when a page returns fewer than [page_size]
    entries or on error (best-effort). *)
let fetch_remaining_delegators ~network ~preferred_base ~baker ~cycle offset =
  let rec go off acc =
    let path =
      Printf.sprintf
        "/v1/rewards/split/%s/%d?limit=%d&offset=%d"
        baker
        cycle
        page_size
        off
    in
    match Indexer.fetch ~network ?preferred_base path with
    | Error _ -> acc
    | Ok body -> (
        match Yojson.Safe.from_string body with
        | json ->
            let open Yojson.Safe.Util in
            let more =
              match member "delegators" json with
              | `List items -> List.map parse_delegator_snapshot items
              | _ -> []
            in
            if List.length more < page_size then acc @ more
            else go (off + page_size) (acc @ more)
        | exception _ -> acc)
  in
  go offset []

let fetch_cycle ~network ~preferred_base ~baker ~cycle =
  let path =
    Printf.sprintf "/v1/rewards/split/%s/%d?limit=%d" baker cycle page_size
  in
  match Indexer.fetch ~network ?preferred_base path with
  | Error (`Msg msg) -> Error (Printf.sprintf "TzKT fetch failed: %s" msg)
  | Ok body -> (
      match Yojson.Safe.from_string body with
      | json ->
          let cr = parse_cycle_rewards ~baker json in
          if cr.num_delegators <= page_size then Ok cr
          else
            let more =
              fetch_remaining_delegators
                ~network
                ~preferred_base
                ~baker
                ~cycle
                page_size
            in
            Ok {cr with delegators = cr.delegators @ more}
      | exception Yojson.Json_error msg ->
          Error (Printf.sprintf "JSON parse error: %s" msg))

let fetch_recent_cycles ~network ~preferred_base ~baker ~limit =
  let path =
    Printf.sprintf "/v1/rewards/bakers/%s?limit=%d&sort.desc=cycle" baker limit
  in
  match Indexer.fetch ~network ?preferred_base path with
  | Error (`Msg msg) -> Error (Printf.sprintf "TzKT fetch failed: %s" msg)
  | Ok body -> (
      match Yojson.Safe.from_string body with
      | `List items -> Ok (List.map (parse_cycle_rewards ~baker) items)
      | _ -> Error "expected JSON array from TzKT"
      | exception Yojson.Json_error msg ->
          Error (Printf.sprintf "JSON parse error: %s" msg))

let fetch_current_cycle ~network ~preferred_base =
  match Indexer.fetch ~network ?preferred_base "/v1/head" with
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
