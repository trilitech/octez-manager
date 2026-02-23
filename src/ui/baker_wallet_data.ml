(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Baker wallet data fetching and caching.

    Fetches wallet state (balances, staking parameters, unstake requests,
    consensus key, voting info) from node RPC and caches it for use by
    the TUI wallet modal and CLI baker commands. *)

(* ── Types ─────────────────────────────────────────────────── *)

type staking_parameters = {
  limit_of_staking_over_baking : int;
  edge_of_baking_over_staking : int;
}

type finalizable_request = {cycle : int; amount : string}

type unfinalizable_request = {cycle : int; amount : string}

type unstake_requests = {
  finalizable : finalizable_request list;
  unfinalizable : unfinalizable_request list;
}

type voting_period_kind =
  | Proposal
  | Exploration
  | Cooldown
  | Promotion
  | Adoption

type ballot_vote = Yay | Nay | Pass

type voting_info = {
  period_kind : voting_period_kind;
  period_position : int;
  period_remaining : int;
  proposals : (string * int) list;
  current_proposal : string option;
  ballots : (string * string) list;
}

type t = {
  pkh : string;
  spendable_balance : string;
  staked_balance : string;
  unstaked_frozen : string;
  full_balance : string;
  is_registered : bool;
  deactivated : bool;
  active_consensus_key : string;
  pending_consensus_keys : (int * string) list;
  staking_parameters : staking_parameters option;
  pending_staking_parameters : staking_parameters option;
  unstake_requests : unstake_requests;
  fetched_at : float;
}

(* ── JSON Parsing ──────────────────────────────────────────── *)

let parse_spendable json =
  try match json with `String s -> Some s | _ -> None with _ -> None

let parse_staking_parameters json =
  let open Yojson.Safe.Util in
  try
    let limit =
      json
      |> member "limit_of_staking_over_baking_millionth"
      |> to_int_option |> Option.value ~default:0
    in
    let edge =
      json
      |> member "edge_of_baking_over_staking_billionth"
      |> to_int_option |> Option.value ~default:0
    in
    Some
      {limit_of_staking_over_baking = limit; edge_of_baking_over_staking = edge}
  with _ -> None

let parse_unstake_request_entry json =
  let open Yojson.Safe.Util in
  let cycle =
    json |> member "cycle" |> to_int_option |> Option.value ~default:0
  in
  let amount =
    json |> member "amount" |> to_string_option |> Option.value ~default:"0"
  in
  (cycle, amount)

let parse_unstake_requests json =
  let open Yojson.Safe.Util in
  try
    let finalizable =
      (try json |> member "finalizable" |> to_list with _ -> [])
      |> List.filter_map (fun entry ->
          try
            let delegate_entry = entry |> member "delegate" in
            let cycle, amount = parse_unstake_request_entry delegate_entry in
            Some ({cycle; amount} : finalizable_request)
          with _ -> None)
    in
    let unfinalizable =
      (try json |> member "unfinalizable" |> member "requests" |> to_list
       with _ -> [])
      |> List.filter_map (fun entry ->
          try
            let delegate_entry = entry |> member "delegate" in
            let cycle, amount = parse_unstake_request_entry delegate_entry in
            Some ({cycle; amount} : unfinalizable_request)
          with _ -> None)
    in
    {finalizable; unfinalizable}
  with _ -> {finalizable = []; unfinalizable = []}

let parse_pending_consensus_keys json =
  let open Yojson.Safe.Util in
  try
    json |> to_list
    |> List.filter_map (fun entry ->
        try
          let cycle =
            entry |> member "cycle" |> to_int_option |> Option.value ~default:0
          in
          let pkh =
            entry |> member "pkh" |> to_string_option
            |> Option.value ~default:""
          in
          Some (cycle, pkh)
        with _ -> None)
  with _ -> []

let try_fields json names =
  List.find_map
    (fun name ->
      try Yojson.Safe.Util.(json |> member name |> to_string_option)
      with _ -> None)
    names
  |> Option.value ~default:"0"

let parse_delegate_aggregate ~pkh json =
  let open Yojson.Safe.Util in
  try
    let deactivated =
      json |> member "deactivated" |> to_bool_option
      |> Option.value ~default:false
    in
    (* own_staked (Quebec+) or current_frozen_deposits (older protocols) *)
    let staked_balance =
      try_fields json ["own_staked"; "current_frozen_deposits"]
    in
    (* own_full_balance (Quebec+) or full_balance (older protocols) *)
    let full_balance = try_fields json ["own_full_balance"; "full_balance"] in
    (* Compute unstaked_frozen from full_balance - staked - own_delegated.
       Falls back to 0 if any value is missing or the result is negative. *)
    let unstaked_frozen =
      let own_delegated = try_fields json ["own_delegated"] in
      try
        let full = Int64.of_string full_balance in
        let staked = Int64.of_string staked_balance in
        let delegated = Int64.of_string own_delegated in
        let frozen = Int64.sub (Int64.sub full staked) delegated in
        if Int64.compare frozen 0L > 0 then Int64.to_string frozen else "0"
      with _ -> "0"
    in
    (* consensus_key.active.pkh (Quebec+) or active_consensus_key (older) *)
    let active_consensus_key =
      let from_nested =
        try
          json |> member "consensus_key" |> member "active" |> member "pkh"
          |> to_string_option
        with _ -> None
      in
      let from_flat =
        try json |> member "active_consensus_key" |> to_string_option
        with _ -> None
      in
      match from_nested with
      | Some s -> s
      | None -> ( match from_flat with Some s -> s | None -> pkh)
    in
    let pending_consensus_keys =
      try
        json |> member "pending_consensus_keys" |> parse_pending_consensus_keys
      with _ -> []
    in
    Some
      {
        pkh;
        spendable_balance = "0";
        staked_balance;
        unstaked_frozen;
        full_balance;
        is_registered = true;
        deactivated;
        active_consensus_key;
        pending_consensus_keys;
        staking_parameters = None;
        pending_staking_parameters = None;
        unstake_requests = {finalizable = []; unfinalizable = []};
        fetched_at = Unix.gettimeofday ();
      }
  with _ -> None

(* ── Voting JSON Parsing ───────────────────────────────────── *)

let voting_period_kind_of_string = function
  | "proposal" -> Some Proposal
  | "exploration" -> Some Exploration
  | "cooldown" -> Some Cooldown
  | "promotion" -> Some Promotion
  | "adoption" -> Some Adoption
  | _ -> None

let parse_voting_info ~period_json ~proposals_json ~ballot_list_json
    ~current_proposal_json =
  let open Yojson.Safe.Util in
  try
    let kind_str =
      period_json |> member "voting_period" |> member "kind" |> to_string_option
      |> Option.value ~default:"proposal"
    in
    let period_kind =
      match voting_period_kind_of_string kind_str with
      | Some k -> k
      | None -> Proposal
    in
    let period_position =
      period_json |> member "position" |> to_int_option
      |> Option.value ~default:0
    in
    let period_remaining =
      period_json |> member "remaining" |> to_int_option
      |> Option.value ~default:0
    in
    let proposals =
      try
        proposals_json |> to_list
        |> List.filter_map (fun entry ->
            try
              match entry |> to_list with
              | [hash_json; count_json] ->
                  let hash =
                    hash_json |> to_string_option |> Option.value ~default:""
                  in
                  let count =
                    count_json |> to_int_option |> Option.value ~default:0
                  in
                  Some (hash, count)
              | _ -> None
            with _ -> None)
      with _ -> []
    in
    let current_proposal =
      try current_proposal_json |> to_string_option with _ -> None
    in
    let ballots =
      try
        ballot_list_json |> to_list
        |> List.filter_map (fun entry ->
            try
              let p =
                entry |> member "pkh" |> to_string_option
                |> Option.value ~default:""
              in
              let b =
                entry |> member "ballot" |> to_string_option
                |> Option.value ~default:""
              in
              Some (p, b)
            with _ -> None)
      with _ -> []
    in
    Some
      {
        period_kind;
        period_position;
        period_remaining;
        proposals;
        current_proposal;
        ballots;
      }
  with _ -> None

(* ── RPC Fetching ──────────────────────────────────────────── *)

let rpc_get ~node_endpoint path =
  let url = Printf.sprintf "%s%s" node_endpoint path in
  match Cmd_runner.run_out_silent ["curl"; "-sfL"; "--max-time"; "10"; url] with
  | Error _ -> None
  | Ok body -> ( try Some (Yojson.Safe.from_string body) with _ -> None)

let fetch_wallet_data ~node_endpoint ~pkh =
  let block_prefix = "/chains/main/blocks/head~2" in
  (* 1. Fetch spendable balance (tests node reachability) *)
  let spendable_path =
    Printf.sprintf "%s/context/contracts/%s/spendable" block_prefix pkh
  in
  match rpc_get ~node_endpoint spendable_path with
  | None -> None
  | Some spendable_json -> (
      let spendable =
        match parse_spendable spendable_json with Some s -> s | None -> "0"
      in
      (* 2. Fetch delegate aggregate (if 404 → not registered) *)
      let delegate_path =
        Printf.sprintf "%s/context/delegates/%s" block_prefix pkh
      in
      match rpc_get ~node_endpoint delegate_path with
      | None ->
          (* Not registered or node error *)
          Some
            {
              pkh;
              spendable_balance = spendable;
              staked_balance = "0";
              unstaked_frozen = "0";
              full_balance = spendable;
              is_registered = false;
              deactivated = false;
              active_consensus_key = pkh;
              pending_consensus_keys = [];
              staking_parameters = None;
              pending_staking_parameters = None;
              unstake_requests = {finalizable = []; unfinalizable = []};
              fetched_at = Unix.gettimeofday ();
            }
      | Some delegate_json -> (
          match parse_delegate_aggregate ~pkh delegate_json with
          | None -> None
          | Some data ->
              let data = {data with spendable_balance = spendable} in
              (* 3. Fetch active staking parameters *)
              let active_params_path =
                Printf.sprintf
                  "%s/context/delegates/%s/active_staking_parameters"
                  block_prefix
                  pkh
              in
              let active_params =
                match rpc_get ~node_endpoint active_params_path with
                | Some json -> parse_staking_parameters json
                | None -> None
              in
              (* 4. Fetch pending staking parameters *)
              let pending_params_path =
                Printf.sprintf
                  "%s/context/delegates/%s/pending_staking_parameters"
                  block_prefix
                  pkh
              in
              let pending_params =
                match rpc_get ~node_endpoint pending_params_path with
                | Some json -> parse_staking_parameters json
                | None -> None
              in
              (* 5. Fetch unstake requests *)
              let unstake_path =
                Printf.sprintf
                  "%s/context/contracts/%s/unstake_requests"
                  block_prefix
                  pkh
              in
              let unstake_reqs =
                match rpc_get ~node_endpoint unstake_path with
                | Some json -> parse_unstake_requests json
                | None -> {finalizable = []; unfinalizable = []}
              in
              Some
                {
                  data with
                  staking_parameters = active_params;
                  pending_staking_parameters = pending_params;
                  unstake_requests = unstake_reqs;
                }))

let fetch_voting_info ~node_endpoint =
  let block_prefix = "/chains/main/blocks/head" in
  (* 1. Fetch current voting period *)
  let period_path = Printf.sprintf "%s/votes/current_period" block_prefix in
  match rpc_get ~node_endpoint period_path with
  | None -> None
  | Some period_json ->
      (* 2. Fetch proposals list *)
      let proposals_path = Printf.sprintf "%s/votes/proposals" block_prefix in
      let proposals_json =
        match rpc_get ~node_endpoint proposals_path with
        | Some j -> j
        | None -> `List []
      in
      (* 3. Fetch ballot list *)
      let ballot_list_path =
        Printf.sprintf "%s/votes/ballot_list" block_prefix
      in
      let ballot_list_json =
        match rpc_get ~node_endpoint ballot_list_path with
        | Some j -> j
        | None -> `List []
      in
      (* 4. Fetch current proposal *)
      let current_proposal_path =
        Printf.sprintf "%s/votes/current_proposal" block_prefix
      in
      let current_proposal_json =
        match rpc_get ~node_endpoint current_proposal_path with
        | Some j -> j
        | None -> `Null
      in
      parse_voting_info
        ~period_json
        ~proposals_json
        ~ballot_list_json
        ~current_proposal_json

(* ── Wallet Data Cache ─────────────────────────────────────── *)

let cache : (string, t) Hashtbl.t = Hashtbl.create 17

let cache_lock = Mutex.create ()

let with_cache_lock f =
  Mutex.lock cache_lock ;
  Fun.protect ~finally:(fun () -> Mutex.unlock cache_lock) f

let get ~pkh = with_cache_lock (fun () -> Hashtbl.find_opt cache pkh)

let set data = with_cache_lock (fun () -> Hashtbl.replace cache data.pkh data)

let get_all () =
  with_cache_lock (fun () -> Hashtbl.fold (fun _ v acc -> v :: acc) cache [])

let clear () = with_cache_lock (fun () -> Hashtbl.clear cache)

let is_stale ~max_age data = Unix.gettimeofday () -. data.fetched_at > max_age

(* ── Voting Info Cache ─────────────────────────────────────── *)

let voting_cache : (string, voting_info) Hashtbl.t = Hashtbl.create 5

let voting_cache_lock = Mutex.create ()

let with_voting_cache_lock f =
  Mutex.lock voting_cache_lock ;
  Fun.protect ~finally:(fun () -> Mutex.unlock voting_cache_lock) f

let get_voting_info ~node_endpoint =
  with_voting_cache_lock (fun () -> Hashtbl.find_opt voting_cache node_endpoint)

let set_voting_info ~node_endpoint info =
  with_voting_cache_lock (fun () ->
      Hashtbl.replace voting_cache node_endpoint info)

(* ── Format Helpers ────────────────────────────────────────── *)

let format_tez mutez_str =
  try
    let mutez = Int64.of_string mutez_str in
    let negative = Int64.compare mutez 0L < 0 in
    let abs_mutez = if negative then Int64.neg mutez else mutez in
    let tez_int = Int64.div abs_mutez 1_000_000L in
    let tez_frac = Int64.to_int (Int64.rem abs_mutez 1_000_000L) in
    (* Format integer part with comma separators *)
    let int_str = Int64.to_string tez_int in
    let len = String.length int_str in
    let buf = Buffer.create (len + (len / 3)) in
    for i = 0 to len - 1 do
      if i > 0 && (len - i) mod 3 = 0 then Buffer.add_char buf ',' ;
      Buffer.add_char buf int_str.[i]
    done ;
    let sign = if negative then "-" else "" in
    Printf.sprintf "%s%s.%06d ꜩ" sign (Buffer.contents buf) tez_frac
  with _ -> mutez_str

let format_staking_limit millionths =
  let f = Float.of_int millionths /. 1_000_000.0 in
  Printf.sprintf "%.1fx" f

let format_baking_edge billionths =
  let f = Float.of_int billionths /. 10_000_000.0 in
  Printf.sprintf "%.1f%%" f

let string_of_ballot_vote = function
  | Yay -> "yay"
  | Nay -> "nay"
  | Pass -> "pass"

let string_of_voting_period_kind = function
  | Proposal -> "proposal"
  | Exploration -> "exploration"
  | Cooldown -> "cooldown"
  | Promotion -> "promotion"
  | Adoption -> "adoption"

(* ── Testing ───────────────────────────────────────────────── *)

module For_tests = struct
  let parse_delegate_aggregate = parse_delegate_aggregate

  let parse_spendable = parse_spendable

  let parse_staking_parameters = parse_staking_parameters

  let parse_unstake_requests = parse_unstake_requests

  let parse_voting_info = parse_voting_info
end
