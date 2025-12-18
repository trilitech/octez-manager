(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Delegate data fetching and caching.

    Fetches delegate participation data from node RPC and caches it
    for use by baker, DAL node, and other components. *)

open Octez_manager_lib

(** Participation info from RPC *)
type participation = {
  expected_cycle_activity : int;
  minimal_cycle_activity : int;
  missed_slots : int;
  missed_levels : int;
  remaining_allowed_missed_slots : int;
  expected_attesting_rewards : string;
}

(** DAL participation info *)
type dal_participation = {
  expected_assigned_shards_per_slot : int;
  delegate_attested_dal_slots : int;
  delegate_attestable_dal_slots : int;
  expected_dal_rewards : string;
  sufficient_dal_participation : bool;
  denounced : bool;
}

(** Core delegate data we care about *)
type t = {
  pkh : string;
  deactivated : bool;
  is_forbidden : bool;
  participation : participation;
  dal_participation : dal_participation;
  baking_power : string;
  total_staked : string;
  total_delegated : string;
  own_full_balance : string;
  fetched_at : float;
}

(** Parse participation from JSON *)
let parse_participation json =
  let open Yojson.Safe.Util in
  {
    expected_cycle_activity =
      json |> member "expected_cycle_activity" |> to_int_option
      |> Option.value ~default:0;
    minimal_cycle_activity =
      json |> member "minimal_cycle_activity" |> to_int_option
      |> Option.value ~default:0;
    missed_slots =
      json |> member "missed_slots" |> to_int_option |> Option.value ~default:0;
    missed_levels =
      json |> member "missed_levels" |> to_int_option |> Option.value ~default:0;
    remaining_allowed_missed_slots =
      json |> member "remaining_allowed_missed_slots" |> to_int_option
      |> Option.value ~default:0;
    expected_attesting_rewards =
      json |> member "expected_attesting_rewards" |> to_string_option
      |> Option.value ~default:"0";
  }

(** Parse DAL participation from JSON *)
let parse_dal_participation json =
  let open Yojson.Safe.Util in
  {
    expected_assigned_shards_per_slot =
      json |> member "expected_assigned_shards_per_slot" |> to_int_option
      |> Option.value ~default:0;
    delegate_attested_dal_slots =
      json |> member "delegate_attested_dal_slots" |> to_int_option
      |> Option.value ~default:0;
    delegate_attestable_dal_slots =
      json |> member "delegate_attestable_dal_slots" |> to_int_option
      |> Option.value ~default:0;
    expected_dal_rewards =
      json |> member "expected_dal_rewards" |> to_string_option
      |> Option.value ~default:"0";
    sufficient_dal_participation =
      json |> member "sufficient_dal_participation" |> to_bool_option
      |> Option.value ~default:false;
    denounced =
      json |> member "denounced" |> to_bool_option |> Option.value ~default:false;
  }

(** Parse delegate data from RPC JSON response *)
let of_json ~pkh json =
  let open Yojson.Safe.Util in
  try
    let deactivated =
      json |> member "deactivated" |> to_bool_option
      |> Option.value ~default:false
    in
    let is_forbidden =
      json |> member "is_forbidden" |> to_bool_option
      |> Option.value ~default:false
    in
    let participation =
      json |> member "participation" |> parse_participation
    in
    let dal_participation =
      json |> member "dal_participation" |> parse_dal_participation
    in
    let baking_power =
      json |> member "baking_power" |> to_string_option
      |> Option.value ~default:"0"
    in
    let total_staked =
      json |> member "total_staked" |> to_string_option
      |> Option.value ~default:"0"
    in
    let total_delegated =
      json |> member "total_delegated" |> to_string_option
      |> Option.value ~default:"0"
    in
    let own_full_balance =
      json |> member "own_full_balance" |> to_string_option
      |> Option.value ~default:"0"
    in
    Some
      {
        pkh;
        deactivated;
        is_forbidden;
        participation;
        dal_participation;
        baking_power;
        total_staked;
        total_delegated;
        own_full_balance;
        fetched_at = Unix.gettimeofday ();
      }
  with _ -> None

(** Fetch delegate data from node RPC.
    Uses head~2 for stability (head can change rapidly). *)
let fetch ~node_endpoint ~pkh =
  let url =
    Printf.sprintf "%s/chains/main/blocks/head~2/context/delegates/%s"
      node_endpoint pkh
  in
  match Common.run_out ["curl"; "-sfL"; "--max-time"; "10"; url] with
  | Error _ -> None
  | Ok body -> (
      try
        let json = Yojson.Safe.from_string body in
        of_json ~pkh json
      with _ -> None)

(** Cache storage *)
let cache : (string, t) Hashtbl.t = Hashtbl.create 17

let cache_lock = Mutex.create ()

let with_cache_lock f =
  Mutex.lock cache_lock ;
  Fun.protect ~finally:(fun () -> Mutex.unlock cache_lock) f

(** Get cached delegate data *)
let get ~pkh = with_cache_lock (fun () -> Hashtbl.find_opt cache pkh)

(** Store delegate data in cache *)
let set data = with_cache_lock (fun () -> Hashtbl.replace cache data.pkh data)

(** Get all cached delegates *)
let get_all () =
  with_cache_lock (fun () ->
      Hashtbl.fold (fun _ v acc -> v :: acc) cache [])

(** Clear cache *)
let clear () = with_cache_lock (fun () -> Hashtbl.clear cache)

(** Check if data is stale (older than max_age seconds) *)
let is_stale ~max_age data =
  Unix.gettimeofday () -. data.fetched_at > max_age

(** Missed slots status *)
type missed_status =
  | Good        (** No missed slots *)
  | Warning     (** Missed >= remaining/2 *)
  | Critical    (** Missed > remaining *)

(** Get missed slots status *)
let missed_slots_status data =
  let missed = data.participation.missed_slots in
  let remaining = data.participation.remaining_allowed_missed_slots in
  if missed = 0 then Good
  else if remaining > 0 && missed >= remaining then Critical
  else if remaining > 0 && missed >= remaining / 2 then Warning
  else Good

(** Format mutez amount as tez with suffix *)
let format_tez mutez_str =
  try
    let mutez = Int64.of_string mutez_str in
    let tez = Int64.to_float mutez /. 1_000_000.0 in
    if tez >= 1_000_000.0 then Printf.sprintf "%.1fM" (tez /. 1_000_000.0)
    else if tez >= 1_000.0 then Printf.sprintf "%.1fK" (tez /. 1_000.0)
    else Printf.sprintf "%.0f" tez
  with _ -> mutez_str
