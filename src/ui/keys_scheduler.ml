(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Octez_manager_lib

type wallet_data = {
  pkh : string;
  network : string;
  spendable_balance : string;
  staked_balance : string;
  full_balance : string;
  delegate : string option;
  is_registered : bool;
  active_consensus_key : string option;
  fetched_at : float;
}

(** Cache: pkh -> wallet_data list (one per network) *)
let cache : (string, wallet_data list) Hashtbl.t = Hashtbl.create 64

let cache_lock = Mutex.create ()

(** Keys to poll: (base_dir, pkh list) pairs *)
let tracked_keys : (string * string list) list ref = ref []

let keys_lock = Mutex.create ()

let get_wallet_data ~pkh =
  Mutex.protect cache_lock (fun () ->
      Hashtbl.find_opt cache pkh |> Option.value ~default:[])

(** Store wallet_data for a pkh, merging with existing entries for other
    networks. *)
let store_wallet_data wd =
  Mutex.protect cache_lock (fun () ->
      let existing =
        Hashtbl.find_opt cache wd.pkh |> Option.value ~default:[]
      in
      let others =
        List.filter
          (fun (w : wallet_data) -> not (String.equal w.network wd.network))
          existing
      in
      Hashtbl.replace cache wd.pkh (wd :: others))

let set_keys keys = Mutex.protect keys_lock (fun () -> tracked_keys := keys)

let stop_flag = Atomic.make false

let started = ref false

(** Pending force-refresh PKHs *)
let pending_refresh : (string, unit) Hashtbl.t = Hashtbl.create 8

let pending_lock = Mutex.create ()

let force_refresh ~pkh =
  Mutex.protect pending_lock (fun () -> Hashtbl.replace pending_refresh pkh ())

(** Get all running node endpoints grouped by network.
    Falls back to public RPC nodes when no local nodes are running. *)
let get_node_endpoints () =
  let local =
    Data.load_service_states ()
    |> List.filter (fun (st : Data.Service_state.t) ->
        String.equal st.service.role "node"
        && match st.status with Running -> true | _ -> false)
    |> List.map (fun (st : Data.Service_state.t) ->
        (st.service.network, Rpc_addr.to_endpoint st.service.rpc_addr))
  in
  let result =
    if local <> [] then local
    else
      (* No local nodes — fall back to public RPC nodes *)
      Public_nodes_cache.get_nodes ()
      |> List.filter_map (fun (n : Public_nodes_cache.node_info) ->
          match n.network with
          | Some net -> Some (net, n.rpc_addr)
          | None -> None)
  in
  List.sort_uniq (fun (a, _) (b, _) -> String.compare a b) result

(** Fetch a JSON string field from an RPC endpoint. *)
let rpc_get_string endpoint path =
  let url = endpoint ^ path in
  match Cmd_runner.run_out_silent ["curl"; "-sfL"; "--max-time"; "10"; url] with
  | Error _ -> None
  | Ok body -> (
      try
        match Yojson.Safe.from_string body with
        | `String s -> Some s
        | _ -> Some (String.trim body)
      with _ -> Some (String.trim body))

(** Fetch balance data for a pkh from a node endpoint. *)
let fetch_wallet_data ~network ~endpoint ~pkh =
  let base =
    Printf.sprintf "/chains/main/blocks/head/context/contracts/%s" pkh
  in
  let spendable =
    rpc_get_string endpoint (base ^ "/balance") |> Option.value ~default:"0"
  in
  let full_balance =
    rpc_get_string endpoint (base ^ "/full_balance")
    |> Option.value ~default:spendable
  in
  let delegate = rpc_get_string endpoint (base ^ "/delegate") in
  (* Check if this pkh is itself a registered delegate *)
  let delegate_path =
    Printf.sprintf "/chains/main/blocks/head/context/delegates/%s" pkh
  in
  let is_registered =
    match
      Cmd_runner.run_out_silent
        ["curl"; "-sfL"; "--max-time"; "10"; endpoint ^ delegate_path]
    with
    | Ok _ -> true
    | Error _ -> false
  in
  let active_consensus_key =
    if is_registered then
      rpc_get_string endpoint (delegate_path ^ "/consensus_key")
    else None
  in
  (* Compute staked = full - spendable *)
  let staked_balance =
    match (int_of_string_opt full_balance, int_of_string_opt spendable) with
    | Some full, Some spend -> string_of_int (full - spend)
    | _ -> "0"
  in
  {
    pkh;
    network;
    spendable_balance = spendable;
    staked_balance;
    full_balance;
    delegate;
    is_registered;
    active_consensus_key;
    fetched_at = Unix.gettimeofday ();
  }

(** Poll all tracked keys across all running node networks. *)
let poll () =
  let endpoints = get_node_endpoints () in
  let keys =
    Mutex.protect keys_lock (fun () -> !tracked_keys)
    |> List.concat_map snd
    |> List.sort_uniq String.compare
  in
  List.iter
    (fun pkh ->
      List.iter
        (fun (network, endpoint) ->
          if Atomic.get stop_flag then ()
          else
            let wd = fetch_wallet_data ~network ~endpoint ~pkh in
            store_wallet_data wd)
        endpoints)
    keys

(** Process any pending force-refresh requests. *)
let process_pending () =
  let pending =
    Mutex.protect pending_lock (fun () ->
        let entries =
          Hashtbl.fold (fun pkh () acc -> pkh :: acc) pending_refresh []
        in
        Hashtbl.clear pending_refresh ;
        entries)
  in
  if pending <> [] then
    let endpoints = get_node_endpoints () in
    List.iter
      (fun pkh ->
        List.iter
          (fun (network, endpoint) ->
            if not (Atomic.get stop_flag) then
              let wd = fetch_wallet_data ~network ~endpoint ~pkh in
              store_wallet_data wd)
          endpoints)
      pending

(** Poll interval: 30 seconds *)
let poll_interval = 30.0

let refresh_tzkt_aliases () =
  let networks =
    get_node_endpoints () |> List.map fst |> List.sort_uniq String.compare
  in
  List.iter
    (fun network ->
      if Tzkt_aliases.needs_refresh ~network then Tzkt_aliases.refresh ~network)
    networks

let scheduler_loop () =
  Eio_unix.sleep 3.0 ;
  while not (Atomic.get stop_flag) do
    (try
       process_pending () ;
       poll () ;
       refresh_tzkt_aliases ()
     with _ -> ()) ;
    Eio_unix.sleep poll_interval
  done

let start () =
  if not !started then (
    started := true ;
    Atomic.set stop_flag false ;
    Domain_pool.submit scheduler_loop)

let stop () = Atomic.set stop_flag true

let get_endpoints_for_network ~network =
  get_node_endpoints ()
  |> List.filter_map (fun (net, endpoint) ->
      if String.equal net network then Some endpoint else None)
