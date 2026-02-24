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

(** Worker queue for per-key fetch requests with deduplication. *)
let worker : unit Worker_queue.t = Worker_queue.create ~name:"keys" ()

(** Get all node endpoints grouped by network.
    Starts with local running nodes, then supplements with public RPC nodes
    for any networks not already covered locally.  This ensures that a key
    from a ghostnet-only setup is still checked on mainnet, etc. *)
let get_node_endpoints () =
  let local =
    Data.load_service_states ()
    |> List.filter (fun (st : Data.Service_state.t) ->
        String.equal st.service.role "node"
        && match st.status with Running -> true | _ -> false)
    |> List.map (fun (st : Data.Service_state.t) ->
        (st.service.network, Rpc_addr.to_endpoint st.service.rpc_addr))
  in
  let local_networks = List.map fst local |> List.sort_uniq String.compare in
  let public_extra =
    Public_nodes_cache.get_nodes ()
    |> List.filter_map (fun (n : Public_nodes_cache.node_info) ->
        match n.network with
        | Some net
          when not (List.exists (fun ln -> String.equal ln net) local_networks)
          ->
            Some (net, n.rpc_addr)
        | _ -> None)
  in
  local @ public_extra

(** Pick one random endpoint per network, distributing load across providers. *)
let pick_endpoints () =
  let all = get_node_endpoints () in
  let by_network : (string, string list) Hashtbl.t = Hashtbl.create 8 in
  List.iter
    (fun (net, ep) ->
      let existing =
        Hashtbl.find_opt by_network net |> Option.value ~default:[]
      in
      Hashtbl.replace by_network net (ep :: existing))
    all ;
  Hashtbl.fold
    (fun net eps acc ->
      let arr = Array.of_list eps in
      let ep = arr.(Random.int (Array.length arr)) in
      (net, ep) :: acc)
    by_network
    []

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

(** Fetch a pkh across all networks, storing results incrementally. *)
let fetch_pkh_all_networks ~pkh =
  let endpoints = pick_endpoints () in
  List.iter
    (fun (network, endpoint) ->
      if not (Atomic.get stop_flag) then
        let wd = fetch_wallet_data ~network ~endpoint ~pkh in
        store_wallet_data wd)
    endpoints

(** Poll interval: 30 seconds. Data fresher than this is not re-fetched. *)
let poll_interval = 30.0

(** Request a fetch for a specific PKH. The request is dropped if the PKH is
    already pending in the worker queue or its cached data is fresh enough
    (< 30s old on all networks). *)
let request_fetch ~pkh =
  let dominated =
    get_wallet_data ~pkh
    |> List.for_all (fun (w : wallet_data) ->
        Unix.gettimeofday () -. w.fetched_at < poll_interval)
  in
  if (not dominated) || List.length (get_wallet_data ~pkh) = 0 then
    Worker_queue.submit_unit worker ~key:pkh ~work:(fun () ->
        fetch_pkh_all_networks ~pkh)

(** Force an immediate re-fetch for a specific PKH, bypassing staleness. *)
let force_refresh ~pkh =
  Worker_queue.submit_unit worker ~key:pkh ~work:(fun () ->
      fetch_pkh_all_networks ~pkh)

let refresh_tzkt_aliases () =
  let networks =
    get_node_endpoints () |> List.map fst |> List.sort_uniq String.compare
  in
  List.iter
    (fun network ->
      if Tzkt_aliases.needs_refresh ~network then Tzkt_aliases.refresh ~network)
    networks

let scheduler_loop () =
  Worker_queue.start worker ;
  Eio_unix.sleep 3.0 ;
  while not (Atomic.get stop_flag) do
    (try refresh_tzkt_aliases () with _ -> ()) ;
    Eio_unix.sleep poll_interval
  done ;
  Worker_queue.stop worker

let start () =
  if not !started then (
    started := true ;
    Atomic.set stop_flag false ;
    Domain_pool.submit scheduler_loop)

let stop () =
  Atomic.set stop_flag true ;
  Worker_queue.stop worker

let get_endpoints_for_network ~network =
  get_node_endpoints ()
  |> List.filter_map (fun (net, endpoint) ->
      if String.equal net network then Some endpoint else None)
