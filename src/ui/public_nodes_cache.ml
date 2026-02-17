(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Cache for public RPC nodes fetched from Taquito *)

open Octez_manager_lib

type node_info = {label : string; rpc_addr : string; network : string option}

(* Cached public nodes *)
let cached_nodes : node_info list ref = ref []

(* Curated fallback list *)
let curated_defaults : node_info list =
  [
    {
      label = "ECAD Infra";
      rpc_addr = "https://mainnet.tezos.ecadinfra.com";
      network = Some "mainnet";
    };
    {
      label = "ecadlabs";
      rpc_addr = "https://mainnet.api.tez.ie";
      network = Some "mainnet";
    };
    {
      label = "SmartPy";
      rpc_addr = "https://mainnet.smartpy.io";
      network = Some "mainnet";
    };
  ]

(** Extract network name from RPC URL when not explicitly provided.
    Looks for common patterns like "mainnet.domain.com" or "https://shadownet.domain.com" *)
let extract_network_from_url (url : string) : string option =
  let lower_url = String.lowercase_ascii url in
  (* Simple substring check *)
  let contains_substring haystack needle =
    let len_h = String.length haystack in
    let len_n = String.length needle in
    let rec check pos =
      if pos + len_n > len_h then false
      else if String.sub haystack pos len_n = needle then true
      else check (pos + 1)
    in
    if len_n = 0 then true else check 0
  in
  (* First check for known URLs that don't contain network in their name *)
  let known_url_mappings =
    [
      ("tzbeta.net", "mainnet");
      (* Note: rpc.tzkt.io removed - it has network in path: rpc.tzkt.io/mainnet *)
    ]
  in
  match
    List.find_opt
      (fun (pattern, _) -> contains_substring lower_url pattern)
      known_url_mappings
  with
  | Some (_, network) -> Some network
  | None ->
      (* Fall back to searching for network name in URL *)
      let known_networks =
        [
          "mainnet";
          "shadownet";
          "tallinnnet";
          "weeklynet";
          "dailynet";
          "mondaynet";
        ]
      in
      List.find_opt (fun net -> contains_substring lower_url net) known_networks

(** Parse Taquito JSON format to extract public nodes *)
let parse_taquito_json (txt : string) : node_info list =
  try
    let j = Yojson.Safe.from_string txt in
    let parse_assoc_list lst ~get_rpc ~get_label ~get_net =
      List.filter_map
        (function
          | `Assoc kv ->
              let rpc = get_rpc kv in
              if rpc = "" then None
              else
                let network =
                  match get_net kv with
                  | Some _ as net -> net (* Explicit network provided *)
                  | None -> extract_network_from_url rpc (* Extract from URL *)
                in
                Some {label = get_label kv rpc; rpc_addr = rpc; network}
          | _ -> None)
        lst
    in
    match j with
    | `List lst ->
        parse_assoc_list
          lst
          ~get_rpc:(fun kv ->
            match List.assoc_opt "rpc" kv with
            | Some (`String s) -> s
            | _ -> (
                match List.assoc_opt "rpc_url" kv with
                | Some (`String s) -> s
                | _ -> ""))
          ~get_label:(fun kv rpc ->
            match List.assoc_opt "name" kv with
            | Some (`String s) when s <> "" -> s
            | _ -> rpc)
          ~get_net:(fun kv ->
            match List.assoc_opt "network" kv with
            | Some (`String s) -> Some s
            | _ -> None)
    | `Assoc kvs -> (
        (* Taquito format: providers map + rpc_endpoints list *)
        let provider_names =
          match List.assoc_opt "providers" kvs with
          | Some (`List provs) ->
              List.fold_left
                (fun acc p ->
                  match p with
                  | `Assoc pkv ->
                      let id =
                        match List.assoc_opt "id" pkv with
                        | Some (`String s) -> s
                        | _ -> ""
                      in
                      let name =
                        match List.assoc_opt "name" pkv with
                        | Some (`String s) -> s
                        | _ -> id
                      in
                      if id <> "" then (id, name) :: acc else acc
                  | _ -> acc)
                []
                provs
          | _ -> []
        in
        match List.assoc_opt "rpc_endpoints" kvs with
        | Some (`List eps) ->
            parse_assoc_list
              eps
              ~get_rpc:(fun kv ->
                match List.assoc_opt "url" kv with
                | Some (`String s) -> s
                | _ -> "")
              ~get_label:(fun kv rpc ->
                let provider_id =
                  match List.assoc_opt "provider" kv with
                  | Some (`String s) -> s
                  | _ -> ""
                in
                let provider_name =
                  match List.assoc_opt provider_id provider_names with
                  | Some n -> n
                  | None -> provider_id
                in
                (* Don't include network in label - it's stored separately and
                   displayed when needed to avoid duplication *)
                if provider_name <> "" then provider_name else rpc)
              ~get_net:(fun kv ->
                match List.assoc_opt "network" kv with
                | Some (`String s) -> Some s
                | _ -> None)
        | _ -> [])
    | _ -> []
  with _ -> []

(** Fetch public nodes from Taquito URLs *)
let fetch_nodes () : node_info list =
  let urls =
    [
      "https://taquito.io/docs/rpc_nodes.json";
      "https://taquito.io/rpc_nodes.json";
      "https://www.taquito.io/docs/rpc_nodes.json";
    ]
  in
  let is_ghostnet_node node =
    match node.network with
    | Some n ->
        let n_lower = String.lowercase_ascii n in
        n_lower = "ghostnet"
    | None ->
        let url_lower = String.lowercase_ascii node.rpc_addr in
        let contains s substring =
          try
            let _ = Str.search_forward (Str.regexp_string substring) s 0 in
            true
          with Not_found -> false
        in
        contains url_lower "ghostnet"
  in
  let rec try_urls = function
    | [] -> []
    | url :: rest -> (
        let cmd = ["curl"; "-fsSL"; "--max-time"; "5"; url] in
        match Cmd_runner.run_out cmd with
        | Ok body ->
            let nodes =
              parse_taquito_json body
              |> List.filter (fun n -> not (is_ghostnet_node n))
            in
            if nodes <> [] then nodes else try_urls rest
        | Error _ -> try_urls rest)
  in
  try_urls urls

(** Set cached nodes (called by rpc_node_selection when it fetches) *)
let set_cache nodes = cached_nodes := nodes

(** Get cached nodes, or fetch if empty *)
let get_nodes () : node_info list =
  if !cached_nodes <> [] then !cached_nodes
  else
    let nodes = fetch_nodes () in
    if nodes <> [] then (
      cached_nodes := nodes ;
      nodes)
    else curated_defaults

(** Convert node_info to Service.t for RPC calls *)
let to_service (info : node_info) : Service.t =
  {
    Service.instance = info.label;
    role = "node";
    network = Option.value ~default:"unknown" info.network;
    history_mode = History_mode.Rolling;
    data_dir = "";
    rpc_addr = Rpc_addr.of_string info.rpc_addr;
    net_addr = "";
    service_user = "";
    app_bin_dir = "";
    bin_source = None;
    created_at = "";
    logging_mode = Logging_mode.Journald;
    snapshot_auto = false;
    snapshot_uri = None;
    snapshot_network_slug = None;
    snapshot_no_check = false;
    extra_args = [];
    depends_on = None;
    dependents = [];
    signer_mode = None;
    signer_uri = None;
  }

(** Get all public nodes as Service.t list *)
let get_services () : Service.t list = List.map to_service (get_nodes ())
