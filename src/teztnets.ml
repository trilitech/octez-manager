(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Rresult

let teztnets_url = "https://teztnets.com/teztnets.json"

let fetch_via_curl () : (string, [> R.msg]) result =
  match
    Common.run_out
      ["curl"; "-fsLm"; "5"; "--connect-timeout"; "2"; teztnets_url]
  with
  | Ok body when String.trim body <> "" -> Ok body
  | Ok _ -> R.error_msg "Empty teztnets.json response"
  | Error (`Msg m) -> R.error_msg m

let fetch_json () : (string, [> R.msg]) result = fetch_via_curl ()

type network_info = {
  alias : string;
  network_url : string;
  chain_name : string;
  human_name : string;
  description : string option;
  faucet_url : string option;
  rpc_url : string option;
  docker_build : string option;
  git_ref : string option;
  last_updated : string option;
  category : string option;
}

let pick_string (klist : string list) (obj : Yojson.Safe.t) : string option =
  let open Yojson.Safe.Util in
  let rec loop ks =
    match ks with
    | [] -> None
    | k :: ks -> (
        match member k obj with
        | `String s when s <> "" -> Some s
        | _ -> loop ks)
  in
  loop klist

let parse_networks (json_s : string) : (network_info list, [> R.msg]) result =
  try
    let open Yojson.Safe in
    let json = from_string json_s in
    let items : Yojson.Safe.t list =
      match json with
      | `List lst -> lst
      | `Assoc kvs -> (
          match
            List.find_opt
              (fun (k, v) ->
                match v with
                | `List _ ->
                    List.mem
                      k
                      [
                        "nets"; "networks"; "items"; "chains"; "teztnets"; "data";
                      ]
                | _ -> false)
              kvs
          with
          | Some (_k, `List lst) -> lst
          | _ ->
              List.filter_map
                (fun (_k, v) -> match v with `Assoc _ -> Some v | _ -> None)
                kvs)
      | _ -> []
    in
    let to_info it =
      let human_name =
        Option.value
          ~default:"unknown"
          (pick_string ["humanName"; "human_name"; "name"; "id"] it)
      in
      let chain_name =
        Option.value ~default:"unknown" (pick_string ["chain_name"] it)
      in
      let slug =
        Option.value ~default:human_name (pick_string ["slug"; "id"; "name"] it)
      in
      let network_url =
        pick_string
          [
            "networkJsonUrl";
            "network_json_url";
            "networkJson";
            "network_json";
            "configURL";
            "config_url";
            "networkURL";
            "network_url";
            "networkConfigURL";
            "network_config_url";
            "chainJSONURL";
            "chain_json_url";
            "json";
          ]
          it
      in
      let is_mn s =
        let s = String.lowercase_ascii s in
        s = "mainnet" || s = "ghostnet"
      in
      let final_url =
        match (slug, human_name, network_url) with
        | s, _, _ when is_mn s -> String.lowercase_ascii s
        | _, l, _ when is_mn l -> String.lowercase_ascii l
        | _, _, Some u -> u
        | _ -> ""
      in
      if final_url = "" then None
      else
        Some
          {
            alias = slug;
            network_url = final_url;
            chain_name;
            human_name;
            description = pick_string ["description"; "desc"] it;
            faucet_url = pick_string ["faucetUrl"; "faucet_url"; "faucet"] it;
            rpc_url = pick_string ["rpcUrl"; "rpc_url"; "rpc"] it;
            docker_build = pick_string ["dockerBuild"; "docker_build"] it;
            git_ref = pick_string ["gitRef"; "git_ref"; "branch"; "tag"] it;
            last_updated = pick_string ["lastUpdated"; "last_updated"] it;
            category = pick_string ["category"; "type"] it;
          }
    in
    let infos = items |> List.filter_map to_info in
    match infos with
    | [] -> R.error_msg "No networks found in teztnets.json"
    | _ -> Ok infos
  with exn ->
    R.error_msgf "Failed to parse teztnets.json: %s" (Printexc.to_string exn)

let fallback_networks =
  [
    {
      alias = "mainnet";
      network_url = "mainnet";
      chain_name = "TEZOS_MAINNET";
      human_name = "Tezos Mainnet";
      description = Some "The main Tezos network";
      faucet_url = None;
      rpc_url = Some "https://mainnet.ecadinfra.com";
      docker_build = None;
      git_ref = None;
      last_updated = None;
      category = Some "Protocol";
    };
    {
      alias = "ghostnet";
      network_url = "ghostnet";
      chain_name = "TEZOS_ITHACANET_2022-01-25T15:00:00Z";
      human_name = "Ghostnet";
      description = Some "Long-running testnet";
      faucet_url = Some "https://faucet.ghostnet.teztnets.com";
      rpc_url = Some "https://rpc.ghostnet.teztnets.com";
      docker_build = None;
      git_ref = None;
      last_updated = None;
      category = Some "Testnet";
    };
    {
      alias = "seoulnet";
      network_url = "https://teztnets.com/seoulnet";
      chain_name = "TEZOS_SEOULNET_2025-07-11T08:00:00Z";
      human_name = "Seoulnet";
      description = Some "Testnet for Seoul protocol";
      faucet_url = Some "https://faucet.seoulnet.teztnets.com";
      rpc_url = Some "https://rpc.seoulnet.teztnets.com";
      docker_build = None;
      git_ref = None;
      last_updated = None;
      category = Some "Testnet";
    };
    {
      alias = "weeklynet";
      network_url = "https://teztnets.com/weeklynet";
      chain_name = "TEZOS-WEEKLYNET-2025-12-10T00:00:00.000Z";
      human_name = "Weeklynet";
      description = Some "Weekly ephemeral testnet";
      faucet_url = None;
      rpc_url = None;
      docker_build = None;
      git_ref = None;
      last_updated = None;
      category = Some "Testnet";
    };
  ]

let list_networks ?(fetch = fetch_json) () :
    (network_info list, [> R.msg]) result =
  match fetch () with
  | Ok s -> (
      match parse_networks s with
      | Ok infos when infos <> [] -> Ok infos
      | _ -> Ok fallback_networks)
  | Error _ -> Ok fallback_networks

let fallback_pairs =
  List.map (fun n -> (n.human_name, n.network_url)) fallback_networks

let is_http_url s =
  let trimmed = String.trim s |> String.lowercase_ascii in
  String.starts_with ~prefix:"http://" trimmed
  || String.starts_with ~prefix:"https://" trimmed

let is_builtin_network s =
  let lower = String.lowercase_ascii (String.trim s) in
  lower = "mainnet" || lower = "ghostnet" || lower = "sandbox"

let resolve_network_for_octez_node :
    ?fetch:(unit -> (network_info list, [> Rresult.R.msg]) result) ->
    string ->
    (string, [> Rresult.R.msg]) result =
 fun ?(fetch =
       fun () ->
         (list_networks () : (network_info list, [> Rresult.R.msg]) result))
     network ->
  let trimmed = String.trim network in
  if trimmed = "" then R.error_msg "Network cannot be empty"
  else if is_http_url trimmed then Ok trimmed
  else if is_builtin_network trimmed then Ok (String.lowercase_ascii trimmed)
  else if Sys.file_exists trimmed then Ok trimmed
  else
    let lower_input = String.lowercase_ascii trimmed in
    match fetch () with
    | Ok infos -> (
        let match_opt =
          List.find_opt
            (fun (n : network_info) ->
              String.lowercase_ascii n.alias = lower_input
              || String.lowercase_ascii n.human_name = lower_input)
            infos
        in
        match match_opt with
        | Some info -> Ok info.network_url
        | None ->
            R.error_msgf
              "Network '%s' not recognized. Use 'list-available-networks' to \
               see supported networks, or provide a URL or file path."
              network)
    | Error (`Msg m) -> R.error_msg m

let resolve_octez_node_chain ~endpoint =
  let res =
    let ( let* ) = Result.bind in
    let* node_chain_name =
      match
        Common.run_out
          ["curl"; "-sf"; "--connect-timeout"; "2"; endpoint ^ "/config"]
      with
      | Ok out -> (
          try
            let j = Yojson.Safe.from_string out in
            let open Yojson.Safe.Util in
            match member "network" j |> member "chain_name" with
            | `String s -> Ok (String.trim s)
            | _ -> Error (`Msg "Failed to retrieve .network.chain_name")
          with exn -> Error (`Msg (Printexc.to_string exn)))
      | Error s -> Error s
    in
    let* networks = list_networks () in
    try
      Ok
        (List.find
           (fun network -> network.chain_name = node_chain_name)
           networks)
    with Not_found ->
      Error
        (`Msg (Format.sprintf "Node chain name is unknown: %S" node_chain_name))
  in
  match res with
  | Ok network -> Ok network.human_name
  | Error (`Msg msg) ->
      Error
        (`Msg
           (Printf.sprintf
              "Unable to determine network from node endpoint %s: %s. Ensure \
               the endpoint is reachable and returns a valid RPC response \
               (e.g., start the node or check the address)."
              endpoint
              msg))

module For_tests = struct
  let fetch_json_with :
      via_eio:(unit -> (string, Rresult.R.msg) result) ->
      via_curl:(unit -> (string, Rresult.R.msg) result) ->
      (string, Rresult.R.msg) result =
   fun ~via_eio ~via_curl ->
    let via_eio_safe () =
      try via_eio ()
      with exn -> R.error_msgf "eio fetch exn: %s" (Printexc.to_string exn)
    in
    let via_curl : unit -> (string, Rresult.R.msg) result = via_curl in
    let via_curl_safe () : (string, Rresult.R.msg) result = via_curl () in
    let result : (string, Rresult.R.msg) result =
      match via_eio_safe () with
      | Ok _ as ok -> ok
      | Error _ -> via_curl_safe ()
    in
    result
end
