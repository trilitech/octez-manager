(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

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

(** Parse a JSON string from the teztnets API into a list of network descriptors. *)
val parse_networks : string -> (network_info list, [> Rresult.R.msg]) result

(** Fetch and parse the list of available Tezos networks.
    An optional [fetch] function can be supplied for testing. *)
val list_networks :
  ?fetch:(unit -> (string, [> Rresult.R.msg]) result) ->
  unit ->
  (network_info list, [> Rresult.R.msg]) result

(** Hardcoded [(alias, network_url)] pairs used as a fallback when the
    teztnets API is unreachable. *)
val fallback_pairs : (string * string) list

(** Resolve a chain name (e.g. ["TEZOS_MAINNET"]) to a {!network_info}
    by matching against known networks. *)
val resolve_network_from_node_chain :
  string -> (network_info, [> Rresult.R.msg]) result

(** Resolve a network alias to its [--network] URL for [octez-node config init].
    Uses the teztnets API (or the supplied [fetch]).  *)
val resolve_network_for_octez_node :
  ?fetch:(unit -> (network_info list, Rresult.R.msg) result) ->
  string ->
  (string, [> Rresult.R.msg]) result

(** Query a running node's RPC endpoint to determine its chain name. *)
val resolve_octez_node_chain :
  endpoint:string -> (string, [> `Msg of string]) result

module For_tests : sig
  val fetch_json_with :
    via_eio:(unit -> (string, Rresult.R.msg) result) ->
    via_curl:(unit -> (string, Rresult.R.msg) result) ->
    (string, Rresult.R.msg) result
end
