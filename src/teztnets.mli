(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
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

val parse_networks : string -> (network_info list, [> Rresult.R.msg]) result

val list_networks :
  ?fetch:(unit -> (string, [> Rresult.R.msg]) result) ->
  unit ->
  (network_info list, [> Rresult.R.msg]) result

val fallback_pairs : (string * string) list

val resolve_network_for_octez_node :
  ?fetch:(unit -> (network_info list, Rresult.R.msg) result) ->
  string ->
  (string, [> Rresult.R.msg]) result

val resolve_octez_node_chain :
  endpoint:string -> (string, [> `Msg of string]) result

module For_tests : sig
  val fetch_json_with :
    via_eio:(unit -> (string, Rresult.R.msg) result) ->
    via_curl:(unit -> (string, Rresult.R.msg) result) ->
    (string, Rresult.R.msg) result
end
