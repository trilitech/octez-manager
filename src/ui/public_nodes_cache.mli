(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Cache for public RPC nodes fetched from Taquito. *)

open Octez_manager_lib

(** A public RPC node's display label, RPC address, and optional network. *)
type node_info = {label : string; rpc_addr : string; network : string option}

(** Hardcoded fallback list of well-known public RPC nodes. *)
val curated_defaults : node_info list

(** Infer the Tezos network name from an RPC URL by matching known domains. *)
val extract_network_from_url : string -> string option

(** Parse a Taquito-format JSON string into a list of node info records. *)
val parse_taquito_json : string -> node_info list

(** Download the public node list from Taquito URLs, returning the first
    successfully parsed non-empty result or [[]] on failure. *)
val fetch_nodes : unit -> node_info list

(** Replace the in-memory cached node list. *)
val set_cache : node_info list -> unit

(** Return cached public nodes, fetching and caching if empty.
    Falls back to {!curated_defaults} if fetching yields nothing. *)
val get_nodes : unit -> node_info list

(** Convert a {!node_info} to a {!Service.t} suitable for RPC calls. *)
val to_service : node_info -> Service.t

(** Return all public nodes as a [Service.t list]. *)
val get_services : unit -> Service.t list
