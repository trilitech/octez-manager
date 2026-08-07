(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

type rpc_metrics = {
  chain_id : string option;
  head_level : int option;
  bootstrapped : bool option;
  last_rpc_refresh : float option;
  node_version : string option;
  data_size : string option;
  proto : string option;
  last_error : string option;
  last_block_time : float option;
}

(** Store RPC metrics for [instance] in the cache (thread-safe). *)
val set : instance:string -> rpc_metrics -> unit

(** Retrieve cached RPC metrics for [instance], or [None] if not yet fetched. *)
val get : instance:string -> rpc_metrics option

(** Clear all cached RPC metrics. *)
val clear : unit -> unit
