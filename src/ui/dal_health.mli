(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

(** DAL node health status polling and caching. *)

type status = Up | Down | Degraded | Unknown

type check = {name : string; status : status}

type t = {status : status; checks : check list; last_fetch : float}

(** Parse a status string (e.g. ["up"], ["degraded"]) into a {!status}. *)
val status_of_string : string -> status

(** Convert a status to its string representation. *)
val status_to_string : status -> string

(** Fetch DAL health from a node's RPC endpoint (performs I/O).
    Returns [None] if the endpoint is unreachable or the response is invalid. *)
val fetch : rpc_endpoint:string -> t option

(** Retrieve the cached DAL health for [instance], or [None] if not yet fetched. *)
val get : instance:string -> t option

(** Store DAL health for [instance] in the cache (thread-safe). *)
val set : instance:string -> t -> unit

(** Remove the cached DAL health for [instance]. *)
val clear_instance : instance:string -> unit

(** Clear all cached DAL health data. *)
val clear : unit -> unit
