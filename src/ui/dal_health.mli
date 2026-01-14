(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** DAL node health status polling and caching. *)

type status = Up | Down | Degraded | Unknown

type check = {name : string; status : status}

type t = {status : status; checks : check list; last_fetch : float}

val status_to_string : status -> string

val fetch : rpc_endpoint:string -> t option

val get : instance:string -> t option

val set : instance:string -> t -> unit

val clear_instance : instance:string -> unit

val clear : unit -> unit
