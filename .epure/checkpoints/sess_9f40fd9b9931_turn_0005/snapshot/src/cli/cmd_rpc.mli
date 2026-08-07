(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** RPC browsing and execution commands. *)

open Octez_manager_lib

(** The rpc command group *)
val rpc_cmd : unit Cmdliner.Cmd.t

(** Parse a URL into a synthetic service for RPC calls. *)
val service_from_url : string -> Service.t

(** Resolve an RPC service from instance name, URL, or public node name. *)
val resolve_service :
  string option -> string option -> string option -> (Service.t, string) result
