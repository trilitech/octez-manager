(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** UI flow orchestration for quick-create wizards.

    Provides modal-based flows for creating node, baker, accuser,
    and DAL node instances via interactive prompts. *)

open Octez_manager_lib

(** Error message shown when an instance name contains invalid characters. *)
val invalid_instance_name_error_msg : string

(** Remove the ["node-"] prefix from an instance name, if present. *)
val strip_node_prefix : string -> string

(** Open the node creation wizard (instance name, network, history mode,
    bootstrap method). *)
val create_node_flow : on_success:(unit -> unit) -> unit

(** Open the baker creation wizard, selecting from available node services. *)
val create_baker_flow :
  services:Service.t list -> on_success:(unit -> unit) -> unit

(** Open the accuser creation wizard. *)
val create_accuser_flow : on_success:(unit -> unit) -> unit

(** Open the DAL node creation wizard. *)
val create_dal_node_flow : on_success:(unit -> unit) -> unit
