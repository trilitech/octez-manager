(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Common types and utilities for form builder field bundles. *)

(** {1 Configuration Types} *)

(** Core service configuration shared by all Octez tools. *)
type core_service_config = {
  instance_name : string;
  service_user : string;
  app_bin_dir : string;
  logging : [`Journald | `File];
  enable_on_boot : bool;
  start_now : bool;
  extra_args : string;
}

(** Client-based tool configuration (baker, accuser, DAL node, signer).
    These tools connect to a node and use a base directory for keys/config. *)
type client_config = {
  base_dir : string;
  node : [`Service of string | `Endpoint of string | `None];
  node_endpoint : string;  (** Resolved endpoint from node selection *)
}

(** Node-specific configuration. *)
type node_config = {
  network : string;
  history_mode : string;
  data_dir : string;
  rpc_addr : string;
  p2p_addr : string;
}

(** {1 Common Validators} *)

val is_nonempty : string -> bool
(** Check if string is non-empty after trimming *)

val normalize : string -> string
(** Lowercase and trim a string *)

val instance_in_use : states:Data.Service_state.t list -> string -> bool
(** Check if an instance name is already in use *)

val service_user_valid : user:string -> bool
(** Check if service user exists or can be created *)

val parse_host_port : string -> (string * int) option
(** Parse "host:port" string *)

val default_service_user : unit -> string
(** Get default service user (octez for root, current user otherwise) *)

(** {1 Helpers} *)

val prepare_extra_args : string -> string list
(** Parse extra args string into list, filtering empty strings *)
