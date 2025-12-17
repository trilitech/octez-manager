(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Common types and utilities for form builder field bundles. *)

open Octez_manager_lib

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

(** Check if string is non-empty after trimming *)
val is_nonempty : string -> bool

(** Lowercase and trim a string *)
val normalize : string -> string

(** Check if an instance name is already in use *)
val instance_in_use : states:Data.Service_state.t list -> string -> bool

(** Check if service user exists or can be created *)
val service_user_valid : user:string -> bool

(** Parse "host:port" string *)
val parse_host_port : string -> (string * int) option

(** Get default service user (octez for root, current user otherwise) *)
val default_service_user : unit -> string

val default_base_dir : role:string -> instance:string -> string

(** [has_binary binary_name dir] checks if [binary_name] exists in [dir]
    and is executable. *)
val has_binary : string -> string -> bool

(** Check if octez-baker binary exists and is executable in the given directory. *)
val has_octez_baker_binary : string -> bool

(** Check if octez-node binary exists and is executable in the given directory. *)
val has_octez_node_binary : string -> bool

(** Add http:// scheme if missing, defaulting to localhost when empty. *)
val endpoint_with_scheme : string -> string

(** Compute endpoint string from a service rpc_addr, adding scheme when missing. *)
val endpoint_of_service : Service.t -> string

(** {1 Helpers} *)

(** Parse extra args string into list, filtering empty strings *)
val prepare_extra_args : string -> string list
