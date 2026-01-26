(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

(** Common types and utilities for form builder field bundles. *)

open Octez_manager_lib

(** {1 Cached Service States}

    Forms call validators frequently (on every render). To avoid repeated
    syscalls, we cache service states with a short TTL. *)

(** Get cached service states, refreshing cache if expired (blocking).
    Use for initial form setup and ~edit callbacks. *)
val cached_service_states : unit -> Data.Service_state.t list

(** Get cached service states without blocking.
    Returns stale data if cache is expired, empty list if cache is empty.
    Use in ~validate and ~validate_msg callbacks during typing. *)
val cached_service_states_nonblocking : unit -> Data.Service_state.t list

(** Force cache invalidation. Call after installing/removing services. *)
val invalidate_service_states_cache : unit -> unit

(** {1 Configuration Types} *)

(** Core service configuration shared by all Octez tools. *)
type core_service_config = {
  instance_name : string;
  service_user : string;
  app_bin_dir : string;
  bin_source : Octez_manager_lib.Binary_registry.bin_source option;
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

(** Check if octez-signer binary exists and is executable in the given directory. *)
val has_octez_signer_binary : string -> bool

(** Check if octez-dal-node binary exists and is executable in the given directory. *)
val has_octez_dal_node_binary : string -> bool

(** [binary_accessible_to_user ~user ~app_bin_dir ~binary_name] validates
    that [user] can execute [binary_name] in [app_bin_dir].
    This checks both existence and permission.
    In root mode, verifies the service user can access the binary.
    In user mode, checks if current user can access it.
    Uses caching (5s TTL) to avoid excessive subprocess calls. *)
val binary_accessible_to_user :
  user:string -> app_bin_dir:string -> binary_name:string -> bool

(** Add http:// scheme if missing, defaulting to localhost when empty. *)
val endpoint_with_scheme : string -> string

(** Compute endpoint string from a service rpc_addr, adding scheme when missing. *)
val endpoint_of_service : Service.t -> string

(** {1 Helpers} *)

(** Parse extra args string into list, filtering empty strings *)
val prepare_extra_args : string -> string list

(** Find the best default app_bin_dir for a given binary.

    Priority order:
    1. Use `which <binary>` to find system-installed binary
    2. Look in registered services for a directory containing the binary
    3. Fall back to /usr/bin

    @param binary_name The name of the binary to find (e.g., "octez-node")
    @return The directory containing the binary, or /usr/bin as fallback *)
val default_app_bin_dir : binary_name:string -> string

(** Parse shellwords-style arguments with quote support.

    Supports:
    - Single quotes: preserve everything literally
    - Double quotes: preserve spaces, allow escaping with backslash
    - Unquoted: split on spaces
    - Backslash escaping in double quotes and unquoted context *)
val parse_shellwords : string -> string list
