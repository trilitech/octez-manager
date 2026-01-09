(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Port validation utilities shared between CLI and UI. *)

(** Parse a host:port string into (host, port) tuple. *)
val parse_host_port : string -> (string * int) option

(** Parse just the port from an address string. *)
val parse_port : string -> int option

(** Collect RPC and P2P ports from registered node services.
    Returns (port, instance_name) pairs.
    @param exclude_instance Optional instance to exclude from the list. *)
val ports_from_services :
  ?exclude_instance:string -> unit -> (int * string) list * (int * string) list

(** Check if a port is owned by a specific instance. *)
val port_owned_by_instance : instance:string -> int -> bool

(** Check if a port is in use by any running process. *)
val is_port_in_use : int -> bool

(** Validation error types. *)
type validation_error =
  | Invalid_format of string  (** Invalid host:port format *)
  | Port_out_of_range  (** Port not in 1024-65535 range *)
  | Used_by_other_instance of int * string  (** Port and instance name *)
  | Port_in_use of int * string option  (** Port and optional process name *)

(** Pretty-print a validation error. *)
val pp_error : validation_error -> string

(** Validate an address.
    @param addr The address in host:port format
    @param exclude_instance Optional instance to exclude from "in use" checks
    @param example Example format for error messages *)
val validate_addr :
  addr:string ->
  ?exclude_instance:string ->
  example:string ->
  unit ->
  (unit, validation_error) result

(** Validate an RPC address (example: 127.0.0.1:8732). *)
val validate_rpc_addr :
  ?exclude_instance:string -> string -> (unit, validation_error) result

(** Validate a P2P address (example: 0.0.0.0:9732). *)
val validate_p2p_addr :
  ?exclude_instance:string -> string -> (unit, validation_error) result
