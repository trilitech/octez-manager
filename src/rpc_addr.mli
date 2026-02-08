(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Opaque wrapper for RPC addresses in [host:port] format.

    Prevents accidental confusion with instance names, network names,
    file paths, or other stringly-typed identifiers. *)

(** The abstract type for RPC addresses (e.g. ["127.0.0.1:8732"]). *)
type t

(** The default node RPC address: [127.0.0.1:8732]. *)
val default : t

(** The default DAL node RPC address: [127.0.0.1:10732]. *)
val default_dal : t

(** {2 Construction} *)

(** Wrap a raw string as an RPC address.
    No validation is performed — use {!Port_validation.validate_rpc_addr}
    at entry points (CLI, UI forms) for validation. *)
val of_string : string -> t

(** Unwrap to the underlying string. *)
val to_string : t -> string

(** {2 Structured access} *)

(** Extract the host part (e.g. ["127.0.0.1"]).
    Returns [None] if the address is not in [host:port] format. *)
val host : t -> string option

(** Extract the port number (e.g. [8732]).
    Returns [None] if the address is not in [host:port] format. *)
val port : t -> int option

(** {2 Endpoint conversion} *)

(** Convert to an HTTP endpoint URL.
    If the address already starts with ["http://"] or ["https://"],
    it is returned as-is. Otherwise, ["http://"] is prepended.
    Empty addresses fall back to ["http://127.0.0.1:8732"]. *)
val to_endpoint : t -> string

(** {2 Comparison} *)

val equal : t -> t -> bool

val compare : t -> t -> int

(** {2 Serialization} *)

val to_yojson : t -> Yojson.Safe.t

val of_yojson : Yojson.Safe.t -> (t, string) result

(** {2 Formatting} *)

val pp : Format.formatter -> t -> unit
