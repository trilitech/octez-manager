(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

(** Simple HTTP client for RPC requests.

    Wraps curl/wget commands with caching and timeout support. *)

(** {1 Requests} *)

(** Execute HTTP GET request.
    Uses curl if available, falls back to wget.

    @param rpc_addr RPC address, e.g., "127.0.0.1:8732" or "https://rpc.example.com"
    @param path Request path, e.g., "/chains/main/blocks/head"
    @param timeout Timeout in seconds (default: 2.0)
    @return Response body or error message *)
val get :
  rpc_addr:string ->
  path:string ->
  ?timeout:float ->
  unit ->
  (string, string) result

(** {1 Caching} *)

(** Get cached result if available and fresh.
    @param rpc_addr RPC address
    @param path Request path
    @param ttl Time-to-live in seconds
    @return Cached result if available and fresh *)
val get_cached : rpc_addr:string -> path:string -> ttl:float -> string option

(** Store result in cache.
    @param rpc_addr RPC address
    @param path Request path
    @param body Response body *)
val cache_put : rpc_addr:string -> path:string -> body:string -> unit

(** Clear entire cache. *)
val clear_cache : unit -> unit

(** {1 Utilities} *)

(** Build a full URL from RPC address and path.
    Handles both raw addresses and https:// prefixed addresses.

    @param rpc_addr RPC address
    @param path Request path
    @return Full URL *)
val build_url : rpc_addr:string -> path:string -> string

(** Check if curl is available.
    Cached after first check. *)
val has_curl : unit -> bool

(** Check if wget is available.
    Cached after first check. *)
val has_wget : unit -> bool
