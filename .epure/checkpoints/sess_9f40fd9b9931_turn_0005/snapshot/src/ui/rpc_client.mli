(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** RPC client for Octez nodes.

    Provides HTTP GET requests with curl/wget/octez-client fallback,
    caching, and concurrency limiting. *)

open Octez_manager_lib

(** {1 HTTP Requests} *)

(** Execute HTTP GET request to a service endpoint.
    Uses curl if available, falls back to wget, then octez-client.
    Respects concurrency limits (default: 2 concurrent requests).

    @param service The service to query
    @param path Request path, e.g., "/chains/main/blocks/head"
    @return Response body or error message *)
val http_get_string : Service.t -> string -> (string, string) result

(** Execute HTTP GET request with URL handling.
    If path starts with "http", treats it as a full URL.
    Otherwise, builds URL from service endpoint.

    @param service The service to query
    @param path Request path or full URL
    @return Response body or error message *)
val http_get_url : Service.t -> string -> (string, string) result

(** {1 URL Utilities} *)

(** Get the HTTP endpoint URL for a service.
    Prepends "http://" if not already prefixed. *)
val endpoint_of : Service.t -> string

(** Build absolute URL from service and path.
    Handles leading slash in path. *)
val absolutize_url : Service.t -> string -> string

(** {1 Tool Detection} *)

(** Check if curl is available. Result is cached. *)
val curl_available : unit -> bool

(** Check if wget is available. Result is cached. *)
val wget_available : unit -> bool

(** {1 Cached RPC Calls} *)

(** Get head block level. Cached with 3.5s TTL. *)
val rpc_head_header : Service.t -> int option

(** Get cached head block level without fetching. *)
val rpc_head_header_cached : Service.t -> int option

(** Get chain ID. Cached with 1 hour TTL. *)
val rpc_chain_id : Service.t -> string option

(** Get cached chain ID without fetching. *)
val rpc_chain_id_cached : Service.t -> string option

(** Get current protocol hash. Not cached. *)
val rpc_protocol : Service.t -> string option

(** Check if node is bootstrapped. Cached with 5.5s TTL. *)
val rpc_is_bootstrapped : Service.t -> bool option

(** Get cached bootstrap status without fetching. *)
val rpc_is_bootstrapped_cached : Service.t -> bool option

(** Get node version from binary. Cached with 1 hour TTL. *)
val node_version : Service.t -> string option

(** {1 Error Tracking} *)

(** Get last RPC error for a service, if any. *)
val rpc_last_error : Service.t -> string option

(** Clear cached error for a service. *)
val clear_error : Service.t -> unit

(** {1 Head Monitoring} *)

(** Handle for controlling a head monitor stream. *)
type monitor_handle = {stop : unit -> unit; alive : unit -> bool}

(** Start streaming head updates from a node.
    Runs as a fiber in the domain pool. Uses Eio.Process when available
    (TUI mode) for non-blocking I/O, falls back to blocking Unix I/O.

    @param service The node to monitor
    @param on_head Callback for each new head (level, protocol, chain_id)
    @param on_disconnect Callback when stream disconnects
    @return Handle to stop the monitor *)
val start_head_monitor :
  Service.t ->
  on_head:
    (level:int option -> proto:string option -> chain_id:string option -> unit) ->
  on_disconnect:(unit -> unit) ->
  monitor_handle

(** Start a generic RPC stream (for any streaming endpoint).
    Runs curl in the domain pool, calling [on_line] for each line received.
    Works for /monitor/*, /chains/*/mempool/*, etc.

    @param service The node to stream from
    @param path RPC path (e.g., "/monitor/heads/main")
    @param on_line Callback for each raw line received
    @param on_disconnect Callback when stream disconnects
    @return Handle to stop the stream *)
val start_rpc_stream :
  Service.t ->
  path:string ->
  on_line:(string -> unit) ->
  on_disconnect:(unit -> unit) ->
  monitor_handle

module For_tests : sig
  val try_fetch_methods :
    string option ->
    (unit -> (string, string) result option) list ->
    (string, string) result

  val octez_client_bin : Service.t -> string

  val with_request_slot : (unit -> 'a) -> 'a
end
