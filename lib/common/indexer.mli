(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Centralised TzKT routing with local-indexer support and round-robin
    fallback.

    This is the {b only} module in the codebase that may embed
    ["api.tzkt.io"] URL literals. All other modules must use {!fetch}
    or {!tzkt_base_url} instead of constructing TzKT URLs directly. *)

(** Canonical public TzKT base URL for [network].
    Returns ["https://api.tzkt.io"] for ["mainnet"] and
    ["https://api.{network}.tzkt.io"] for other networks. *)
val tzkt_base_url : network:string -> string

(** Register a local indexer base URL for [network].
    Appended to the round-robin pool; call again to add another entry. *)
val register_local : network:string -> base_url:string -> unit

(** Remove all locally registered endpoints for [network]. *)
val clear_local : network:string -> unit

(** Fetch [path] from the best available indexer for [network].

    Priority: [preferred_base] → local endpoints (round-robin) → TzKT.
    An empty response body is treated as failure and the next source is
    tried. Returns the first non-empty successful response.

    @param network       Network name, e.g. ["mainnet"] or ["ghostnet"].
    @param preferred_base  Optional override tried before local endpoints
                           (e.g. [payout_config.tzkt_url]).
    @param timeout       Per-request timeout in seconds (default 15.0). *)
val fetch :
  network:string ->
  ?preferred_base:string ->
  ?timeout:float ->
  string ->
  (string, [`Msg of string]) result

(** Set the info-level logger.
    Called when a custom indexer source is in use.
    Default: no-op. *)
val set_log_info : (string -> unit) -> unit

(** Set the warn-level logger.
    Called on fallback to public TzKT after a custom source failure,
    and on indexer divergence in debug mode.
    Default: no-op. *)
val set_log_warn : (string -> unit) -> unit

(** Set a callback invoked on divergence between a custom source and
    public TzKT in debug mode.  Arguments: [path], [custom_body],
    [tzkt_body].  Default: logs a one-line warning via {!set_log_warn}. *)
val set_on_divergence : (string -> string -> string -> unit) -> unit

(** Enable or disable debug mode.
    When enabled, {!fetch} also queries TzKT whenever a local or preferred
    source is used successfully, and calls the divergence callback if the
    responses differ. *)
val set_debug_mode : bool -> unit

(** Query every distinct registered source for [path] and return labelled
    results.

    Returns one entry per distinct source base URL:
    [(label, Ok body)] on success, [(label, Error msg)] on failure. *)
val query_all :
  network:string -> string -> (string * (string, string) result) list

(**/**)

(** Internal helpers exposed for unit testing only. *)
module Internal_for_tests : sig
  (** Override the HTTP backend (avoids real network calls in tests). *)
  val set_http_fn :
    (url:string -> timeout:float -> (string, [`Msg of string]) result) -> unit

  (** Restore the default curl-based HTTP backend. *)
  val reset_http_fn : unit -> unit

  (** Return the currently registered local endpoints for [network]. *)
  val get_local_endpoints : network:string -> string list

  (** Override the warn-logging function (to capture log calls in tests). *)
  val set_log_fn : (string -> unit) -> unit

  (** Restore the default no-op warn-logging function. *)
  val reset_log_fn : unit -> unit
end

(**/**)
