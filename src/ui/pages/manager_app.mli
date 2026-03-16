(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

(** Register all TUI pages with the page registry. *)
val register_pages : unit -> unit

(** Register pages and initialize the runtime (schedulers, logging, etc.).
    @param log Enable Miaou debug logging (default [false]).
    @param logfile Path to log file. *)
val register_and_init : ?log:bool -> ?logfile:string -> unit -> unit

(** Gracefully shut down background schedulers and clean up resources. *)
val shutdown : unit -> unit

(** Start the TUI application.
    @param page Initial page to display (defaults to the instances page).
    @param log Enable Miaou debug logging.
    @param logfile Path to log file.
    @param theme Theme name or path.
    @param local_indexer Register a local indexer endpoint (e.g.
      ["http://localhost:5000"]).  The URL is registered for
      [indexer_network] (default ["mainnet"]).
    @param compare_indexers When [true] and a local indexer is
      registered, every fetch also queries public TzKT and logs
      divergences. *)
val run :
  ?page:string ->
  ?log:bool ->
  ?logfile:string ->
  ?theme:string ->
  ?local_indexer:string ->
  ?indexer_network:string ->
  ?compare_indexers:bool ->
  unit ->
  (unit, [> `Msg of string]) result
