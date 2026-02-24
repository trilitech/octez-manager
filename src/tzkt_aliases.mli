(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** tzkt delegate alias resolution and caching.

    Fetches delegate aliases from the tzkt API per network, caches in memory
    and persists to disk. The scheduling of refreshes is handled by the UI
    layer (e.g. a background scheduler calling {!refresh} periodically).

    Usage:
    - Call {!load} at startup to load disk caches
    - Call {!find} from render functions (fast, no I/O)
    - Call {!refresh} from a background scheduler *)

(** Look up the human-readable alias for a delegate PKH on a given network.
    Returns [None] if no alias is known. Fast, no I/O — reads from cache. *)
val find : network:string -> pkh:string -> string option

(** Force a refresh of aliases for a specific network.
    Fetches from tzkt API and updates both memory and disk caches.
    On API failure, falls back to disk cache if no memory cache exists. *)
val refresh : network:string -> unit

(** Load aliases from disk cache for a network. Call at startup
    to populate the in-memory cache before the first API fetch. *)
val load : network:string -> unit

(** Check if a network's cache needs refreshing (older than 6 hours
    or never fetched). Used by background schedulers. *)
val needs_refresh : network:string -> bool
