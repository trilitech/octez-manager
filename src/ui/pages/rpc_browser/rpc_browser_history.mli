(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Dynamic value history and recent paths LRU for the RPC Browser.

    Manages the in-memory history of user-provided dynamic segment values
    and the LRU list of recently used RPC paths, with persistence via
    {!Rpc_browser_persistence}. *)

(** Maximum number of recent paths to keep. *)
val max_recent_paths : int

(** Add a dynamic value to history.
    Deduplicates by segment_type+value and caps at 50 entries.
    Persists to disk.
    @param segment_type Type of segment (e.g., "chain_id", "block_id")
    @param value The user-provided value *)
val add_dynamic_value :
  segment_type:string ->
  value:string ->
  Rpc_browser_types.state ->
  Rpc_browser_types.state

(** Get recent values for a segment type, deduped and capped at 10. *)
val get_recent_values :
  segment_type:string -> Rpc_browser_types.state -> string list

(** Add a path to the recent paths LRU list.
    Deduplicates by path and caps at {!max_recent_paths}.
    Persists to disk.
    @param path The RPC path (e.g., "/chains/main/blocks/head")
    @param desc Description for display *)
val add_recent_path :
  path:string ->
  desc:string ->
  Rpc_browser_types.state ->
  Rpc_browser_types.state

(** Get recent paths sorted by timestamp (most recent first). *)
val get_recent_paths :
  Rpc_browser_types.state -> Rpc_browser_types.recent_path list
