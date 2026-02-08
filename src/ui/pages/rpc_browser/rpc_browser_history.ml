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

open Rpc_browser_types

(** Maximum number of recent paths to keep. *)
let max_recent_paths = 5

(** Add a dynamic value to history.
    Deduplicates by segment_type+value and caps at 50 entries.
    Persists to disk. *)
let add_dynamic_value ~segment_type ~value state =
  let now = Unix.gettimeofday () in
  let new_entry = {segment_type; value; timestamp = now} in
  (* Remove older entries for same type/value, keep max 50 entries *)
  let filtered =
    List.filter
      (fun dv -> not (dv.segment_type = segment_type && dv.value = value))
      state.dynamic_history
  in
  let new_history =
    new_entry :: filtered |> fun lst ->
    if List.length lst > 50 then List.filteri (fun i _ -> i < 50) lst else lst
  in
  Rpc_browser_persistence.save_dynamic_history new_history ;
  {state with dynamic_history = new_history}

(** Get recent values for a segment type, deduped and capped at 10. *)
let get_recent_values ~segment_type state =
  state.dynamic_history
  |> List.filter (fun dv -> dv.segment_type = segment_type)
  |> List.sort (fun a b -> compare b.timestamp a.timestamp)
  |> List.map (fun dv -> dv.value)
  |> fun lst ->
  (* Dedupe while preserving order *)
  let seen = Hashtbl.create 16 in
  List.filter
    (fun v ->
      if Hashtbl.mem seen v then false
      else (
        Hashtbl.add seen v () ;
        true))
    lst
  |> fun lst ->
  if List.length lst > 10 then List.filteri (fun i _ -> i < 10) lst else lst

(** Add a path to the recent paths LRU list.
    Deduplicates by path and caps at {!max_recent_paths}.
    Persists to disk. *)
let add_recent_path ~path ~desc state =
  let now = Unix.gettimeofday () in
  let new_entry = {rp_path = path; rp_desc = desc; rp_timestamp = now} in
  (* Remove existing entry for same path, then prepend *)
  let filtered =
    List.filter (fun rp -> rp.rp_path <> path) state.recent_paths
  in
  let new_paths =
    new_entry :: filtered |> fun lst ->
    if List.length lst > max_recent_paths then
      List.filteri (fun i _ -> i < max_recent_paths) lst
    else lst
  in
  Rpc_browser_persistence.save_recent_paths new_paths ;
  {state with recent_paths = new_paths}

(** Get recent paths sorted by timestamp (most recent first). *)
let get_recent_paths state =
  state.recent_paths
  |> List.sort (fun a b -> compare b.rp_timestamp a.rp_timestamp)
  |> fun lst ->
  if List.length lst > max_recent_paths then
    List.filteri (fun i _ -> i < max_recent_paths) lst
  else lst
