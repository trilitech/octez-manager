(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Cached cursor navigation for Result mode browser panel.

    When the RPC browser is in Result (side-by-side) mode, the browser panel
    maintains a cached list of entries and cursor position independent from the
    main list mode. These functions manage that cached navigation state. *)

open Rpc_browser_types

(** Move cached cursor up. *)
let cursor_up state =
  if state.cached_cursor > 0 then
    {state with cached_cursor = state.cached_cursor - 1}
  else state

(** Move cached cursor down. *)
let cursor_down state =
  if state.cached_cursor < List.length state.cached_entries - 1 then
    {state with cached_cursor = state.cached_cursor + 1}
  else state

(** Get the entry at the cached cursor position. *)
let get_entry state = List.nth_opt state.cached_entries state.cached_cursor

(** Navigate to a child path while staying in Result mode.
    Updates path and clears cached entries (to be refetched). *)
let navigate segment state =
  {
    state with
    path = state.path @ [segment];
    cached_entries = [];
    cached_cursor = 0;
  }

(** Set cached entries (used after fetching for Result mode browser). *)
let set_entries entries state =
  {state with cached_entries = entries; cached_cursor = 0}

(** Navigate up one level while staying in Result mode. *)
let navigate_up state =
  match state.path with
  | [] -> state
  | _ ->
      let new_path = List.rev (List.tl (List.rev state.path)) in
      {state with path = new_path; cached_entries = []; cached_cursor = 0}
