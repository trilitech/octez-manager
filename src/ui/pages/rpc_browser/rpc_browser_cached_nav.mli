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

(** Move cached cursor up. *)
val cursor_up : Rpc_browser_types.state -> Rpc_browser_types.state

(** Move cached cursor down. *)
val cursor_down : Rpc_browser_types.state -> Rpc_browser_types.state

(** Get the entry at the cached cursor position. *)
val get_entry : Rpc_browser_types.state -> Rpc_browser_types.entry option

(** Navigate to a child path while staying in Result mode.
    Updates path and clears cached entries (to be refetched). *)
val navigate : string -> Rpc_browser_types.state -> Rpc_browser_types.state

(** Set cached entries (used after fetching for Result mode browser). *)
val set_entries :
  Rpc_browser_types.entry list ->
  Rpc_browser_types.state ->
  Rpc_browser_types.state

(** Navigate up one level while staying in Result mode. *)
val navigate_up : Rpc_browser_types.state -> Rpc_browser_types.state
