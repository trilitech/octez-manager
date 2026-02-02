(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** RPC Browser actions.

    Action handlers for RPC Browser page interactions. *)

open Octez_manager_lib

(** {1 Entry Selection} *)

(** Get the currently selected entry (under cursor).
    @return Entry at cursor position, or None if list empty *)
val get_selected_entry :
  Rpc_browser_state.state -> Rpc_browser_state.entry option

(** {1 Navigation Actions} *)

(** Handle Enter key - navigate or execute based on entry kind.
    - SUB: navigate into subdirectory
    - GET: execute GET request
    - DYN: prompt for dynamic value *)
val handle_enter :
  Rpc_browser_state.state -> (Rpc_browser_state.state -> unit) -> unit

(** {1 RPC Execution} *)

(** Build full URL for RPC call.
    @param service Service with RPC endpoint
    @param path Path segments *)
val build_rpc_url : Service.t -> string list -> string

(** Execute GET request for current path.
    @param state Current state
    @param on_update Callback to update state *)
val execute_get :
  Rpc_browser_state.state -> (Rpc_browser_state.state -> unit) -> unit

(** {1 Dynamic Value Prompts} *)

(** Get smart default for dynamic segment.
    @param name Segment name (e.g., "chain_id", "block_id")
    @param typ Type hint from OpenAPI
    @return Default value *)
val default_for_dynamic : name:string -> typ:string -> string

(** Open modal to prompt for dynamic segment value.
    @param name Segment name
    @param typ Type hint
    @param state Current state
    @param on_value Callback with entered value *)
val prompt_dynamic :
  name:string ->
  typ:string ->
  Rpc_browser_state.state ->
  (string -> unit) ->
  unit

(** {1 Instance Cycling} *)

(** Cycle to next instance.
    @param delta Direction (+1 for next, -1 for previous) *)
val cycle_instance :
  delta:int -> Rpc_browser_state.state -> Rpc_browser_state.state

(** {1 Fetch Entries} *)

(** Fetch entries for current path synchronously. *)
val fetch_entries_sync : Rpc_browser_state.state -> Rpc_browser_state.state

(** Fetch entries for current path and call update callback. *)
val fetch_entries :
  Rpc_browser_state.state -> (Rpc_browser_state.state -> unit) -> unit
