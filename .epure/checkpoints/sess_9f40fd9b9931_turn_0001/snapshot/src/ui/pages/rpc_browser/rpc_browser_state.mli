(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** RPC Browser state management.

    Pure functional state transformations for the RPC Browser page. *)

open Octez_manager_lib

include module type of Rpc_browser_types

(** Get public nodes from cache (fetches from Taquito if needed) *)
val public_nodes : unit -> Service.t list

(** {1 Selected Instance Override} *)

(** Set the selected instance override.
    Used by rpc_node_selection to pass the selected node to the RPC browser. *)
val set_selected_instance : Service.t option -> unit

(** Get the selected instance override. *)
val get_selected_instance : unit -> Service.t option

(** Clear the selected instance override. *)
val clear_selected_instance : unit -> unit

(** {1 Initialization} *)

(** Create initial state.
    @param instances List of services with RPC endpoints *)
val init : instances:Service.t list -> state

(** {1 Instance Selection} *)

(** Select an instance by index.
    Returns unchanged state if index out of bounds. *)
val select_instance : int -> state -> state

(** Get the currently selected instance, if any. *)
val current_instance : state -> Service.t option

(** Get all available instances. *)
val get_instances : state -> Service.t list

(** Get all available target instances: public nodes + local instances *)
val get_all_targets : state -> Service.t list

(** {1 Navigation} *)

(** Navigate into a child path.
    Clears current entries and sets loading. *)
val navigate_to : string -> state -> state

(** Navigate up one level in the path.
    Returns to root if at top level. *)
val navigate_up : state -> state

(** Navigate to root path. *)
val navigate_root : state -> state

(** {1 Entry Updates} *)

(** Set entries for current path.
    Clears loading state and error. *)
val set_entries : entry list -> state -> state

(** Set loading state for entries. *)
val set_loading : bool -> state -> state

(** {1 Pager Management} *)

(** Create an empty pager slot with the given ID.
    @param target_instance Optional target node for this pager *)
val create_empty_pager : ?target_instance:Service.t option -> int -> pager_slot

(** Set the target instance for the focused pager. *)
val set_pager_target : Service.t option -> state -> state

(** Get the target instance for the focused pager. *)
val get_pager_target : state -> Service.t option

(** Add a new pager to the state. Focus moves to the new pager.
    Returns None if already at max (10) pagers. *)
val add_pager : state -> state option

(** Remove a pager by ID. Returns None if only 1 pager remains or ID not found. *)
val remove_pager : int -> state -> state option

(** Focus a pager by ID. Returns unchanged state if ID not found. *)
val focus_pager : int -> state -> state

(** Get the currently focused pager slot, if any. *)
val get_focused_pager : state -> pager_slot option

(** Get the focused pager ID (returns 0 if focus is on browser). *)
val get_focused_pager_id : state -> int

(** Set result in a specific pager slot.
    @param pager_id Target pager ID
    @param request The request URL
    @param raw_body Original response
    @param response_time_ms Optional request duration in milliseconds
    @param response_size Optional response body size in bytes *)
val set_pager_result :
  pager_id:int ->
  request:string ->
  raw_body:string ->
  ?response_time_ms:float ->
  ?response_size:int ->
  state ->
  state

(** Find the next available pager ID (0-9), or None if all slots used. *)
val next_available_id : state -> int option

(** Get the list of all pager IDs in the current state. *)
val get_pager_ids : state -> int list

(** Check if we're in Result mode. *)
val is_result_mode : state -> bool

(** Get the current result_focus. Returns FocusBrowser if not in Result mode. *)
val get_result_focus : state -> result_focus

(** Set the result focus. Only works in Result mode. *)
val set_result_focus : result_focus -> state -> state

(** {1 Result Mode} *)

(** Enter result mode with a single empty pager (pager 0).
    Focus is set to pager 0. *)
val enter_result_mode : state -> state

(** Execute a GET request on the focused pager (sets loading state).
    @param url Full URL being requested *)
val execute_get : url:string -> state -> state

(** Set result body after successful request.
    @param raw_body Original response
    @param response_time_ms Optional request duration in milliseconds
    @param response_size Optional response body size in bytes *)
val set_result :
  raw_body:string ->
  ?response_time_ms:float ->
  ?response_size:int ->
  state ->
  state

(** {1 Cursor Movement} *)

(** Move cursor up in list mode. *)
val cursor_up : state -> state

(** Move cursor down in list mode. *)
val cursor_down : state -> state

(** Scroll result view. *)
val scroll : int -> state -> state

(** {1 Errors} *)

(** Set error message. *)
val set_error : string -> state -> state

(** Clear error message. *)
val clear_error : state -> state

(** {1 OpenAPI Status} *)

(** Set OpenAPI loading status. *)
val set_openapi_status : openapi_status -> state -> state

(** {1 Focus} *)

(** Switch focus between browser and pager in side-by-side mode.
    When switching to pager, focuses pager 0. *)
val toggle_focus : state -> state

(** Set focus to browser in Result mode. *)
val focus_browser : state -> state

(** {1 Pager} *)

(** Get the pager from the focused pager slot, if available. *)
val get_pager : state -> Miaou_widgets_display.Pager_widget.t option

(** Update the pager in the focused pager slot. *)
val set_pager : Miaou_widgets_display.Pager_widget.t -> state -> state

(** Get all pager slots in Result mode. *)
val get_pagers : state -> pager_slot list

(** {1 Dynamic Value History} *)

(** Add a dynamic value to history.
    @param segment_type Type of segment (e.g., "chain_id", "block_id")
    @param value The user-provided value *)
val add_dynamic_value : segment_type:string -> value:string -> state -> state

(** Get recent values for a segment type. *)
val get_recent_values : segment_type:string -> state -> string list

(** Load dynamic history from disk. *)
val load_dynamic_history : unit -> dynamic_value list

(** Save dynamic history to disk. *)
val save_dynamic_history : dynamic_value list -> unit

(** {1 JSON Folding} *)

(** Toggle fold at a specific line in the JSON view of the focused pager.
    @param line Line number to toggle fold at *)
val toggle_fold : line:int -> state -> state

(** Unfold all JSON sections in the focused pager. *)
val unfold_all_json : state -> state

(** Fold all JSON sections in the focused pager. *)
val fold_all_json : state -> state

(** {1 Cached Cursor for Result Mode} *)

(** Move cached cursor up in result mode browser panel. *)
val cached_cursor_up : state -> state

(** Move cached cursor down in result mode browser panel. *)
val cached_cursor_down : state -> state

(** Get the entry at the cached cursor position. *)
val get_cached_entry : state -> entry option

(** Navigate to a child path while staying in Result mode.
    Updates path and clears cached entries (to be refetched). *)
val navigate_cached : string -> state -> state

(** Set cached entries (used after fetching for Result mode browser). *)
val set_cached_entries : entry list -> state -> state

(** Navigate up one level while staying in Result mode. *)
val navigate_cached_up : state -> state

(** {1 Recent Paths LRU} *)

(** Add a path to the recent paths LRU list.
    @param path The RPC path (e.g., "/chains/main/blocks/head")
    @param desc Description for display *)
val add_recent_path : path:string -> desc:string -> state -> state

(** Get recent paths sorted by timestamp (most recent first). *)
val get_recent_paths : state -> recent_path list

(** Load recent paths from disk. *)
val load_recent_paths : unit -> recent_path list

(** Save recent paths to disk. *)
val save_recent_paths : recent_path list -> unit

(** {1 Streaming} *)

(** Stop streaming on a specific pager slot by ID. *)
val stop_streaming_pager : int -> state -> state

(** Stop all active streaming connections across all pagers. *)
val stop_all_streaming : state -> state

(** Check if the focused pager has an active streaming connection. *)
val is_streaming : state -> bool

(** Set up a streaming pager: create pager in streaming mode, start RPC stream,
    wire on_line to feed JSON streamer and append to pager.
    @param pager_id Target pager ID
    @param request The request URL for display
    @param service The target service to stream from
    @param rpc_path The RPC path to stream
    @param on_state_update Callback to update global state ref and trigger re-render *)
val start_streaming_pager :
  pager_id:int ->
  request:string ->
  service:Service.t ->
  rpc_path:string ->
  on_state_update:(state -> unit) ->
  state ->
  state
