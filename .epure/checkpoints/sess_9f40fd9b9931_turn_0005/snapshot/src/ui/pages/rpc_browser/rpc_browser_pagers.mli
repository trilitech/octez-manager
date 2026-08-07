(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Multi-pager management for the RPC Browser.

    Handles creation, removal, focus, and content updates for the 0-9 pager
    slots used in Result (side-by-side) mode. Each pager can target a different
    node instance and maintains its own response body, JSON fold state, and
    performance metrics. *)

open Octez_manager_lib
open Rpc_browser_types
module Pager = Miaou_widgets_display.Pager_widget

(** Create an empty pager slot with the given ID.
    @param target_instance Optional target node for this pager *)
val create_empty_pager : ?target_instance:Service.t option -> int -> pager_slot

(** Get the list of all pager IDs in the current state. *)
val get_pager_ids : state -> int list

(** Find the next available pager ID (0-9), or None if all slots used. *)
val next_available_id : state -> int option

(** Check if we're in Result mode. *)
val is_result_mode : state -> bool

(** Get the current result_focus. Returns FocusBrowser if not in Result mode. *)
val get_result_focus : state -> result_focus

(** Set the result focus. Only works in Result mode. *)
val set_result_focus : result_focus -> state -> state

(** Enter result mode with a single empty pager (pager 0).
    @param current_instance The currently selected instance (fallback target) *)
val enter_result_mode : current_instance:Service.t option -> state -> state

(** Add a new pager to the state. Focus moves to the new pager.
    Returns None if already at max (10) pagers.
    @param current_instance The currently selected instance (fallback target) *)
val add_pager : current_instance:Service.t option -> state -> state option

(** Remove a pager by ID.
    Returns None if only 1 pager remains or ID not found. *)
val remove_pager : int -> state -> state option

(** Focus a pager by ID. Returns unchanged state if ID not found. *)
val focus_pager : int -> state -> state

(** Set focus to browser in Result mode. *)
val focus_browser : state -> state

(** Get the currently focused pager slot, if any. *)
val get_focused_pager : state -> pager_slot option

(** Get the focused pager ID (returns 0 if focus is on browser). *)
val get_focused_pager_id : state -> int

(** Get the target instance for the focused pager. *)
val get_pager_target : state -> Service.t option

(** Set the target instance for the focused pager. *)
val set_pager_target : Service.t option -> state -> state

(** Get all pager slots in Result mode. *)
val get_pagers : state -> pager_slot list

(** Update a pager slot by ID using a transformation function. *)
val update_pager_slot : int -> (pager_slot -> pager_slot) -> state -> state

(** Set result in a specific pager slot.
    Creates foldable JSON from raw body and initializes the pager widget.
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

(** Execute a GET request on the focused pager (sets loading state).
    @param url Full URL being requested *)
val execute_get : url:string -> state -> state

(** Set result body after successful request.
    Uses the focused pager's request URL.
    @param raw_body Original response
    @param response_time_ms Optional request duration in milliseconds
    @param response_size Optional response body size in bytes *)
val set_result :
  raw_body:string ->
  ?response_time_ms:float ->
  ?response_size:int ->
  state ->
  state

(** Get the pager widget from the focused pager slot, if available. *)
val get_pager : state -> Pager.t option

(** Update the pager widget in the focused pager slot. *)
val set_pager : Pager.t -> state -> state

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
