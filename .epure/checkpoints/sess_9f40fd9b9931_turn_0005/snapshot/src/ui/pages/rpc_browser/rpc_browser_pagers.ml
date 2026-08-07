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

open Rpc_browser_types
module Pager = Miaou_widgets_display.Pager_widget
module Style_context = Miaou_style.Style_context

let with_current_theme f =
  Style_context.with_theme (Theme_manager.get_current ()) f

(** Create an empty pager slot with the given ID.
    @param target_instance Optional target node for this pager *)
let create_empty_pager ?(target_instance = None) id =
  {
    id;
    request = "";
    body = "";
    raw_body = "";
    pager = None;
    foldable = None;
    response_time_ms = None;
    response_size = None;
    target_instance;
    streaming_handle = None;
  }

(** Get the list of all pager IDs in the current state. *)
let get_pager_ids state =
  match state.mode with
  | Result {pagers; _} -> List.map (fun p -> p.id) pagers
  | List _ -> []

(** Find the next available pager ID (0-9), or None if all slots used. *)
let next_available_id state =
  let used_ids = get_pager_ids state in
  let rec find_id n =
    if n > 9 then None
    else if List.mem n used_ids then find_id (n + 1)
    else Some n
  in
  find_id 0

(** Check if we're in Result mode. *)
let is_result_mode state =
  match state.mode with Result _ -> true | List _ -> false

(** Get the current result_focus. Returns FocusBrowser if not in Result mode. *)
let get_result_focus state =
  match state.mode with Result {focus; _} -> focus | List _ -> FocusBrowser

(** Set the result focus. Only works in Result mode. *)
let set_result_focus focus state =
  match state.mode with
  | Result r -> {state with mode = Result {r with focus}}
  | List _ -> state

(** Enter result mode with a single empty pager (pager 0).
    @param current_instance The currently selected instance (fallback target) *)
let enter_result_mode ~current_instance state =
  (* Use target_override if set, else current instance *)
  let target =
    match state.target_override with
    | Some _ as t -> t
    | None -> current_instance
  in
  let pager0 = create_empty_pager ~target_instance:target 0 in
  {
    state with
    mode = Result {pagers = [pager0]; focus = FocusPager 0; last_pager_id = 0};
  }

(** Add a new pager to the state. Focus moves to the new pager.
    Returns None if already at max (10) pagers.
    @param current_instance The currently selected instance (fallback target) *)
let add_pager ~current_instance state =
  match state.mode with
  | Result {pagers; _} -> (
      if List.length pagers >= 10 then None
      else
        match next_available_id state with
        | None -> None
        | Some new_id ->
            (* Use target_override if set, else current instance *)
            let target =
              match state.target_override with
              | Some _ as t -> t
              | None -> current_instance
            in
            let new_pager = create_empty_pager ~target_instance:target new_id in
            Some
              {
                state with
                mode =
                  Result
                    {
                      pagers = pagers @ [new_pager];
                      focus = FocusPager new_id;
                      last_pager_id = new_id;
                    };
              })
  | List _ -> None

(** Remove a pager by ID. Returns None if only 1 pager remains or ID not
    found. Stops any active streaming connection on the removed pager. *)
let remove_pager id state =
  match state.mode with
  | Result {pagers; focus; last_pager_id} ->
      if List.length pagers <= 1 then None
      else (
        (* Stop streaming on the pager being removed *)
        List.iter
          (fun p ->
            if p.id = id then
              match p.streaming_handle with
              | Some h -> h.Rpc_client.stop ()
              | None -> ())
          pagers ;
        let new_pagers = List.filter (fun p -> p.id <> id) pagers in
        if List.length new_pagers = List.length pagers then None
          (* id not found *)
        else
          let new_focus : result_focus =
            match (focus : result_focus) with
            | FocusPager n when n = id -> (
                (* Find the next pager to focus *)
                match new_pagers with
                | [] -> FocusBrowser
                | first :: _ -> FocusPager first.id)
            | _ -> focus
          in
          let new_last_pager_id =
            if last_pager_id = id then
              match new_pagers with [] -> 0 | first :: _ -> first.id
            else last_pager_id
          in
          Some
            {
              state with
              mode =
                Result
                  {
                    pagers = new_pagers;
                    focus = new_focus;
                    last_pager_id = new_last_pager_id;
                  };
            })
  | List _ -> None

(** Focus a pager by ID. Returns unchanged state if ID not found. *)
let focus_pager id state =
  match state.mode with
  | Result {pagers; _} ->
      if List.exists (fun p -> p.id = id) pagers then
        {
          state with
          mode =
            Result
              {
                pagers;
                focus = (FocusPager id : result_focus);
                last_pager_id = id;
              };
        }
      else state
  | List _ -> state

(** Set focus to browser in Result mode. *)
let focus_browser state =
  match state.mode with
  | Result r ->
      {state with mode = Result {r with focus = (FocusBrowser : result_focus)}}
  | List _ -> state

(** Get the currently focused pager slot, if any. *)
let get_focused_pager state =
  match state.mode with
  | Result {pagers; focus = FocusPager id; _} ->
      List.find_opt (fun p -> p.id = id) pagers
  | Result {pagers; focus = FocusBrowser; last_pager_id; _} ->
      (* When browser is focused, return last focused pager *)
      List.find_opt (fun p -> p.id = last_pager_id) pagers
  | List _ -> None

(** Get the focused pager ID (returns 0 if focus is on browser). *)
let get_focused_pager_id state =
  match state.mode with
  | Result {focus = FocusPager id; _} -> id
  | Result {focus = FocusBrowser; last_pager_id; _} -> last_pager_id
  | List _ -> 0

(** Get the target instance for the focused pager. *)
let get_pager_target state =
  match state.mode with
  | List _ -> state.target_override
  | Result _ -> (
      match get_focused_pager state with
      | Some pager -> (
          (* Pager target if set, else fall back to global target_override *)
          match pager.target_instance with
          | Some _ as t -> t
          | None -> state.target_override)
      | None -> state.target_override)

(** Set the target instance for the focused pager. *)
let set_pager_target target state =
  match state.mode with
  | Result {pagers; focus; last_pager_id} ->
      let pager_id = get_focused_pager_id state in
      let new_pagers =
        List.map
          (fun p ->
            if p.id = pager_id then {p with target_instance = target} else p)
          pagers
      in
      {
        state with
        mode = Result {pagers = new_pagers; focus; last_pager_id};
        target_override = target;
      }
  | List _ -> {state with target_override = target}

(** Get all pager slots in Result mode. *)
let get_pagers state =
  match state.mode with Result {pagers; _} -> pagers | List _ -> []

(** Update a pager slot by ID using a transformation function. *)
let update_pager_slot id f state =
  match state.mode with
  | Result ({pagers; _} as r) ->
      let new_pagers =
        List.map (fun p -> if p.id = id then f p else p) pagers
      in
      {state with mode = Result {r with pagers = new_pagers}}
  | List _ -> state

(** Set result in a specific pager slot.
    Creates foldable JSON from raw body and initializes the pager widget.
    @param pager_id Target pager ID
    @param request The request URL
    @param body Formatted/highlighted body
    @param raw_body Original response
    @param response_time_ms Optional request duration in milliseconds
    @param response_size Optional response body size in bytes *)
let set_pager_result ~pager_id ~request ~raw_body ?response_time_ms
    ?response_size state =
  (* Create foldable JSON from raw body *)
  let foldable = Foldable_json.of_string raw_body in
  (* Use foldable render if available, otherwise highlight raw JSON.
     Always render with the current theme to avoid cross-pager color drift. *)
  let display_body =
    with_current_theme (fun () ->
        match foldable with
        | Some f -> Foldable_json.render f
        | None -> (
            match Json_highlighter.highlight raw_body with
            | Ok h -> h
            | Error _ -> raw_body))
  in
  (* Create pager from rendered content *)
  let pager = Pager.open_text ~title:"Response" display_body in
  (* Enable cursor mode when foldable JSON is available for fold/unfold *)
  let pager =
    match foldable with
    | Some _ -> Pager.set_cursor_mode pager true
    | None -> pager
  in
  update_pager_slot
    pager_id
    (fun slot ->
      {
        slot with
        request;
        body = display_body;
        raw_body;
        pager = Some pager;
        foldable;
        response_time_ms;
        response_size;
      })
    state

(** Execute a GET request on the focused pager (sets loading state).
    Stops any active streaming on the pager before starting.
    @param url Full URL being requested *)
let execute_get ~url state =
  match state.mode with
  | Result {pagers; last_pager_id; _} ->
      let pager_id = get_focused_pager_id state in
      (* Stop any active streaming on this pager *)
      List.iter
        (fun p ->
          if p.id = pager_id then (
            (match p.streaming_handle with
            | Some h -> h.Rpc_client.stop ()
            | None -> ()) ;
            match p.pager with Some pg -> Pager.stop_streaming pg | None -> ()))
        pagers ;
      (* Update the focused pager to show loading state *)
      let new_pagers =
        List.map
          (fun p ->
            if p.id = pager_id then
              {
                p with
                request = url;
                body = "Loading...";
                raw_body = "";
                pager = None;
                foldable = None;
                response_time_ms = None;
                response_size = None;
                streaming_handle = None;
              }
            else p)
          pagers
      in
      {
        state with
        mode =
          Result
            {pagers = new_pagers; focus = FocusPager pager_id; last_pager_id};
        error = None;
      }
  | List _ ->
      (* Enter result mode with a single pager *)
      let pager0 =
        {(create_empty_pager 0) with request = url; body = "Loading..."}
      in
      {
        state with
        mode =
          Result {pagers = [pager0]; focus = FocusPager 0; last_pager_id = 0};
        error = None;
      }

(** Set result body after successful request.
    Uses the focused pager's request URL. *)
let set_result ~raw_body ?response_time_ms ?response_size state =
  match state.mode with
  | Result _ ->
      let pager_id = get_focused_pager_id state in
      let focused_pager = get_focused_pager state in
      let request =
        match focused_pager with Some p -> p.request | None -> ""
      in
      set_pager_result
        ~pager_id
        ~request
        ~raw_body
        ?response_time_ms
        ?response_size
        state
  | List _ -> state

(** Get the pager widget from the focused pager slot, if available. *)
let get_pager state =
  match get_focused_pager state with Some slot -> slot.pager | None -> None

(** Update the pager widget in the focused pager slot. *)
let set_pager pager state =
  match state.mode with
  | Result _ ->
      let pager_id = get_focused_pager_id state in
      update_pager_slot
        pager_id
        (fun slot -> {slot with pager = Some pager})
        state
  | List _ -> state

(** Stop streaming on a specific pager slot by ID. *)
let stop_streaming_pager pager_id state =
  update_pager_slot
    pager_id
    (fun slot ->
      (match slot.streaming_handle with
      | Some h -> h.Rpc_client.stop ()
      | None -> ()) ;
      (match slot.pager with Some p -> Pager.stop_streaming p | None -> ()) ;
      {slot with streaming_handle = None})
    state

(** Stop all active streaming connections across all pagers. *)
let stop_all_streaming state =
  match state.mode with
  | Result {pagers; _} -> (
      List.iter
        (fun p ->
          (match p.streaming_handle with
          | Some h -> h.Rpc_client.stop ()
          | None -> ()) ;
          match p.pager with Some pg -> Pager.stop_streaming pg | None -> ())
        pagers ;
      let new_pagers =
        List.map (fun p -> {p with streaming_handle = None}) pagers
      in
      match state.mode with
      | Result r -> {state with mode = Result {r with pagers = new_pagers}}
      | List _ -> state)
  | List _ -> state

(** Check if the focused pager has an active streaming connection. *)
let is_streaming state =
  match get_focused_pager state with
  | Some slot -> (
      match slot.streaming_handle with
      | Some h -> h.Rpc_client.alive ()
      | None -> false)
  | None -> false

(** Highlight a streaming JSON line and append it (with separator) to a pager. *)
let append_streaming_line pager line =
  let highlighted =
    with_current_theme (fun () ->
        match Json_highlighter.highlight line with Ok h -> h | Error _ -> line)
  in
  let lines = String.split_on_char '\n' highlighted in
  (* Add a blank separator line between JSON objects for readability *)
  Pager.append_lines_batched pager (lines @ [""])

(** Set up a streaming pager: create pager in streaming mode, start RPC stream,
    wire on_line to highlight each JSON object and append to pager.
    @param pager_id Target pager ID
    @param request The request URL for display
    @param service The target service to stream from
    @param rpc_path The RPC path to stream
    @param on_state_update Callback to update global state ref and trigger re-render *)
let start_streaming_pager ~pager_id ~request ~service ~rpc_path ~on_state_update
    state =
  let pager =
    Pager.open_text
      ~title:"Streaming"
      ~notify_render:(fun () -> Context.mark_instances_dirty ())
      ""
  in
  Pager.start_streaming pager ;
  let state =
    update_pager_slot
      pager_id
      (fun slot ->
        {
          slot with
          request;
          body = "Streaming...";
          raw_body = "";
          pager = Some pager;
          foldable = None;
          response_time_ms = None;
          response_size = None;
        })
      state
  in
  let handle =
    Rpc_client.start_rpc_stream
      service
      ~path:rpc_path
      ~on_line:(append_streaming_line pager)
      ~on_disconnect:(fun () ->
        Pager.stop_streaming pager ;
        Context.mark_instances_dirty ())
  in
  let state =
    update_pager_slot
      pager_id
      (fun slot -> {slot with streaming_handle = Some handle})
      state
  in
  on_state_update state ;
  state
