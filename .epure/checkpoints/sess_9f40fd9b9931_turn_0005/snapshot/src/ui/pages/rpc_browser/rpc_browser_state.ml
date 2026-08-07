(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Octez_manager_lib
include Rpc_browser_types
module Pager = Miaou_widgets_display.Pager_widget

(** Get public nodes from the shared cache (fetches from Taquito if needed) *)
let public_nodes () : Service.t list = Public_nodes_cache.get_services ()

(* Selected instance override - set by rpc_node_selection page *)
let selected_instance_override : Service.t option ref = ref None

let set_selected_instance inst = selected_instance_override := inst

let get_selected_instance () = !selected_instance_override

let clear_selected_instance () = selected_instance_override := None

(* Persistence delegations *)
let load_recent_paths = Rpc_browser_persistence.load_recent_paths

let save_recent_paths = Rpc_browser_persistence.save_recent_paths

let load_dynamic_history = Rpc_browser_persistence.load_dynamic_history

let save_dynamic_history = Rpc_browser_persistence.save_dynamic_history

let init ~instances =
  let dynamic_history = load_dynamic_history () in
  let recent_paths = load_recent_paths () in
  {
    instances;
    selected_idx = 0;
    path = [];
    mode = List {entries = []; cursor = 0; loading = true};
    openapi_status = NotAvailable;
    error = None;
    dynamic_history;
    recent_paths;
    cached_entries = [];
    cached_cursor = 0;
    target_override = None;
  }

let select_instance idx state =
  if idx < 0 || idx >= List.length state.instances then state
  else
    {
      state with
      selected_idx = idx;
      path = [];
      mode = List {entries = []; cursor = 0; loading = true};
      error = None;
      cached_entries = [];
      cached_cursor = 0;
    }

let current_instance state = List.nth_opt state.instances state.selected_idx

let get_instances state = state.instances

(** Get all available target instances: public nodes + local instances *)
let get_all_targets state =
  let local = state.instances in
  let public = public_nodes () in
  (* Combine, avoiding duplicates by rpc_addr *)
  let local_addrs = List.map (fun s -> s.Service.rpc_addr) local in
  let unique_public =
    List.filter (fun s -> not (List.mem s.Service.rpc_addr local_addrs)) public
  in
  (* Local instances first, then public *)
  local @ unique_public

let navigate_to segment state =
  {
    state with
    path = state.path @ [segment];
    mode = List {entries = []; cursor = 0; loading = true};
    error = None;
  }

let navigate_up state =
  match state.path with
  | [] -> state
  | _ :: [] ->
      {
        state with
        path = [];
        mode = List {entries = []; cursor = 0; loading = true};
        error = None;
      }
  | _ ->
      let new_path = List.rev (List.tl (List.rev state.path)) in
      {
        state with
        path = new_path;
        mode = List {entries = []; cursor = 0; loading = true};
        error = None;
      }

let navigate_root state =
  {
    state with
    path = [];
    mode = List {entries = []; cursor = 0; loading = true};
    error = None;
  }

let set_entries entries state =
  match state.mode with
  | List m ->
      let len = List.length entries in
      let cursor = if len = 0 then 0 else min m.cursor (len - 1) in
      {
        state with
        mode = List {entries; cursor; loading = false};
        error = None;
        cached_entries = entries;
        cached_cursor = cursor;
      }
  | Result _ -> state

let set_loading loading state =
  match state.mode with
  | List m -> {state with mode = List {m with loading}}
  | Result _ -> state

(* Pager management delegations *)

let create_empty_pager = Rpc_browser_pagers.create_empty_pager

let get_pager_ids = Rpc_browser_pagers.get_pager_ids

let next_available_id = Rpc_browser_pagers.next_available_id

let is_result_mode = Rpc_browser_pagers.is_result_mode

let get_result_focus = Rpc_browser_pagers.get_result_focus

let set_result_focus = Rpc_browser_pagers.set_result_focus

let enter_result_mode state =
  Rpc_browser_pagers.enter_result_mode
    ~current_instance:(current_instance state)
    state

let add_pager state =
  Rpc_browser_pagers.add_pager ~current_instance:(current_instance state) state

let remove_pager = Rpc_browser_pagers.remove_pager

let focus_pager = Rpc_browser_pagers.focus_pager

let focus_browser = Rpc_browser_pagers.focus_browser

let get_focused_pager = Rpc_browser_pagers.get_focused_pager

let get_focused_pager_id = Rpc_browser_pagers.get_focused_pager_id

let get_pager_target = Rpc_browser_pagers.get_pager_target

let set_pager_target = Rpc_browser_pagers.set_pager_target

let get_pagers = Rpc_browser_pagers.get_pagers

let set_pager_result = Rpc_browser_pagers.set_pager_result

let execute_get = Rpc_browser_pagers.execute_get

let set_result = Rpc_browser_pagers.set_result

let cursor_up state =
  match state.mode with
  | List m when m.cursor > 0 ->
      let new_cursor = m.cursor - 1 in
      {
        state with
        mode = List {m with cursor = new_cursor};
        cached_cursor = new_cursor;
      }
  | _ -> state

let cursor_down state =
  match state.mode with
  | List m when m.cursor < List.length m.entries - 1 ->
      let new_cursor = m.cursor + 1 in
      {
        state with
        mode = List {m with cursor = new_cursor};
        cached_cursor = new_cursor;
      }
  | _ -> state

let scroll _delta state =
  (* Scrolling is now handled by the pager widget, this is kept for compatibility *)
  state

let set_error msg state = {state with error = Some msg}

let clear_error state = {state with error = None}

let set_openapi_status status state = {state with openapi_status = status}

let toggle_focus state =
  match state.mode with
  | Result ({focus; _} as r) ->
      let new_focus : result_focus =
        match (focus : result_focus) with
        | FocusBrowser -> (
            if List.exists (fun p -> p.id = r.last_pager_id) r.pagers then
              FocusPager r.last_pager_id
            else
              match r.pagers with
              | first :: _ -> FocusPager first.id
              | [] -> FocusBrowser)
        | FocusPager _ -> FocusBrowser
      in
      {state with mode = Result {r with focus = new_focus}}
  | List _ -> state

let get_pager = Rpc_browser_pagers.get_pager

let set_pager = Rpc_browser_pagers.set_pager

(* Dynamic value history delegations *)
let add_dynamic_value = Rpc_browser_history.add_dynamic_value

let get_recent_values = Rpc_browser_history.get_recent_values

(* JSON Folding delegations *)
let toggle_fold = Rpc_browser_json_fold.toggle_fold

let unfold_all_json = Rpc_browser_json_fold.unfold_all

let fold_all_json = Rpc_browser_json_fold.fold_all

(* Cached cursor navigation delegations *)
let cached_cursor_up = Rpc_browser_cached_nav.cursor_up

let cached_cursor_down = Rpc_browser_cached_nav.cursor_down

let get_cached_entry = Rpc_browser_cached_nav.get_entry

let navigate_cached = Rpc_browser_cached_nav.navigate

let set_cached_entries = Rpc_browser_cached_nav.set_entries

let navigate_cached_up = Rpc_browser_cached_nav.navigate_up

(* Recent paths LRU delegations *)
let add_recent_path = Rpc_browser_history.add_recent_path

let get_recent_paths = Rpc_browser_history.get_recent_paths

(* Streaming delegations *)
let stop_streaming_pager = Rpc_browser_pagers.stop_streaming_pager

let stop_all_streaming = Rpc_browser_pagers.stop_all_streaming

let is_streaming = Rpc_browser_pagers.is_streaming

let start_streaming_pager = Rpc_browser_pagers.start_streaming_pager
