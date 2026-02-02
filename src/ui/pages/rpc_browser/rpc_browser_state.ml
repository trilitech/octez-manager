(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Octez_manager_lib
module Pager = Miaou_widgets_display.Pager_widget

type entry_kind = Get | Sub | Dyn of string

type entry = {name : string; kind : entry_kind}

type pager_slot = {
  id : int;
  request : string;
  body : string;
  raw_body : string;
  pager : Pager.t option;
  foldable : Foldable_json.t option;
  response_time_ms : float option;
  response_size : int option;
}

type result_focus = FocusBrowser | FocusPager of int

(* Legacy type alias - kept for backwards compatibility with external code.
   Note: This type's constructors FocusBrowser and FocusPager shadow the result_focus ones,
   so we use explicit type annotations in code that uses result_focus. *)
type focus = FocusBrowser | FocusPager

type mode =
  | List of {entries : entry list; cursor : int; loading : bool}
  | Result of {
      pagers : pager_slot list;
      focus : result_focus;
      last_pager_id : int; (* Last focused pager, used when browser is focused *)
    }

type openapi_status = Loading | Ready | Error of string | NotAvailable

type dynamic_value = {segment_type : string; value : string; timestamp : float}

type state = {
  instances : Service.t list;
  selected_idx : int;
  path : string list;
  mode : mode;
  openapi_status : openapi_status;
  error : string option;
  dynamic_history : dynamic_value list;
  cached_entries : entry list;
  cached_cursor : int;
}

(* Dynamic history file path *)
let history_file () =
  Filename.concat
    (Common.xdg_config_home ())
    "octez-manager/rpc_dynamic_history.json"

let load_dynamic_history () =
  let path = history_file () in
  if Sys.file_exists path then
    try
      let ic = open_in path in
      let content = really_input_string ic (in_channel_length ic) in
      close_in ic ;
      match Yojson.Safe.from_string content with
      | `List items ->
          List.filter_map
            (fun item ->
              match item with
              | `Assoc kvs -> (
                  match
                    ( List.assoc_opt "segment_type" kvs,
                      List.assoc_opt "value" kvs,
                      List.assoc_opt "timestamp" kvs )
                  with
                  | Some (`String st), Some (`String v), Some (`Float ts) ->
                      Some {segment_type = st; value = v; timestamp = ts}
                  | _ -> None)
              | _ -> None)
            items
      | _ -> []
    with _ -> []
  else []

let save_dynamic_history history =
  let path = history_file () in
  let dir = Filename.dirname path in
  (if not (Sys.file_exists dir) then try Unix.mkdir dir 0o755 with _ -> ()) ;
  try
    let json =
      `List
        (List.map
           (fun dv ->
             `Assoc
               [
                 ("segment_type", `String dv.segment_type);
                 ("value", `String dv.value);
                 ("timestamp", `Float dv.timestamp);
               ])
           history)
    in
    let oc = open_out path in
    output_string oc (Yojson.Safe.pretty_to_string json) ;
    close_out oc
  with _ -> ()

let init ~instances =
  let dynamic_history = load_dynamic_history () in
  {
    instances;
    selected_idx = 0;
    path = [];
    mode = List {entries = []; cursor = 0; loading = true};
    openapi_status = NotAvailable;
    error = None;
    dynamic_history;
    cached_entries = [];
    cached_cursor = 0;
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
      {
        state with
        mode = List {m with entries; loading = false};
        error = None;
        cached_entries = entries;
        cached_cursor = m.cursor;
      }
  | Result _ -> state

let set_loading loading state =
  match state.mode with
  | List m -> {state with mode = List {m with loading}}
  | Result _ -> state

(* Pager management functions *)

let create_empty_pager id =
  {
    id;
    request = "";
    body = "";
    raw_body = "";
    pager = None;
    foldable = None;
    response_time_ms = None;
    response_size = None;
  }

let get_pager_ids state =
  match state.mode with
  | Result {pagers; _} -> List.map (fun p -> p.id) pagers
  | List _ -> []

let next_available_id state =
  let used_ids = get_pager_ids state in
  let rec find_id n =
    if n > 9 then None
    else if List.mem n used_ids then find_id (n + 1)
    else Some n
  in
  find_id 0

let is_result_mode state =
  match state.mode with Result _ -> true | List _ -> false

let get_result_focus state =
  match state.mode with Result {focus; _} -> focus | List _ -> FocusBrowser

let set_result_focus focus state =
  match state.mode with
  | Result r -> {state with mode = Result {r with focus}}
  | List _ -> state

let enter_result_mode state =
  let pager0 = create_empty_pager 0 in
  {
    state with
    mode = Result {pagers = [pager0]; focus = FocusPager 0; last_pager_id = 0};
  }

let add_pager state =
  match state.mode with
  | Result {pagers; _} -> (
      if List.length pagers >= 10 then None
      else
        match next_available_id state with
        | None -> None
        | Some new_id ->
            let new_pager = create_empty_pager new_id in
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

let remove_pager id state =
  match state.mode with
  | Result {pagers; focus; last_pager_id} ->
      if List.length pagers <= 1 then None
      else
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
            }
  | List _ -> None

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

let focus_browser state =
  match state.mode with
  | Result r ->
      {state with mode = Result {r with focus = (FocusBrowser : result_focus)}}
  | List _ -> state

let get_focused_pager state =
  match state.mode with
  | Result {pagers; focus = FocusPager id; _} ->
      List.find_opt (fun p -> p.id = id) pagers
  | Result {pagers; focus = FocusBrowser; last_pager_id; _} ->
      (* When browser is focused, return last focused pager *)
      List.find_opt (fun p -> p.id = last_pager_id) pagers
  | List _ -> None

let get_focused_pager_id state =
  match state.mode with
  | Result {focus = FocusPager id; _} -> id
  | Result {focus = FocusBrowser; last_pager_id; _} -> last_pager_id
  | List _ -> 0

let get_pagers state =
  match state.mode with Result {pagers; _} -> pagers | List _ -> []

let update_pager_slot id f state =
  match state.mode with
  | Result ({pagers; _} as r) ->
      let new_pagers =
        List.map (fun p -> if p.id = id then f p else p) pagers
      in
      {state with mode = Result {r with pagers = new_pagers}}
  | List _ -> state

let set_pager_result ~pager_id ~request ~body ~raw_body ?response_time_ms
    ?response_size state =
  (* Create foldable JSON from raw body *)
  let foldable = Foldable_json.of_string raw_body in
  (* Use foldable render if available, otherwise fall back to highlighted body *)
  let display_body =
    match foldable with Some f -> Foldable_json.render f | None -> body
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

let execute_get ~url state =
  match state.mode with
  | Result {pagers; last_pager_id; _} ->
      let pager_id = get_focused_pager_id state in
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

let set_result ~body ~raw_body ?response_time_ms ?response_size state =
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
        ~body
        ~raw_body
        ?response_time_ms
        ?response_size
        state
  | List _ -> state

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

(* Focus functions *)
let toggle_focus state =
  match state.mode with
  | Result ({focus; _} as r) ->
      let new_focus : result_focus =
        match (focus : result_focus) with
        | FocusBrowser -> FocusPager 0
        | FocusPager _ -> FocusBrowser
      in
      {state with mode = Result {r with focus = new_focus}}
  | List _ -> state

(* Pager functions - work with the focused pager *)
let get_pager state =
  match get_focused_pager state with Some slot -> slot.pager | None -> None

let set_pager pager state =
  match state.mode with
  | Result _ ->
      let pager_id = get_focused_pager_id state in
      update_pager_slot
        pager_id
        (fun slot -> {slot with pager = Some pager})
        state
  | List _ -> state

(* Dynamic value history functions *)
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
  save_dynamic_history new_history ;
  {state with dynamic_history = new_history}

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

(* JSON Folding functions - work with the focused pager *)
let update_focused_pager_from_foldable state =
  match get_focused_pager state with
  | Some slot -> (
      match slot.foldable with
      | Some f ->
          let new_body = Foldable_json.render f in
          let pager = Pager.open_text ~title:"Response" new_body in
          (* Preserve cursor mode and position from old pager *)
          let pager =
            match slot.pager with
            | Some old_p ->
                let pager =
                  Pager.set_cursor_mode pager (Pager.cursor_mode old_p)
                in
                Pager.set_cursor pager (Pager.get_cursor_line old_p)
            | None -> Pager.set_cursor_mode pager true
          in
          let pager_id = get_focused_pager_id state in
          update_pager_slot
            pager_id
            (fun s -> {s with body = new_body; pager = Some pager})
            state
      | None -> state)
  | None -> state

let toggle_fold ~line state =
  match get_focused_pager state with
  | Some slot -> (
      match slot.foldable with
      | Some f ->
          let f' = Foldable_json.toggle_fold_at_line f ~line in
          let pager_id = get_focused_pager_id state in
          let state' =
            update_pager_slot
              pager_id
              (fun s -> {s with foldable = Some f'})
              state
          in
          update_focused_pager_from_foldable state'
      | None -> state)
  | None -> state

let unfold_all_json state =
  match get_focused_pager state with
  | Some slot -> (
      match slot.foldable with
      | Some f ->
          let f' = Foldable_json.unfold_all f in
          let pager_id = get_focused_pager_id state in
          let state' =
            update_pager_slot
              pager_id
              (fun s -> {s with foldable = Some f'})
              state
          in
          update_focused_pager_from_foldable state'
      | None -> state)
  | None -> state

let fold_all_json state =
  match get_focused_pager state with
  | Some slot -> (
      match slot.foldable with
      | Some f ->
          let f' = Foldable_json.fold_all f in
          let pager_id = get_focused_pager_id state in
          let state' =
            update_pager_slot
              pager_id
              (fun s -> {s with foldable = Some f'})
              state
          in
          update_focused_pager_from_foldable state'
      | None -> state)
  | None -> state

(* Cached cursor navigation for Result mode browser panel *)
let cached_cursor_up state =
  if state.cached_cursor > 0 then
    {state with cached_cursor = state.cached_cursor - 1}
  else state

let cached_cursor_down state =
  if state.cached_cursor < List.length state.cached_entries - 1 then
    {state with cached_cursor = state.cached_cursor + 1}
  else state

let get_cached_entry state =
  List.nth_opt state.cached_entries state.cached_cursor

(* Navigate path while staying in Result mode (for browser panel navigation) *)
let navigate_cached segment state =
  {
    state with
    path = state.path @ [segment];
    cached_entries = [];
    cached_cursor = 0;
  }

let set_cached_entries entries state =
  {state with cached_entries = entries; cached_cursor = 0}

let navigate_cached_up state =
  match state.path with
  | [] -> state
  | _ ->
      let new_path = List.rev (List.tl (List.rev state.path)) in
      {state with path = new_path; cached_entries = []; cached_cursor = 0}
