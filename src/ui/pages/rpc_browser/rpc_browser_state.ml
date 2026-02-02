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

type focus = FocusBrowser | FocusPager

type mode =
  | List of {entries : entry list; cursor : int; loading : bool}
  | Result of {
      request : string;
      body : string;
      raw_body : string;
      scroll_offset : int;
      response_time_ms : float option;
      response_size : int option;
      pager : Pager.t option;
      foldable : Foldable_json.t option;
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
  focus : focus;
  dynamic_history : dynamic_value list;
}

(* Dynamic history file path *)
let history_file () =
  Filename.concat (Common.xdg_config_home ()) "octez-manager/rpc_dynamic_history.json"

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
  (if not (Sys.file_exists dir) then
     try Unix.mkdir dir 0o755 with _ -> ()) ;
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
    focus = FocusBrowser;
    dynamic_history;
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
      focus = FocusBrowser;
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
      {state with mode = List {m with entries; loading = false}; error = None}
  | Result _ -> state

let set_loading loading state =
  match state.mode with
  | List m -> {state with mode = List {m with loading}}
  | Result _ -> state

let execute_get ~url state =
  {
    state with
    mode =
      Result
        {
          request = url;
          body = "Loading...";
          raw_body = "";
          scroll_offset = 0;
          response_time_ms = None;
          response_size = None;
          pager = None;
          foldable = None;
        };
    error = None;
    focus = FocusPager;
  }

let set_result ~body ~raw_body ?response_time_ms ?response_size state =
  match state.mode with
  | Result r ->
      let time =
        match response_time_ms with
        | Some t -> Some t
        | None -> r.response_time_ms
      in
      let size =
        match response_size with Some s -> Some s | None -> r.response_size
      in
      (* Create foldable JSON from raw body *)
      let foldable = Foldable_json.of_string raw_body in
      (* Use foldable render if available, otherwise fall back to highlighted body *)
      let display_body =
        match foldable with
        | Some f -> Foldable_json.render f
        | None -> body
      in
      (* Create pager from rendered content *)
      let pager = Pager.open_text ~title:"Response" display_body in
      {
        state with
        mode =
          Result
            {
              r with
              body = display_body;
              raw_body;
              response_time_ms = time;
              response_size = size;
              pager = Some pager;
              foldable;
            };
      }
  | List _ -> state

let cursor_up state =
  match state.mode with
  | List m when m.cursor > 0 ->
      {state with mode = List {m with cursor = m.cursor - 1}}
  | _ -> state

let cursor_down state =
  match state.mode with
  | List m when m.cursor < List.length m.entries - 1 ->
      {state with mode = List {m with cursor = m.cursor + 1}}
  | _ -> state

let scroll delta state =
  match state.mode with
  | Result r ->
      let new_offset = max 0 (r.scroll_offset + delta) in
      {state with mode = Result {r with scroll_offset = new_offset}}
  | List _ -> state

let set_error msg state = {state with error = Some msg}

let clear_error state = {state with error = None}

let set_openapi_status status state = {state with openapi_status = status}

(* Focus functions *)
let toggle_focus state =
  let new_focus =
    match state.focus with FocusBrowser -> FocusPager | FocusPager -> FocusBrowser
  in
  {state with focus = new_focus}

let set_focus focus state = {state with focus}

(* Pager functions *)
let get_pager state =
  match state.mode with Result {pager; _} -> pager | List _ -> None

let set_pager pager state =
  match state.mode with
  | Result r -> {state with mode = Result {r with pager = Some pager}}
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
    if List.length lst > 50 then
      List.filteri (fun i _ -> i < 50) lst
    else lst
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

(* JSON Folding functions *)
let update_pager_from_foldable state =
  match state.mode with
  | Result ({foldable = Some f; _} as r) ->
      let new_body = Foldable_json.render f in
      let pager = Pager.open_text ~title:"Response" new_body in
      {state with mode = Result {r with body = new_body; pager = Some pager}}
  | _ -> state

let toggle_fold ~line state =
  match state.mode with
  | Result ({foldable = Some f; _} as r) ->
      let f' = Foldable_json.toggle_fold_at_line f ~line in
      let state' = {state with mode = Result {r with foldable = Some f'}} in
      update_pager_from_foldable state'
  | _ -> state

let unfold_all_json state =
  match state.mode with
  | Result ({foldable = Some f; _} as r) ->
      let f' = Foldable_json.unfold_all f in
      let state' = {state with mode = Result {r with foldable = Some f'}} in
      update_pager_from_foldable state'
  | _ -> state

let fold_all_json state =
  match state.mode with
  | Result ({foldable = Some f; _} as r) ->
      let f' = Foldable_json.fold_all f in
      let state' = {state with mode = Result {r with foldable = Some f'}} in
      update_pager_from_foldable state'
  | _ -> state
