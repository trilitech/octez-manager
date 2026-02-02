(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Octez_manager_lib

type entry_kind = Get | Sub | Dyn of string

type entry = {name : string; kind : entry_kind}

type mode =
  | List of {entries : entry list; cursor : int; loading : bool}
  | Result of {
      request : string;
      body : string;
      raw_body : string;
      scroll_offset : int;
      response_time_ms : float option;
      response_size : int option;
    }

type openapi_status = Loading | Ready | Error of string | NotAvailable

type state = {
  instances : Service.t list;
  selected_idx : int;
  path : string list;
  mode : mode;
  openapi_status : openapi_status;
  error : string option;
}

let init ~instances =
  {
    instances;
    selected_idx = 0;
    path = [];
    mode = List {entries = []; cursor = 0; loading = true};
    openapi_status = NotAvailable;
    error = None;
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
        };
    error = None;
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
      {
        state with
        mode =
          Result
            {
              r with
              body;
              raw_body;
              response_time_ms = time;
              response_size = size;
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
