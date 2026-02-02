(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

module State = Rpc_browser_state

(** Quick access shortcuts for common RPC endpoints *)
let shortcuts =
  [
    ("1", "/version", "Node version");
    ("2", "/chains/main/blocks/head", "Latest block");
    ("3", "/chains/main/is_bootstrapped", "Bootstrap status");
    ("4", "/network/connections", "Network peers");
    ("5", "/config/network", "Network config");
  ]

let get_selected_entry state =
  match state.State.mode with
  | State.List {entries; cursor; _} -> List.nth_opt entries cursor
  | State.Result _ -> None

let build_rpc_url service path =
  let base = Rpc_client.endpoint_of service in
  let path_str = if path = [] then "/" else "/" ^ String.concat "/" path in
  base ^ path_str

let default_for_dynamic ~name ~typ =
  let _ = typ in
  match name with
  | "chain_id" -> "main"
  | "block_id" -> "head"
  | "block_hash" -> "head"
  | "contract_id" -> ""
  | "pkh" -> ""
  | _ -> ""

let prompt_dynamic ~name ~typ state on_value =
  let default = default_for_dynamic ~name ~typ in
  let _ = state in
  let title = Printf.sprintf "Enter %s" name in
  let placeholder =
    if default = "" then Some typ
    else Some (Printf.sprintf "%s (default: %s)" typ default)
  in
  Modal_helpers.prompt_text_modal
    ~title
    ~initial:default
    ~placeholder
    ~on_submit:(fun text ->
      let value = if text = "" then default else text in
      on_value value)
    ()

let fetch_entries_sync state =
  match State.current_instance state with
  | None -> State.set_error "No instance selected" state
  | Some service ->
      let segs = state.State.path in
      let entries, _source = Rpc_describe.fetch_entries service ~segs in
      let state_entries =
        List.map
          (fun (e : Rpc_describe.entry) ->
            let kind =
              match e.Rpc_describe.kind with
              | Rpc_describe.Sub -> State.Sub
              | Rpc_describe.Get -> State.Get
              | Rpc_describe.Dyn typ -> State.Dyn typ
            in
            {State.name = e.Rpc_describe.name; kind})
          entries
      in
      State.set_entries state_entries state

let execute_get state on_update =
  match State.current_instance state with
  | None -> on_update (State.set_error "No instance selected" state)
  | Some service -> (
      let url = build_rpc_url service state.State.path in
      let state = State.execute_get ~url state in
      on_update state ;
      let path = "/" ^ String.concat "/" state.State.path in
      let start_time = Unix.gettimeofday () in
      match Rpc_client.http_get_url service path with
      | Ok body ->
          let end_time = Unix.gettimeofday () in
          let response_time_ms = (end_time -. start_time) *. 1000.0 in
          let response_size = String.length body in
          let highlighted =
            match Json_highlighter.highlight body with
            | Ok h -> h
            | Error _ -> body
          in
          on_update
            (State.set_result
               ~body:highlighted
               ~raw_body:body
               ~response_time_ms
               ~response_size
               state)
      | Error msg -> on_update (State.set_error msg state))

let fetch_entries state on_update =
  let new_state = fetch_entries_sync state in
  on_update new_state

let handle_enter state on_update =
  match get_selected_entry state with
  | None -> ()
  | Some entry -> (
      match entry.State.kind with
      | State.Sub ->
          let new_state = State.navigate_to entry.State.name state in
          fetch_entries new_state on_update
      | State.Get -> execute_get state on_update
      | State.Dyn typ ->
          prompt_dynamic ~name:entry.State.name ~typ state (fun value ->
              let new_state = State.navigate_to value state in
              fetch_entries new_state on_update))

let cycle_instance ~delta state =
  let n = List.length state.State.instances in
  if n = 0 then state
  else
    let new_idx = (state.State.selected_idx + delta + n) mod n in
    State.select_instance new_idx state

let execute_shortcut ~key state on_update =
  match List.find_opt (fun (k, _, _) -> k = key) shortcuts with
  | None -> false
  | Some (_, path, _) -> (
      match State.current_instance state with
      | None ->
          on_update (State.set_error "No instance selected" state) ;
          true
      | Some service ->
          let url = Rpc_client.endpoint_of service ^ path in
          let state = State.execute_get ~url state in
          on_update state ;
          let start_time = Unix.gettimeofday () in
          (match Rpc_client.http_get_url service path with
          | Ok body ->
              let end_time = Unix.gettimeofday () in
              let response_time_ms = (end_time -. start_time) *. 1000.0 in
              let response_size = String.length body in
              let highlighted =
                match Json_highlighter.highlight body with
                | Ok h -> h
                | Error _ -> body
              in
              on_update
                (State.set_result
                   ~body:highlighted
                   ~raw_body:body
                   ~response_time_ms
                   ~response_size
                   state)
          | Error msg -> on_update (State.set_error msg state)) ;
          true)
