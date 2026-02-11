(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

module State = Rpc_browser_state
module Bg = Background_runner

(** Default shortcuts for when no recent paths exist *)
let default_shortcuts =
  [
    ("/version", "Node version");
    ("/chains/main/blocks/head", "Latest block");
    ("/chains/main/is_bootstrapped", "Bootstrap status");
    ("/network/connections", "Network peers");
    ("/config/network", "Network config");
  ]

(** Get shortcuts from LRU or defaults - returns (key, path, desc) list *)
let get_shortcuts state =
  let recent = State.get_recent_paths state in
  if List.length recent > 0 then
    List.mapi
      (fun i rp -> (string_of_int (i + 1), rp.State.rp_path, rp.State.rp_desc))
      recent
  else
    List.mapi
      (fun i (path, desc) -> (string_of_int (i + 1), path, desc))
      default_shortcuts

(** Group services by network, returning list of (network_name, services) *)
let group_by_network services =
  (* Build a map of network -> services *)
  let network_map =
    List.fold_left
      (fun acc svc ->
        let network = svc.Octez_manager_lib.Service.network in
        let existing = try List.assoc network acc with Not_found -> [] in
        (network, svc :: existing) :: List.remove_assoc network acc)
      []
      services
  in
  (* Sort by network name and reverse service lists (they were consed) *)
  List.sort
    (fun (n1, _) (n2, _) -> String.compare n1 n2)
    (List.map (fun (net, svcs) -> (net, List.rev svcs)) network_map)

(** Format a service for display in the modal.
    For public nodes, show both label and URL. For local nodes, show just the label. *)
let format_service_label (svc : Octez_manager_lib.Service.t) ~is_current =
  let is_public =
    svc.Octez_manager_lib.Service.data_dir = ""
    && svc.Octez_manager_lib.Service.app_bin_dir = ""
  in
  let name = svc.Octez_manager_lib.Service.instance in
  let label =
    if is_public then
      (* Public nodes: show "Name https://..." *)
      let url =
        Octez_manager_lib.Rpc_addr.to_string
          svc.Octez_manager_lib.Service.rpc_addr
      in
      Printf.sprintf "%s %s" name (Miaou_widgets_display.Widgets.dim url)
    else
      (* Local instances: just show name *)
      name
  in
  if is_current then Miaou_widgets_display.Widgets.fg 10 ("      ✓ " ^ label)
  else "        " ^ label

(** Build modal items with local/public sections and network grouping.
    Returns a flat list where each item carries its section, network, and service. *)
let build_instance_items ~local ~public =
  (* Group services by network and add section/network labels to display *)
  let build_labeled_items section_label services =
    let grouped = group_by_network services in
    List.concat_map
      (fun (network, svcs) ->
        List.map
          (fun svc ->
            (* Attach section and network labels for display *)
            (section_label, String.capitalize_ascii network, svc))
          svcs)
      grouped
  in
  let local_items =
    if local <> [] then build_labeled_items "Local Instances" local else []
  in
  let public_items =
    if public <> [] then build_labeled_items "Public Nodes" public else []
  in
  local_items @ public_items

let get_selected_entry state =
  match state.State.mode with
  | State.List {entries; cursor; _} -> List.nth_opt entries cursor
  | State.Result _ -> None

let build_rpc_url service path =
  let base = Rpc_client.endpoint_of service in
  let path_str = if path = [] then "/" else "/" ^ String.concat "/" path in
  base ^ path_str

let default_for_dynamic ~name ~typ state =
  let _ = name in
  (* First check history for recent values - use typ (without angle brackets) *)
  match State.get_recent_values ~segment_type:typ state with
  | recent :: _ -> recent
  | [] -> (
      (* Fall back to hardcoded defaults *)
      match typ with
      | "chain_id" -> "main"
      | "block_id" -> "head"
      | "block_hash" -> "head"
      | "contract_id" -> ""
      | "pkh" -> ""
      | _ -> "")

(* State ref for recording dynamic values *)
let pending_dynamic_update : (State.state -> unit) option ref = ref None

let prompt_dynamic ~name ~typ state on_value on_update =
  let default = default_for_dynamic ~name ~typ state in
  (* Use typ (without angle brackets) for history lookup *)
  let recent_values = State.get_recent_values ~segment_type:typ state in
  let title =
    if List.length recent_values > 0 then
      Printf.sprintf
        "Enter %s (recent: %s)"
        name
        (String.concat ", " (List.filteri (fun i _ -> i < 3) recent_values))
    else Printf.sprintf "Enter %s" name
  in
  let placeholder =
    if default = "" then Some typ
    else Some (Printf.sprintf "%s (default: %s)" typ default)
  in
  (* Store update callback for use after modal closes *)
  pending_dynamic_update := Some on_update ;
  Modal_helpers.prompt_text_modal
    ~title
    ~initial:default
    ~placeholder
    ~on_submit:(fun text ->
      let value = if text = "" then default else text in
      (* Record the value in history - use typ (without angle brackets) *)
      let new_state = State.add_dynamic_value ~segment_type:typ ~value state in
      (match !pending_dynamic_update with
      | Some update -> update new_state
      | None -> ()) ;
      pending_dynamic_update := None ;
      on_value value)
    ()

(** Expand Rpc_describe entries to State entries with recent dynamic values. *)
let expand_entries state entries =
  let expand_entry (e : Rpc_describe.entry) =
    match e.Rpc_describe.kind with
    | Rpc_describe.Sub -> [{State.name = e.Rpc_describe.name; kind = State.Sub}]
    | Rpc_describe.Get -> [{State.name = e.Rpc_describe.name; kind = State.Get}]
    | Rpc_describe.Dyn typ ->
        (* Get recent values for this type, limit to 5 *)
        let recent =
          State.get_recent_values ~segment_type:typ state |> fun lst ->
          if List.length lst > 5 then List.filteri (fun i _ -> i < 5) lst
          else lst
        in
        (* Create DynValue entries for recent values *)
        let recent_entries =
          List.map
            (fun value ->
              {State.name = "<>" ^ value; kind = State.DynValue (typ, value)})
            recent
        in
        (* Add the original Dyn entry after recent values *)
        recent_entries
        @ [{State.name = e.Rpc_describe.name; kind = State.Dyn typ}]
  in
  let state_entries = List.concat_map expand_entry entries in
  (* Add [change target] button at the top *)
  let change_target_entry =
    {State.name = "[change target]"; kind = State.ChangeTarget}
  in
  change_target_entry :: state_entries

let fetch_entries_sync state =
  match State.current_instance state with
  | None -> State.set_error "No instance selected" state
  | Some service ->
      let segs = state.State.path in
      let entries, _source = Rpc_describe.fetch_entries service ~segs in
      State.set_entries (expand_entries state entries) state

(** Async version: fetch entries in background pool.
    Sets loading state first, then fetches in background. *)
let fetch_entries_async state ~on_done =
  match State.current_instance state with
  | None -> on_done (State.set_error "No instance selected" state)
  | Some service ->
      let segs = state.State.path in
      Bg.submit_blocking (fun () ->
          let entries, _source = Rpc_describe.fetch_entries service ~segs in
          on_done (State.set_entries (expand_entries state entries) state))

(* Get the target instance for the current/focused pager, falling back to current_instance *)
let get_target_instance state =
  match State.get_pager_target state with
  | Some svc -> Some svc
  | None -> State.current_instance state

let debug msg = Cmd_runner.append_debug_log ("RPC_BROWSER " ^ msg)

(** Detect whether an RPC path is a streaming endpoint.
    Streaming endpoints never complete - they keep sending JSON objects
    as events occur. Known patterns:
    - /monitor/*  (heads, bootstrapped, protocols, etc.)
    - /chains/*/mempool/monitor_operations *)
let is_streaming_path (path_segments : string list) =
  (* Check if "monitor" is in the path *)
  List.exists
    (fun seg -> seg = "monitor" || String.starts_with ~prefix:"monitor_" seg)
    path_segments

(** Execute a GET request in background: fetch, highlight JSON, and update state
    with the result or error. The HTTP request runs in the background pool
    to avoid blocking the main TUI thread.
    @param caller Label for debug logging (e.g., "execute_get_internal")
    @param service Target node
    @param path RPC path (e.g., "/chains/main/blocks/head")
    @param state Current state (already in loading)
    @param on_update Callback to push updated state *)
let fetch_and_set_result ~caller ~service ~path state on_update =
  Bg.submit_blocking (fun () ->
      let start_time = Unix.gettimeofday () in
      match Rpc_client.http_get_url service path with
      | Ok body ->
          let response_time_ms =
            (Unix.gettimeofday () -. start_time) *. 1000.0
          in
          let response_size = String.length body in
          debug
            (Printf.sprintf
               "%s: OK %d bytes in %.1fms"
               caller
               response_size
               response_time_ms) ;
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
      | Error msg ->
          debug (Printf.sprintf "%s: ERROR: %s" caller msg) ;
          on_update (State.set_error msg state))

(** Start a streaming pager for a streaming RPC endpoint. *)
let start_streaming ~service ~url ~rpc_path state on_update =
  debug "detected streaming endpoint, starting stream" ;
  let state = State.execute_get ~url state in
  let pager_id = State.get_focused_pager_id state in
  let _state =
    State.start_streaming_pager
      ~pager_id
      ~request:url
      ~service
      ~rpc_path
      ~on_state_update:on_update
      state
  in
  ()

(* Internal: execute GET with a specific path for the HTTP request *)
let execute_get_internal ~url_path state on_update =
  match get_target_instance state with
  | None -> on_update (State.set_error "No instance selected" state)
  | Some service ->
      let url = build_rpc_url service url_path in
      let path = "/" ^ String.concat "/" url_path in
      debug
        (Printf.sprintf
           "execute_get_internal: path=%s url=%s instance=%s"
           path
           url
           service.Octez_manager_lib.Service.instance) ;
      if is_streaming_path url_path then
        start_streaming ~service ~url ~rpc_path:path state on_update
      else
        let state = State.execute_get ~url state in
        on_update state ;
        fetch_and_set_result
          ~caller:"execute_get_internal"
          ~service
          ~path
          state
          on_update

(* Execute GET with an additional endpoint name appended to the path *)
let execute_get_with_name endpoint_name state on_update =
  let url_path = state.State.path @ [endpoint_name] in
  execute_get_internal ~url_path state on_update

let execute_get state on_update =
  execute_get_internal ~url_path:state.State.path state on_update

let fetch_entries state on_update = fetch_entries_async state ~on_done:on_update

let handle_enter state on_update =
  match get_selected_entry state with
  | None -> on_update (State.set_error "No entry selected" state)
  | Some entry -> (
      match entry.State.kind with
      | State.Sub ->
          let new_state = State.navigate_to entry.State.name state in
          fetch_entries new_state on_update
      | State.Get ->
          (* Execute GET - if entry name is empty, GET is at current path *)
          if entry.State.name = "" then execute_get state on_update
          else execute_get_with_name entry.State.name state on_update
      | State.Dyn typ ->
          prompt_dynamic
            ~name:entry.State.name
            ~typ
            state
            (fun value ->
              let new_state = State.navigate_to value state in
              fetch_entries new_state on_update)
            on_update
      | State.DynValue (typ, value) ->
          (* Navigate directly with the recent value, record in history *)
          let new_state =
            State.add_dynamic_value ~segment_type:typ ~value state
          in
          let new_state = State.navigate_to value new_state in
          fetch_entries new_state on_update
      | State.ChangeTarget ->
          (* Open modal to select target instance with sections *)
          let all_instances = State.get_instances state in
          (* Local instances have non-empty data_dir or app_bin_dir *)
          let is_local svc =
            svc.Octez_manager_lib.Service.data_dir <> ""
            || svc.Octez_manager_lib.Service.app_bin_dir <> ""
          in
          let local = List.filter is_local all_instances in
          let public = State.public_nodes () in
          (* Get current target for highlighting *)
          let current_target = get_target_instance state in
          let is_current svc =
            match current_target with
            | None -> false
            | Some curr ->
                curr.Octez_manager_lib.Service.rpc_addr
                = svc.Octez_manager_lib.Service.rpc_addr
          in
          (* Build items with section headers and network grouping *)
          let items = build_instance_items ~local ~public in
          if items = [] then
            on_update (State.set_error "No instances available" state)
          else
            (* Expand items to include visual headers for display *)
            let display_items =
              let rec expand prev_section prev_network = function
                | [] -> []
                | (section, network, svc) :: rest ->
                    let needs_section =
                      match prev_section with
                      | None -> true
                      | Some s -> s <> section
                    in
                    let needs_network =
                      (not needs_section)
                      &&
                      match prev_network with
                      | None -> true
                      | Some n -> n <> network
                    in
                    let items =
                      if needs_section then
                        [
                          (section, network, svc, `SectionHeader);
                          (section, network, svc, `NetworkHeader);
                          (section, network, svc, `Service);
                        ]
                      else if needs_network then
                        [
                          (section, network, svc, `NetworkHeader);
                          (section, network, svc, `Service);
                        ]
                      else [(section, network, svc, `Service)]
                    in
                    items @ expand (Some section) (Some network) rest
              in
              expand None None items
            in
            Modal_helpers.open_choice_modal
              ~title:"Select target instance"
              ~items:display_items
              ~to_string:(fun (_section, network, svc, kind) ->
                match kind with
                | `SectionHeader ->
                    (* Section appears in title, but we need something here for the item *)
                    let section =
                      if
                        svc.Octez_manager_lib.Service.data_dir <> ""
                        || svc.Octez_manager_lib.Service.app_bin_dir <> ""
                      then "Local Instances"
                      else "Public Nodes"
                    in
                    Miaou_widgets_display.Widgets.bold ("── " ^ section ^ " ──")
                | `NetworkHeader ->
                    Miaou_widgets_display.Widgets.fg 14 ("  • " ^ network)
                | `Service ->
                    format_service_label svc ~is_current:(is_current svc))
              ~on_select:(fun (_, _, svc, kind) ->
                (* Only react to service selections, ignore headers *)
                match kind with
                | `Service ->
                    let new_state = State.set_pager_target (Some svc) state in
                    on_update new_state
                | `SectionHeader | `NetworkHeader -> ())
              ())

let cycle_instance ~delta state =
  let n = List.length state.State.instances in
  if n = 0 then state
  else
    let new_idx = (state.State.selected_idx + delta + n) mod n in
    State.select_instance new_idx state

let execute_shortcut ~key state on_update =
  let shortcuts = get_shortcuts state in
  match List.find_opt (fun (k, _, _) -> k = key) shortcuts with
  | None -> false
  | Some (_, path, desc) -> (
      match get_target_instance state with
      | None ->
          on_update (State.set_error "No instance selected" state) ;
          true
      | Some service ->
          let url = Rpc_client.endpoint_of service ^ path in
          debug
            (Printf.sprintf
               "execute_shortcut: key=%s path=%s instance=%s"
               key
               path
               service.Octez_manager_lib.Service.instance) ;
          (* Record in LRU before executing *)
          let state = State.add_recent_path ~path ~desc state in
          let path_segments =
            String.split_on_char '/' path |> List.filter (fun s -> s <> "")
          in
          if is_streaming_path path_segments then (
            start_streaming ~service ~url ~rpc_path:path state on_update ;
            true)
          else
            let state = State.execute_get ~url state in
            on_update state ;
            fetch_and_set_result
              ~caller:"execute_shortcut"
              ~service
              ~path
              state
              on_update ;
            true)

let fetch_cached_entries state on_update =
  match State.current_instance state with
  | None -> on_update (State.set_error "No instance selected" state)
  | Some service ->
      let segs = state.State.path in
      Bg.submit_blocking (fun () ->
          let entries, _source = Rpc_describe.fetch_entries service ~segs in
          let state_entries = expand_entries state entries in
          on_update (State.set_cached_entries state_entries state))

let handle_cached_enter state on_update =
  match State.get_cached_entry state with
  | None ->
      (* No entry found at cursor position *)
      on_update
        (State.set_error
           (Printf.sprintf
              "No entry at cursor %d (entries: %d)"
              state.State.cached_cursor
              (List.length state.State.cached_entries))
           state)
  | Some entry -> (
      match entry.State.kind with
      | State.Sub ->
          (* Navigate path but stay in Result mode *)
          let new_state = State.navigate_cached entry.State.name state in
          fetch_cached_entries new_state on_update
      | State.Get ->
          (* Execute GET - if entry name is empty, GET is at current path *)
          if entry.State.name = "" then execute_get state on_update
          else execute_get_with_name entry.State.name state on_update
      | State.Dyn typ ->
          prompt_dynamic
            ~name:entry.State.name
            ~typ
            state
            (fun value ->
              let new_state = State.navigate_cached value state in
              fetch_cached_entries new_state on_update)
            on_update
      | State.DynValue (typ, value) ->
          (* Navigate directly with the recent value, record in history *)
          let new_state =
            State.add_dynamic_value ~segment_type:typ ~value state
          in
          let new_state = State.navigate_cached value new_state in
          fetch_cached_entries new_state on_update
      | State.ChangeTarget ->
          (* Open modal to select target instance with sections *)
          let all_instances = State.get_instances state in
          (* Local instances have non-empty data_dir or app_bin_dir *)
          let is_local svc =
            svc.Octez_manager_lib.Service.data_dir <> ""
            || svc.Octez_manager_lib.Service.app_bin_dir <> ""
          in
          let local = List.filter is_local all_instances in
          let public = State.public_nodes () in
          (* Get current target for highlighting *)
          let current_target = get_target_instance state in
          let is_current svc =
            match current_target with
            | None -> false
            | Some curr ->
                curr.Octez_manager_lib.Service.rpc_addr
                = svc.Octez_manager_lib.Service.rpc_addr
          in
          (* Build items with section headers and network grouping *)
          let items = build_instance_items ~local ~public in
          if items = [] then
            on_update (State.set_error "No instances available" state)
          else
            (* Expand items to include visual headers for display *)
            let display_items =
              let rec expand prev_section prev_network = function
                | [] -> []
                | (section, network, svc) :: rest ->
                    let needs_section =
                      match prev_section with
                      | None -> true
                      | Some s -> s <> section
                    in
                    let needs_network =
                      (not needs_section)
                      &&
                      match prev_network with
                      | None -> true
                      | Some n -> n <> network
                    in
                    let items =
                      if needs_section then
                        [
                          (section, network, svc, `SectionHeader);
                          (section, network, svc, `NetworkHeader);
                          (section, network, svc, `Service);
                        ]
                      else if needs_network then
                        [
                          (section, network, svc, `NetworkHeader);
                          (section, network, svc, `Service);
                        ]
                      else [(section, network, svc, `Service)]
                    in
                    items @ expand (Some section) (Some network) rest
              in
              expand None None items
            in
            Modal_helpers.open_choice_modal
              ~title:"Select target instance"
              ~items:display_items
              ~to_string:(fun (_section, network, svc, kind) ->
                match kind with
                | `SectionHeader ->
                    let section =
                      if
                        svc.Octez_manager_lib.Service.data_dir <> ""
                        || svc.Octez_manager_lib.Service.app_bin_dir <> ""
                      then "Local Instances"
                      else "Public Nodes"
                    in
                    Miaou_widgets_display.Widgets.bold ("── " ^ section ^ " ──")
                | `NetworkHeader ->
                    Miaou_widgets_display.Widgets.fg 14 ("  • " ^ network)
                | `Service ->
                    format_service_label svc ~is_current:(is_current svc))
              ~on_select:(fun (_, _, svc, kind) ->
                (* Only react to service selections, ignore headers *)
                match kind with
                | `Service ->
                    let new_state = State.set_pager_target (Some svc) state in
                    on_update new_state
                | `SectionHeader | `NetworkHeader -> ())
              ())

let navigate_cached_back state on_update =
  let new_state = State.navigate_cached_up state in
  fetch_cached_entries new_state on_update
