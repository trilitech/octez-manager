(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

module Keys = Miaou.Core.Keys
module Navigation = Miaou.Core.Navigation
module Vsection = Miaou_widgets_layout.Vsection
module Widgets = Miaou_widgets_display.Widgets
module Pager = Miaou_widgets_display.Pager_widget
module Grid = Miaou_widgets_layout.Grid_layout
module Service_state = Data.Service_state

let name = "rpc_browser"

module State = Rpc_browser_state
module Actions = Rpc_browser_actions

type state = State.state

type msg = unit

type pstate = state Navigation.t

let state_ref : state option ref = ref None

(* Chord state for C-x prefix *)
let pending_chord : string option ref = ref None

let build_instance_items = Actions.build_instance_items

let update_state s =
  state_ref := Some s ;
  Context.mark_instances_dirty ()

let init () =
  (* Trigger OpenAPI download if needed (for public nodes without /describe) *)
  if Rpc_openapi.needs_download () then
    Modal_helpers.show_spinner_modal
      ~title:"Fetching OpenAPI"
      ~label:"Downloading RPC specifications..."
      ~work:(fun () ->
        match Rpc_openapi.download_sync () with
        | Ok () -> Ok ()
        | Error msg -> Error (`Msg msg))
      ~on_complete:(fun status ->
        match status with
        | `Succeeded ->
            (* Clear rpc_describe cache so new OpenAPI entries are used *)
            Rpc_describe.clear_cache () ;
            Context.toast_info
              "OpenAPI specs ready - public nodes now browsable"
        | `Failed msg ->
            Context.toast_warn
              (Printf.sprintf "OpenAPI download failed: %s" msg)
        | `Cancelled -> ())
      () ;
  (* Load local node instances *)
  let local_nodes =
    let service_states = Data.load_service_states () in
    List.filter_map
      (fun (ss : Service_state.t) ->
        if ss.service.Octez_manager_lib.Service.role = "node" then
          Some ss.service
        else None)
      service_states
  in
  (* Check if a specific instance was selected from rpc_node_selection *)
  let nodes =
    match State.get_selected_instance () with
    | Some service ->
        (* Clear the override so next time we use local instances *)
        State.clear_selected_instance () ;
        (* Include selected instance + local nodes (selected first) *)
        let is_same svc =
          svc.Octez_manager_lib.Service.rpc_addr
          = service.Octez_manager_lib.Service.rpc_addr
        in
        service :: List.filter (fun svc -> not (is_same svc)) local_nodes
    | None -> local_nodes
  in
  let state = State.init ~instances:nodes in
  state_ref := Some state ;
  Navigation.make state

let update ps _ = ps

let refresh ps =
  match !state_ref with
  | Some s ->
      let new_state = Actions.fetch_entries_sync s in
      state_ref := Some new_state ;
      Navigation.update (fun _ -> new_state) ps
  | None -> ps

let move _ps _n = refresh _ps

let service_select ps _idx = ps

let service_cycle ps _delta = ps

let back ps =
  let s = ps.Navigation.s in
  match s.State.mode with
  | State.Result _ ->
      (* Stop all active streaming connections before leaving Result mode *)
      let s = State.stop_all_streaming s in
      let new_state =
        {
          s with
          State.mode = State.List {entries = []; cursor = 0; loading = true};
        }
      in
      let new_state = Actions.fetch_entries_sync new_state in
      state_ref := Some new_state ;
      Navigation.update (fun _ -> new_state) ps
  | State.List _ ->
      if s.State.path = [] then Navigation.back ps
      else
        let new_state = State.navigate_up s in
        let new_state = Actions.fetch_entries_sync new_state in
        state_ref := Some new_state ;
        Navigation.update (fun _ -> new_state) ps

let handled_keys () =
  Miaou.Core.Keys.
    [Escape; Enter; Up; Down; Char "j"; Char "k"; Char "u"; Char "r"; Tab]

let cycle_instance ps =
  let s = ps.Navigation.s in
  (* Only cycle instance in List mode; in Result mode, Tab cycles pagers *)
  match s.State.mode with
  | State.List _ ->
      let new_state = Actions.cycle_instance ~delta:1 s in
      let new_state = Actions.fetch_entries_sync new_state in
      state_ref := Some new_state ;
      Navigation.update (fun _ -> new_state) ps
  | State.Result _ ->
      (* Cycle through pagers in Result mode *)
      let pager_ids = State.get_pager_ids s |> List.sort compare in
      let current_id = State.get_focused_pager_id s in
      let next_id =
        match List.find_opt (fun id -> id > current_id) pager_ids with
        | Some id -> id
        | None -> (
            match pager_ids with [] -> current_id | first :: _ -> first)
      in
      let new_state = State.focus_pager next_id s in
      state_ref := Some new_state ;
      Navigation.update (fun _ -> new_state) ps

let keymap _ps =
  let noop ps = ps in
  let kb key help =
    {Miaou.Core.Tui_page.key; action = noop; help; display_only = true}
  in
  [
    kb "Esc" "Back";
    kb "↑/↓" "Navigate";
    kb "r" "Refresh";
    kb "Tab" "Instance/Pager";
    kb "@" "Target instance";
    kb "?" "Help";
  ]

(* Minimum width for side-by-side layout *)
let side_by_side_min_width = 140

(* OpenCode style: render two columns side by side with muted separator *)
let render_side_by_side ~left ~right ~left_width ~total_width ~rows
    ~left_focused:_ ~right_focused:_ =
  (* Thicker muted vertical separator (3 chars: space + bar + space) *)
  let separator = Widgets.themed_muted " │ " in
  let sep_column = String.concat "\n" (List.init rows (fun _ -> separator)) in
  let right_width = total_width - left_width - 3 in
  let grid =
    Grid.create
      ~rows:[Grid.Fr 1.]
      ~cols:[Grid.Px left_width; Grid.Px 3; Grid.Px right_width]
      [
        Grid.cell ~row:0 ~col:0 (fun ~size:_ -> left);
        Grid.cell ~row:0 ~col:1 (fun ~size:_ -> sep_column);
        Grid.cell ~row:0 ~col:2 (fun ~size:_ -> right);
      ]
  in
  Grid.render grid ~size:{LTerm_geom.rows; cols = total_width}

let view ps ~focus ~size =
  let s = ps.Navigation.s in
  let cols = size.LTerm_geom.cols in
  let rows = size.LTerm_geom.rows in
  let result_focus = State.get_result_focus s in
  let is_browser_focused =
    match result_focus with State.FocusBrowser -> true | _ -> false
  in
  let body =
    match s.State.mode with
    | State.List _ ->
        let lines = Rpc_browser_render_list.render ~focus ~state:s ~cols in
        String.concat "\n" lines
    | State.Result _ ->
        (* Check if we should use side-by-side layout *)
        if cols >= side_by_side_min_width then
          (* Side-by-side: browser on left, result on right *)
          let left_width = 50 in
          let right_width = cols - left_width - 1 in
          let browser_focus = focus && is_browser_focused in
          let pager_focus = focus && not is_browser_focused in
          (* Render browser list using cached entries *)
          let left_state =
            {
              s with
              mode =
                State.List
                  {
                    entries = s.State.cached_entries;
                    cursor = s.State.cached_cursor;
                    loading = false;
                  };
            }
          in
          let left_lines =
            Rpc_browser_render_list.render
              ~focus:browser_focus
              ~state:left_state
              ~cols:left_width
          in
          let left =
            (* OpenCode style: clean header with subtle focus indicator *)
            let header_text =
              if browser_focus then " Browser" else " Browser"
            in
            let header_len = String.length header_text in
            let header_padded =
              if header_len >= left_width then header_text
              else header_text ^ String.make (left_width - header_len) ' '
            in
            let header =
              if browser_focus then
                Miaou_style.Style_context.with_child_context
                  ~widget_name:"rpc-browser-header"
                  ~focused:true
                  (fun () ->
                    Widgets.themed_contextual_fill
                      (Widgets.themed_contextual header_padded))
              else Widgets.themed_muted header_padded
            in
            header :: left_lines |> String.concat "\n"
          in
          let right =
            Rpc_browser_render_result.render
              ~state:s
              ~cols:right_width
              ~rows:(rows - 1)
              ~focus:pager_focus
          in
          render_side_by_side
            ~left
            ~right
            ~left_width
            ~total_width:cols
            ~rows
            ~left_focused:browser_focus
            ~right_focused:pager_focus
        else if
          (* Single-column mode: show browser OR pager based on focus *)
          is_browser_focused
        then
          (* Show only browser list with focus border *)
          let left_state =
            {
              s with
              mode =
                State.List
                  {
                    entries = s.State.cached_entries;
                    cursor = s.State.cached_cursor;
                    loading = false;
                  };
            }
          in
          let lines =
            Rpc_browser_render_list.render ~focus ~state:left_state ~cols
          in
          (* OpenCode style: clean header for single-column browser mode *)
          let pager_ids = State.get_pager_ids s in
          let focused_pager = State.get_focused_pager_id s in
          let tabs_plain =
            pager_ids |> List.sort compare
            |> List.map (fun id ->
                if id = focused_pager then Printf.sprintf "[%d]" id
                else Printf.sprintf " %d " id)
            |> String.concat ""
          in
          let header_text = " Browser  |  Pager " ^ tabs_plain in
          let header_len = String.length header_text in
          let header_padded =
            if header_len >= cols then header_text
            else header_text ^ String.make (cols - header_len) ' '
          in
          let header =
            Miaou_style.Style_context.with_child_context
              ~widget_name:"rpc-browser-header"
              ~focused:true
              (fun () ->
                Widgets.themed_contextual_fill
                  (Widgets.themed_contextual header_padded))
          in
          header :: lines |> String.concat "\n"
        else
          (* OpenCode style: clean header for single-column pager mode *)
          let pager_ids = State.get_pager_ids s in
          let focused_pager = State.get_focused_pager_id s in
          let tabs_plain =
            pager_ids |> List.sort compare
            |> List.map (fun id ->
                if id = focused_pager then Printf.sprintf "[%d]" id
                else Printf.sprintf " %d " id)
            |> String.concat ""
          in
          let header_text = " Browser  |  Pager " ^ tabs_plain in
          let header_len = String.length header_text in
          let header_padded =
            if header_len >= cols then header_text
            else header_text ^ String.make (cols - header_len) ' '
          in
          let header =
            Miaou_style.Style_context.with_child_context
              ~widget_name:"rpc-pager-header"
              ~focused:true
              (fun () ->
                Widgets.themed_contextual_fill
                  (Widgets.themed_contextual header_padded))
          in
          let result =
            Rpc_browser_render_result.render
              ~state:s
              ~cols
              ~rows:(rows - 1)
              ~focus
          in
          header ^ "\n" ^ result
  in
  Themed_page.render_layout ~size ~header:[] ~footer:[] ~child:(fun _ -> body)

let handle_modal_key ps key ~size:_ =
  Miaou.Core.Modal_manager.handle_key key ;
  ps

let handle_key ps key ~size =
  if Miaou.Core.Modal_manager.has_active () then (
    Miaou.Core.Modal_manager.handle_key key ;
    ps)
  else
    let s = ps.Navigation.s in
    (* Handle C-x chord for pager selection before any other dispatch *)
    match !pending_chord with
    | Some "C-x" ->
        pending_chord := None ;
        if String.length key = 1 && key.[0] >= '0' && key.[0] <= '9' then
          let pager_id = Char.code key.[0] - Char.code '0' in
          let pager_ids = State.get_pager_ids s in
          if List.mem pager_id pager_ids then (
            let new_state = State.focus_pager pager_id s in
            state_ref := Some new_state ;
            Navigation.update (fun _ -> new_state) ps)
          else ps
        else ps
    | Some _ ->
        pending_chord := None ;
        ps
    | None -> (
        if
          (* Check for C-x prefix *)
          key = "C-x"
        then (
          pending_chord := Some "C-x" ;
          ps)
        else
          match s.State.mode with
          | State.List _ -> (
              (* Check for shortcut keys (1-9) when at root *)
              let shortcut_keys =
                ["1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"]
              in
              let is_shortcut_key =
                s.State.path = []
                && List.exists (fun k -> key = k) shortcut_keys
              in
              if is_shortcut_key then
                let handled = Actions.execute_shortcut ~key s update_state in
                if handled then
                  match !state_ref with
                  | Some new_s -> Navigation.update (fun _ -> new_s) ps
                  | None -> ps
                else ps
              else
                match Keys.of_string key with
                | Some Keys.Escape -> back ps
                | Some Keys.Enter -> (
                    Actions.handle_enter s update_state ;
                    match !state_ref with
                    | Some new_s -> Navigation.update (fun _ -> new_s) ps
                    | None -> ps)
                | Some Keys.Up | Some (Keys.Char "k") ->
                    let new_state = State.cursor_up s in
                    state_ref := Some new_state ;
                    Navigation.update (fun _ -> new_state) ps
                | Some Keys.Down | Some (Keys.Char "j") ->
                    let new_state = State.cursor_down s in
                    state_ref := Some new_state ;
                    Navigation.update (fun _ -> new_state) ps
                | Some (Keys.Char "u") | Some Keys.Backspace -> back ps
                | Some (Keys.Char "r") -> refresh ps
                | Some Keys.Tab -> cycle_instance ps
                | _ -> ps)
          | State.Result _ -> (
              let result_focus = State.get_result_focus s in
              let is_browser_focused =
                match result_focus with
                | State.FocusBrowser -> true
                | _ -> false
              in
              (* Handle save key - saves the focused pager's content *)
              if key = "s" then (
                match State.get_focused_pager s with
                | Some slot when slot.State.raw_body <> "" -> (
                    (* Save to file - use raw JSON (unfolded, no colors) *)
                    let filename =
                      let base =
                        slot.State.request |> String.split_on_char '/'
                        |> List.filter (fun str -> str <> "")
                        |> String.concat "_"
                      in
                      Printf.sprintf
                        "rpc_%s_%d.json"
                        base
                        (int_of_float (Unix.time ()))
                    in
                    try
                      let oc = open_out filename in
                      output_string oc slot.State.raw_body ;
                      close_out oc ;
                      let new_state =
                        State.set_error
                          (Printf.sprintf "Saved to %s" filename)
                          s
                      in
                      state_ref := Some new_state ;
                      Navigation.update (fun _ -> new_state) ps
                    with exn ->
                      let new_state =
                        State.set_error
                          (Printf.sprintf
                             "Save failed: %s"
                             (Printexc.to_string exn))
                          s
                      in
                      state_ref := Some new_state ;
                      Navigation.update (fun _ -> new_state) ps)
                | _ ->
                    let new_state = State.set_error "No content to save" s in
                    state_ref := Some new_state ;
                    Navigation.update (fun _ -> new_state) ps
                    (* Handle split key - create new pager *))
              else if key = "S" then (
                match State.add_pager s with
                | Some new_state ->
                    (* Keep focus on browser after creating pager *)
                    let new_state = State.focus_browser new_state in
                    state_ref := Some new_state ;
                    Navigation.update (fun _ -> new_state) ps
                | None ->
                    let new_state =
                      State.set_error "Maximum pagers reached (10)" s
                    in
                    state_ref := Some new_state ;
                    Navigation.update (fun _ -> new_state) ps
                    (* Handle close pager key *))
              else if key = "x" then (
                let focused_id = State.get_focused_pager_id s in
                match State.remove_pager focused_id s with
                | Some new_state ->
                    state_ref := Some new_state ;
                    Navigation.update (fun _ -> new_state) ps
                | None ->
                    let new_state =
                      State.set_error "Cannot close last pager" s
                    in
                    state_ref := Some new_state ;
                    Navigation.update (fun _ -> new_state) ps
                (* Handle target instance selection *))
              else if key = "@" || key = "t" then (
                (* Open modal to select target instance with sections *)
                let all_instances = State.get_instances s in
                (* Local instances have non-empty data_dir or app_bin_dir *)
                let is_local svc =
                  svc.Octez_manager_lib.Service.data_dir <> ""
                  || svc.Octez_manager_lib.Service.app_bin_dir <> ""
                in
                let local = List.filter is_local all_instances in
                let public = State.public_nodes () in
                let items = build_instance_items ~local ~public in
                if items = [] then (
                  let new_state = State.set_error "No instances available" s in
                  state_ref := Some new_state ;
                  Navigation.update (fun _ -> new_state) ps)
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
                    ~title:"Select target instance for pager"
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
                          Miaou_widgets_display.Widgets.themed_emphasis section
                      | `NetworkHeader ->
                          Miaou_widgets_display.Widgets.themed_accent
                            ("  • " ^ network)
                      | `Service ->
                          let name = svc.Octez_manager_lib.Service.instance in
                          "        " ^ name)
                    ~on_select:(fun (_, _, svc, kind) ->
                      (* Only react to service selections, ignore headers *)
                      match kind with
                      | `Service -> (
                          match !state_ref with
                          | Some current_state ->
                              let new_state =
                                State.set_pager_target (Some svc) current_state
                              in
                              state_ref := Some new_state ;
                              update_state new_state
                          | None -> ())
                      | `SectionHeader | `NetworkHeader -> ())
                    () ;
                  ps
                (* Handle shortcut keys 1-9, but not if pager is in input mode *))
              else if String.length key = 1 && key.[0] >= '1' && key.[0] <= '9'
              then
                (* Check if pager is in search/input mode - if so, pass to pager *)
                let pager_in_input_mode =
                  match State.get_pager s with
                  | Some p -> p.Pager.input_mode <> `None
                  | None -> false
                in
                if pager_in_input_mode then
                  (* Let pager handle digit keys in search mode *)
                  match State.get_pager s with
                  | Some pager ->
                      let win = size.LTerm_geom.rows - 3 in
                      let pager', _consumed =
                        Pager.handle_key pager ~key ~win
                      in
                      let new_state = State.set_pager pager' s in
                      state_ref := Some new_state ;
                      Navigation.update (fun _ -> new_state) ps
                  | None -> ps
                else
                  let handled = Actions.execute_shortcut ~key s update_state in
                  if handled then
                    match !state_ref with
                    | Some new_s -> Navigation.update (fun _ -> new_s) ps
                    | None -> ps
                  else ps (* Handle fold keys *)
              else if key = "f" then (
                (* Fold all sections *)
                let new_state = State.fold_all_json s in
                state_ref := Some new_state ;
                Navigation.update (fun _ -> new_state) ps)
              else if key = "F" then (
                (* Unfold all sections *)
                let new_state = State.unfold_all_json s in
                state_ref := Some new_state ;
                Navigation.update (fun _ -> new_state) ps)
              else if key = "Tab" then cycle_instance ps
              else if is_browser_focused then
                (* Browser panel navigation in Result mode *)
                match Keys.of_string key with
                | Some Keys.Escape -> back ps
                | Some (Keys.Char "u") | Some Keys.Backspace -> (
                    (* Navigate back in browser while staying in Result mode *)
                    Actions.navigate_cached_back s update_state ;
                    match !state_ref with
                    | Some new_s -> Navigation.update (fun _ -> new_s) ps
                    | None -> ps)
                | Some Keys.Up | Some (Keys.Char "k") ->
                    let new_state = State.cached_cursor_up s in
                    state_ref := Some new_state ;
                    Navigation.update (fun _ -> new_state) ps
                | Some Keys.Down | Some (Keys.Char "j") ->
                    let new_state = State.cached_cursor_down s in
                    state_ref := Some new_state ;
                    Navigation.update (fun _ -> new_state) ps
                | Some Keys.Enter -> (
                    Actions.handle_cached_enter s update_state ;
                    match !state_ref with
                    | Some new_s -> Navigation.update (fun _ -> new_s) ps
                    | None -> ps)
                | Some Keys.Right ->
                    (* Switch focus to pager (last focused or 0) *)
                    let pager_id = State.get_focused_pager_id s in
                    let new_state = State.focus_pager pager_id s in
                    state_ref := Some new_state ;
                    Navigation.update (fun _ -> new_state) ps
                | _ -> ps
              else
                (* Pager-focused handling *)
                match Keys.of_string key with
                | Some Keys.Escape -> back ps
                | Some Keys.Left ->
                    (* Switch focus to browser panel *)
                    let new_state = State.focus_browser s in
                    state_ref := Some new_state ;
                    Navigation.update (fun _ -> new_state) ps
                | Some (Keys.Char " ") -> (
                    (* Toggle fold at cursor position (uses pager cursor mode) *)
                    match State.get_pager s with
                    | Some pager ->
                        let line = Pager.get_cursor_line pager in
                        let new_state = State.toggle_fold ~line s in
                        state_ref := Some new_state ;
                        Navigation.update (fun _ -> new_state) ps
                    | None -> ps)
                | _ -> (
                    (* Delegate to pager for all other keys *)
                    match State.get_pager s with
                    | Some pager ->
                        let win = size.LTerm_geom.rows - 3 in
                        let pager', _consumed =
                          Pager.handle_key pager ~key ~win
                        in
                        let new_state = State.set_pager pager' s in
                        state_ref := Some new_state ;
                        Navigation.update (fun _ -> new_state) ps
                    | None -> ps)))

let has_modal ps =
  (* Report modal-like state for both the global modal manager and the pager's
     own input mode (search, help). This lets Themed_page bypass global
     shortcut interception when the pager owns the keyboard. *)
  Miaou.Core.Modal_manager.has_active ()
  ||
  let s = ps.Navigation.s in
  match State.get_pager s with
  | Some p -> p.Pager.input_mode <> `None
  | None -> false

module Page_Impl : Miaou.Core.Tui_page.PAGE_SIG = struct
  type nonrec state = state

  type nonrec msg = msg

  type key_binding = state Miaou.Core.Tui_page.key_binding_desc

  type nonrec pstate = pstate

  let init = init

  let update = update

  let refresh = refresh

  let move = move

  let service_select = service_select

  let service_cycle = service_cycle

  let back = back

  let keymap = keymap

  let handled_keys = handled_keys

  let view = view

  let handle_key = handle_key

  let handle_modal_key = handle_modal_key

  let on_key ps key ~size =
    let ps' = handle_key ps (Miaou.Core.Keys.to_string key) ~size in
    (ps', Miaou_interfaces.Key_event.Handled)

  let on_modal_key ps key ~size =
    let ps' = handle_modal_key ps (Miaou.Core.Keys.to_string key) ~size in
    (ps', Miaou_interfaces.Key_event.Handled)

  let key_hints _ps =
    Miaou.Core.Tui_page.
      [
        {key = "Esc"; help = "Back"};
        {key = "↑/↓"; help = "Navigate"};
        {key = "r"; help = "Refresh"};
        {key = "Tab"; help = "Instance/Pager"};
        {key = "@"; help = "Target instance"};
        {key = "?"; help = "Help"};
      ]

  let has_modal = has_modal
end

module Page =
  Themed_page.Make
    (Page_Impl)
    (struct
      let page_name = "rpc_browser"
    end)

let page : Miaou.Core.Registry.page =
  (module Page : Miaou.Core.Tui_page.PAGE_SIG)

let register () =
  if not (Miaou.Core.Registry.exists name) then
    Miaou.Core.Registry.register name page
