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
module Service_state = Data.Service_state

let name = "rpc_browser"

module State = Rpc_browser_state
module Actions = Rpc_browser_actions

type state = State.state

type msg = unit

type pstate = state Navigation.t

let state_ref : state option ref = ref None

let update_state s =
  state_ref := Some s ;
  Context.mark_instances_dirty ()

let init () =
  let service_states = Data.load_service_states () in
  let nodes =
    List.filter_map
      (fun (ss : Service_state.t) ->
        if ss.service.Octez_manager_lib.Service.role = "node" then
          Some ss.service
        else None)
      service_states
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

let keymap _ps =
  let noop ps = ps in
  let kb key action help =
    {Miaou.Core.Tui_page.key; action; help; display_only = false}
  in
  [
    kb "Esc" back "Back";
    kb "Enter" noop "Select";
    kb "↑/↓" noop "Navigate";
    kb "r" refresh "Refresh";
    kb "Tab" noop "Instance";
    {
      Miaou.Core.Tui_page.key = "?";
      action = noop;
      help = "Help";
      display_only = true;
    };
  ]

let view ps ~focus:_ ~size =
  let s = ps.Navigation.s in
  let cols = size.LTerm_geom.cols in
  let rows = size.LTerm_geom.rows in
  let lines =
    match s.State.mode with
    | State.List _ -> Rpc_browser_render_list.render ~state:s ~cols
    | State.Result _ -> Rpc_browser_render_result.render ~state:s ~cols ~rows
  in
  let body = String.concat "\n" lines in
  Vsection.render ~size ~header:[] ~content_footer:[] ~child:(fun _ -> body)

let handle_modal_key ps key ~size:_ =
  Miaou.Core.Modal_manager.handle_key key ;
  ps

let handle_key ps key ~size:_ =
  if Miaou.Core.Modal_manager.has_active () then (
    Miaou.Core.Modal_manager.handle_key key ;
    ps)
  else
    let s = ps.Navigation.s in
    match s.State.mode with
    | State.List _ -> (
        (* Check for shortcut keys (1-9) when at root *)
        let shortcut_keys = ["1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"] in
        let is_shortcut_key =
          s.State.path = [] && List.exists (fun k -> key = k) shortcut_keys
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
          | Some Keys.Tab ->
              let new_state = Actions.cycle_instance ~delta:1 s in
              let new_state = Actions.fetch_entries_sync new_state in
              state_ref := Some new_state ;
              Navigation.update (fun _ -> new_state) ps
          | _ -> ps)
    | State.Result _ -> (
        match Keys.of_string key with
        | Some Keys.Escape -> back ps
        | Some Keys.Up | Some (Keys.Char "k") ->
            let new_state = State.scroll (-1) s in
            state_ref := Some new_state ;
            Navigation.update (fun _ -> new_state) ps
        | Some Keys.Down | Some (Keys.Char "j") ->
            let new_state = State.scroll 1 s in
            state_ref := Some new_state ;
            Navigation.update (fun _ -> new_state) ps
        | Some Keys.PageUp ->
            let new_state = State.scroll (-10) s in
            state_ref := Some new_state ;
            Navigation.update (fun _ -> new_state) ps
        | Some Keys.PageDown ->
            let new_state = State.scroll 10 s in
            state_ref := Some new_state ;
            Navigation.update (fun _ -> new_state) ps
        | Some (Keys.Char "g") ->
            let new_state =
              match s.State.mode with
              | State.Result r ->
                  {s with mode = State.Result {r with scroll_offset = 0}}
              | _ -> s
            in
            state_ref := Some new_state ;
            Navigation.update (fun _ -> new_state) ps
        | Some (Keys.Char "G") ->
            let new_state =
              match s.State.mode with
              | State.Result r ->
                  let lines = String.split_on_char '\n' r.body in
                  let total = List.length lines in
                  {
                    s with
                    mode =
                      State.Result {r with scroll_offset = max 0 (total - 20)};
                  }
              | _ -> s
            in
            state_ref := Some new_state ;
            Navigation.update (fun _ -> new_state) ps
        | _ -> ps)

let has_modal _ = Miaou.Core.Modal_manager.has_active ()

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

  let has_modal = has_modal
end

module Page =
  Monitored_page.Make
    (Page_Impl)
    (struct
      let page_name = "rpc_browser"
    end)

let page : Miaou.Core.Registry.page =
  (module Page : Miaou.Core.Tui_page.PAGE_SIG)

let register () =
  if not (Miaou.Core.Registry.exists name) then
    Miaou.Core.Registry.register name page
