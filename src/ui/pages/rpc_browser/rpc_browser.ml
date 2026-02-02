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

let enter ps =
  let s = ps.Navigation.s in
  Actions.handle_enter s update_state ;
  match !state_ref with
  | Some new_s -> Navigation.update (fun _ -> new_s) ps
  | None -> ps

let cycle_instance ps =
  let s = ps.Navigation.s in
  let new_state = Actions.cycle_instance ~delta:1 s in
  let new_state = Actions.fetch_entries_sync new_state in
  state_ref := Some new_state ;
  Navigation.update (fun _ -> new_state) ps

let keymap _ps =
  let noop ps = ps in
  let kb key action help =
    {Miaou.Core.Tui_page.key; action; help; display_only = false}
  in
  [
    kb "Esc" back "Back";
    kb "Enter" enter "Select";
    kb "↑/↓" noop "Navigate";
    kb "r" refresh "Refresh";
    kb "Tab" cycle_instance "Instance";
    {
      Miaou.Core.Tui_page.key = "?";
      action = noop;
      help = "Help";
      display_only = true;
    };
  ]

(* Minimum width for side-by-side layout *)
let side_by_side_min_width = 140

(* Render two columns side by side *)
let render_side_by_side ~left ~right ~left_width ~total_width ~rows =
  let left_lines = String.split_on_char '\n' left in
  let right_lines = String.split_on_char '\n' right in
  let separator = Widgets.dim "│" in
  (* Use Widgets.visible_chars_count for proper ANSI handling *)
  let truncate_line line width =
    let visible_len = Widgets.visible_chars_count line in
    if visible_len <= width then line
    else
      (* Truncate to width visible chars - need to find byte position *)
      let byte_idx = Widgets.visible_byte_index_of_pos line width in
      String.sub line 0 byte_idx ^ "\027[0m"
  in
  let pad_line line width =
    let visible_len = Widgets.visible_chars_count line in
    if visible_len >= width then truncate_line line width
    else line ^ String.make (width - visible_len) ' '
  in
  let right_width = total_width - left_width - 1 in
  let combined =
    List.mapi
      (fun i _ ->
        let left_line =
          match List.nth_opt left_lines i with Some l -> l | None -> ""
        in
        let right_line =
          match List.nth_opt right_lines i with Some r -> r | None -> ""
        in
        Printf.sprintf "%s%s%s"
          (pad_line left_line left_width)
          separator
          (pad_line right_line right_width))
      (List.init rows (fun i -> i))
  in
  String.concat "\n" combined

let view ps ~focus ~size =
  let s = ps.Navigation.s in
  let cols = size.LTerm_geom.cols in
  let rows = size.LTerm_geom.rows in
  let body =
    match s.State.mode with
    | State.List _ ->
        let lines = Rpc_browser_render_list.render ~state:s ~cols in
        String.concat "\n" lines
    | State.Result _ ->
        (* Check if we should use side-by-side layout *)
        if cols >= side_by_side_min_width then
          (* Side-by-side: browser on left, result on right *)
          let left_width = 50 in
          let right_width = cols - left_width - 1 in
          let browser_focus = focus && s.State.focus = State.FocusBrowser in
          let pager_focus = focus && s.State.focus = State.FocusPager in
          (* Render browser list using cached entries *)
          let left_state =
            {s with mode = State.List {
              entries = s.State.cached_entries;
              cursor = s.State.cached_cursor;
              loading = false
            }}
          in
          let left_lines = Rpc_browser_render_list.render ~state:left_state ~cols:left_width in
          let left =
            if browser_focus then
              (Widgets.fg 14 "▶ Browser") :: left_lines |> String.concat "\n"
            else
              (Widgets.dim "  Browser") :: left_lines |> String.concat "\n"
          in
          let right = Rpc_browser_render_result.render ~state:s ~cols:right_width ~rows ~focus:pager_focus in
          render_side_by_side ~left ~right ~left_width ~total_width:cols ~rows
        else
          (* Regular full-width result view *)
          let pager_focus = focus && s.State.focus = State.FocusPager in
          Rpc_browser_render_result.render ~state:s ~cols ~rows ~focus:pager_focus
  in
  Vsection.render ~size ~header:[] ~content_footer:[] ~child:(fun _ -> body)

let handle_modal_key ps key ~size:_ =
  Miaou.Core.Modal_manager.handle_key key ;
  ps

let handle_key ps key ~size =
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
        (* Handle save key *)
        if key = "s" then (
          match s.State.mode with
          | State.Result {raw_body; request; _} ->
              (* Save to file - use raw JSON (unfolded, no colors) *)
              let filename =
                let base =
                  request
                  |> String.split_on_char '/'
                  |> List.filter (fun s -> s <> "")
                  |> String.concat "_"
                in
                Printf.sprintf "rpc_%s_%d.json" base (int_of_float (Unix.time ()))
              in
              (try
                 let oc = open_out filename in
                 output_string oc raw_body ;
                 close_out oc ;
                 let new_state = State.set_error (Printf.sprintf "Saved to %s" filename) s in
                 state_ref := Some new_state ;
                 Navigation.update (fun _ -> new_state) ps
               with exn ->
                 let new_state = State.set_error (Printf.sprintf "Save failed: %s" (Printexc.to_string exn)) s in
                 state_ref := Some new_state ;
                 Navigation.update (fun _ -> new_state) ps)
          | _ -> ps)
        (* Handle fold keys *)
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
        else
          match Keys.of_string key with
          | Some Keys.Escape -> back ps
          | Some Keys.Left ->
              (* Switch focus to browser panel *)
              let new_state = State.set_focus State.FocusBrowser s in
              state_ref := Some new_state ;
              Navigation.update (fun _ -> new_state) ps
          | Some Keys.Right ->
              (* Switch focus to pager panel *)
              let new_state = State.set_focus State.FocusPager s in
              state_ref := Some new_state ;
              Navigation.update (fun _ -> new_state) ps
          | Some (Keys.Char " ") ->
              (* Toggle fold at cursor position (uses pager cursor mode) *)
              (match State.get_pager s with
              | Some pager ->
                  let line = Pager.get_cursor_line pager in
                  let new_state = State.toggle_fold ~line s in
                  state_ref := Some new_state ;
                  Navigation.update (fun _ -> new_state) ps
              | None -> ps)
          | _ ->
              (* Delegate to pager for all other keys *)
              (match State.get_pager s with
              | Some pager ->
                  let win = size.LTerm_geom.rows - 3 in
                  let pager', _consumed = Pager.handle_key pager ~key ~win in
                  let new_state = State.set_pager pager' s in
                  state_ref := Some new_state ;
                  Navigation.update (fun _ -> new_state) ps
              | None -> ps))

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
