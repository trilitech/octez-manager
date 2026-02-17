(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Octez_manager_lib
module Widgets = Miaou_widgets_display.Widgets
module State = Rpc_browser_state

let render_breadcrumb path =
  match path with
  | [] -> Widgets.themed_emphasis "/"
  | segments ->
      let parts =
        Widgets.themed_muted "/"
        :: List.map (fun seg -> Widgets.themed_emphasis seg) segments
      in
      String.concat " / " parts

let render_instance_selector ~target =
  match target with
  | None -> Widgets.themed_muted "No instance selected"
  | Some svc ->
      let name = svc.Service.instance in
      let network = svc.Service.network in
      Printf.sprintf
        "%s (%s)"
        (Widgets.themed_emphasis name)
        (Widgets.themed_muted network)

let render_entry_kind = function
  | State.Get -> Widgets.themed_success "[GET]"
  | State.Sub -> Widgets.themed_accent "[SUB]"
  | State.Dyn typ -> Widgets.themed_warning (Printf.sprintf "[DYN:%s]" typ)
  | State.DynValue (typ, _) -> Widgets.themed_muted (Printf.sprintf "[%s]" typ)
  | State.ChangeTarget -> Widgets.themed_info "[TARGET]"

let render_entry ~cursor ~idx ~focus entry =
  let is_selected = cursor = idx in
  let marker =
    if is_selected then
      if focus then Widgets.themed_accent "▸ " else Widgets.themed_muted "▸ "
    else "  "
  in
  (* For GET at current path, show [GET] as the name *)
  let display_name =
    match (entry.State.name, entry.State.kind) with
    | "", State.Get -> "[GET]"
    | name, _ -> name
  in
  let name =
    if is_selected then
      if focus then Widgets.themed_emphasis display_name
      else Widgets.themed_muted display_name
    else display_name
  in
  let kind = render_entry_kind entry.State.kind in
  Printf.sprintf "%s%-40s %s" marker name kind

let render_loading ?(msg = "Loading...") () =
  let spinner = Context.render_spinner "" in
  Printf.sprintf "%s %s" spinner (Widgets.themed_muted msg)

let render_error = function
  | None -> []
  | Some msg -> [Widgets.themed_error ("Error: " ^ msg)]

let render_header ~target ~path =
  (* OpenCode style: clean header without heavy box characters *)
  let instance = render_instance_selector ~target in
  let breadcrumb = render_breadcrumb path in
  Printf.sprintf "RPC Browser  |  %s  |  %s" instance breadcrumb

let render_entries ~cursor ~entries ~focus =
  if entries = [] then [Widgets.themed_muted "  (no entries at this path)"]
  else
    List.mapi (fun idx entry -> render_entry ~cursor ~idx ~focus entry) entries

let render_shortcuts ~state =
  let shortcuts = Rpc_browser_actions.get_shortcuts state in
  let header = Widgets.themed_emphasis "Quick Access:" in
  let items =
    List.map
      (fun (key, path, desc) ->
        Printf.sprintf
          "  %s. %s  %s"
          (Widgets.themed_accent key)
          (Widgets.themed_emphasis path)
          (Widgets.themed_muted (Printf.sprintf "(%s)" desc)))
      shortcuts
  in
  header :: items

let render_help () =
  let keys =
    [
      ("↑/↓", "navigate");
      ("Enter", "select");
      ("Backspace", "up");
      ("Tab", "instance");
      ("Esc", "back");
    ]
  in
  let parts = List.map (fun (k, v) -> Printf.sprintf "%s: %s" k v) keys in
  Widgets.themed_muted (String.concat "  " parts)

let render ~focus ~state ~cols =
  (* Truncate a line to fit within cols, preserving ANSI codes *)
  let truncate line =
    let visible_len = Widgets.visible_chars_count line in
    if visible_len <= cols then line
    else
      let byte_idx = Widgets.visible_byte_index_of_pos line (cols - 3) in
      String.sub line 0 byte_idx ^ "\027[0m..."
  in
  (* Get the actual target: override if set, else instances[selected_idx] *)
  let target =
    match state.State.target_override with
    | Some _ as t -> t
    | None -> List.nth_opt state.State.instances state.State.selected_idx
  in
  let header = render_header ~target ~path:state.State.path in
  (* OpenCode style: no heavy separators, just empty line for spacing *)
  match state.State.mode with
  | State.List {entries; cursor; loading} ->
      let shortcuts_section =
        if state.State.path = [] then render_shortcuts ~state @ [""] else []
      in
      let entries_header =
        if state.State.path = [] then [Widgets.themed_emphasis "All Endpoints:"]
        else []
      in
      let content =
        if loading then [render_loading ()]
        else render_entries ~cursor ~entries ~focus
      in
      let error_lines = render_error state.State.error in
      let help = render_help () in
      let lines =
        [header; ""] @ shortcuts_section @ entries_header @ content
        @ error_lines @ [""; help]
      in
      List.map truncate lines
  | State.Result _ ->
      (* Result mode is handled by a different renderer *)
      List.map
        truncate
        [
          header;
          "";
          Widgets.themed_muted "  (result mode - use detail renderer)";
        ]
