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
  | [] -> Widgets.bold "/"
  | segments ->
      let parts =
        Widgets.dim "/" :: List.map (fun seg -> Widgets.bold seg) segments
      in
      String.concat " / " parts

let render_instance_selector ~instances ~selected_idx =
  match List.nth_opt instances selected_idx with
  | None -> Widgets.dim "No instance selected"
  | Some svc ->
      let name = svc.Service.instance in
      let network = svc.Service.network in
      Printf.sprintf "%s (%s)" (Widgets.bold name) (Widgets.dim network)

let render_entry_kind = function
  | State.Get -> Widgets.green "[GET]"
  | State.Sub -> Widgets.fg 14 "[SUB]"
  | State.Dyn typ -> Widgets.yellow (Printf.sprintf "[DYN:%s]" typ)

let render_entry ~cursor ~idx entry =
  let is_selected = cursor = idx in
  let marker = if is_selected then Widgets.fg 14 "▸ " else "  " in
  let name =
    if is_selected then Widgets.bold entry.State.name else entry.State.name
  in
  let kind = render_entry_kind entry.State.kind in
  Printf.sprintf "%s%-40s %s" marker name kind

let render_loading ?(msg = "Loading...") () =
  let spinner = Context.render_spinner "" in
  Printf.sprintf "%s %s" spinner (Widgets.dim msg)

let render_error = function
  | None -> []
  | Some msg -> [Widgets.red ("Error: " ^ msg)]

let render_header ~instances ~selected_idx ~path =
  let instance = render_instance_selector ~instances ~selected_idx in
  let breadcrumb = render_breadcrumb path in
  Printf.sprintf "RPC Browser │ %s │ %s" instance breadcrumb

let render_entries ~cursor ~entries =
  if entries = [] then [Widgets.dim "  (no entries at this path)"]
  else List.mapi (fun idx entry -> render_entry ~cursor ~idx entry) entries

let render_shortcuts () =
  let header = Widgets.bold "Quick Access:" in
  let items =
    List.map
      (fun (key, path, desc) ->
        Printf.sprintf
          "  %s. %s  %s"
          (Widgets.fg 14 key)
          (Widgets.bold path)
          (Widgets.dim (Printf.sprintf "(%s)" desc)))
      Rpc_browser_actions.shortcuts
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
  Widgets.dim (String.concat "  " parts)

let render ~state ~cols =
  let _ = cols in
  let header =
    render_header
      ~instances:state.State.instances
      ~selected_idx:state.State.selected_idx
      ~path:state.State.path
  in
  let separator = Widgets.dim (String.make 60 '-') in
  match state.State.mode with
  | State.List {entries; cursor; loading} ->
      let shortcuts_section =
        if state.State.path = [] then render_shortcuts () @ [""] else []
      in
      let entries_header =
        if state.State.path = [] then [Widgets.bold "All Endpoints:"] else []
      in
      let content =
        if loading then [render_loading ()] else render_entries ~cursor ~entries
      in
      let error_lines = render_error state.State.error in
      let help = render_help () in
      [header; separator] @ shortcuts_section @ entries_header @ content
      @ error_lines @ [separator; help]
  | State.Result _ ->
      (* Result mode is handled by a different renderer *)
      [header; separator; Widgets.dim "  (result mode - use detail renderer)"]
