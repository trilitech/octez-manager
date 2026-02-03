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

let render_instance_selector ~target =
  match target with
  | None -> Widgets.dim "No instance selected"
  | Some svc ->
      let name = svc.Service.instance in
      let network = svc.Service.network in
      Printf.sprintf "%s (%s)" (Widgets.bold name) (Widgets.dim network)

let render_entry_kind = function
  | State.Get -> Widgets.green "[GET]"
  | State.Sub -> Widgets.fg 14 "[SUB]"
  | State.Dyn typ -> Widgets.yellow (Printf.sprintf "[DYN:%s]" typ)
  | State.DynValue (typ, _) -> Widgets.dim (Printf.sprintf "[%s]" typ)
  | State.ChangeTarget -> Widgets.fg 11 "[TARGET]"

let render_entry ~cursor ~idx entry =
  let is_selected = cursor = idx in
  let marker = if is_selected then Widgets.fg 14 "▸ " else "  " in
  (* For GET at current path, show [GET] as the name *)
  let display_name =
    match (entry.State.name, entry.State.kind) with
    | "", State.Get -> "[GET]"
    | name, _ -> name
  in
  let name = if is_selected then Widgets.bold display_name else display_name in
  let kind = render_entry_kind entry.State.kind in
  Printf.sprintf "%s%-40s %s" marker name kind

let render_loading ?(msg = "Loading...") () =
  let spinner = Context.render_spinner "" in
  Printf.sprintf "%s %s" spinner (Widgets.dim msg)

let render_error = function
  | None -> []
  | Some msg -> [Widgets.red ("Error: " ^ msg)]

let render_header ~target ~path =
  let instance = render_instance_selector ~target in
  let breadcrumb = render_breadcrumb path in
  Printf.sprintf "RPC Browser │ %s │ %s" instance breadcrumb

let render_entries ~cursor ~entries =
  if entries = [] then [Widgets.dim "  (no entries at this path)"]
  else List.mapi (fun idx entry -> render_entry ~cursor ~idx entry) entries

let render_shortcuts ~state =
  let shortcuts = Rpc_browser_actions.get_shortcuts state in
  let header = Widgets.bold "Quick Access:" in
  let items =
    List.map
      (fun (key, path, desc) ->
        Printf.sprintf
          "  %s. %s  %s"
          (Widgets.fg 14 key)
          (Widgets.bold path)
          (Widgets.dim (Printf.sprintf "(%s)" desc)))
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
  Widgets.dim (String.concat "  " parts)

let render ~state ~cols =
  let _ = cols in
  (* Get the actual target: override if set, else instances[selected_idx] *)
  let target =
    match state.State.target_override with
    | Some _ as t -> t
    | None -> List.nth_opt state.State.instances state.State.selected_idx
  in
  let header =
    render_header
      ~target
      ~path:state.State.path
  in
  let separator = Widgets.dim (String.make 60 '-') in
  match state.State.mode with
  | State.List {entries; cursor; loading} ->
      let shortcuts_section =
        if state.State.path = [] then render_shortcuts ~state @ [""] else []
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
