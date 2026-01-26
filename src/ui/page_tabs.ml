(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

(** Shared tabs widget for main page navigation *)

module Tabs = Miaou_widgets_navigation.Tabs_widget
module Widgets = Miaou_widgets_display.Widgets

(** Create the main navigation tabs *)
let make_tabs () =
  Tabs.(
    make
      [
        tab ~id:"instances" ~label:"Instances";
        tab ~id:"binaries" ~label:"Binaries";
        tab ~id:"diagnostics" ~label:"Diagnostics";
      ])

(** Set the active tab by page name *)
let select_tab tabs ~page_name = Tabs.select tabs ~id:page_name

(** Move tab selection left/right *)
let move_left tabs = Tabs.move tabs `Left

let move_right tabs = Tabs.move tabs `Right

(** Get the currently selected (focused) tab ID *)
let get_focused_tab tabs =
  match Tabs.current tabs with Some tab -> Some (Tabs.id tab) | None -> None

(** Custom render that shows current page highlighted and focused tab with cursor
    @param tabs The tabs widget
    @param current_page_name The name of the currently displayed page
    @param has_focus Whether the tabs bar itself is focused (selected=-1) *)
let render tabs ~current_page_name ~has_focus =
  let pad s = Printf.sprintf " %s " s in
  let all_tabs =
    [
      ("instances", "Instances");
      ("binaries", "Binaries");
      ("diagnostics", "Diagnostics");
    ]
  in
  let focused_tab = get_focused_tab tabs in
  let rendered =
    List.map
      (fun (id, label) ->
        let is_current = String.equal id current_page_name in
        let is_focused =
          has_focus
          &&
          match focused_tab with
          | Some fid -> String.equal fid id
          | None -> false
        in
        if is_current && is_focused then
          (* Current page AND focused: bold with cursor *)
          Widgets.bold (Printf.sprintf ">%s<" (pad label))
        else if is_current then
          (* Current page but not focused: just bold *)
          Widgets.bold (pad label)
        else if is_focused then
          (* Not current but focused: show cursor *)
          Printf.sprintf ">%s<" (pad label)
        else
          (* Neither: dim *)
          Widgets.dim (pad label))
      all_tabs
  in
  String.concat " " rendered

(** Navigate to the currently focused tab *)
let navigate_to_focused tabs =
  match get_focused_tab tabs with
  | Some page_id -> Context.navigate page_id
  | None -> ()
