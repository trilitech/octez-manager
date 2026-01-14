(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)
(* State *)
type t = {idx : int; label : string option; width : int}

let open_centered ?label ?(width = 60) () = {idx = 0; label; width}

let tick t = {t with idx = t.idx + 1}

let set_label t lbl = {t with label = lbl}

let frames_unicode = [|"⠋"; "⠙"; "⠹"; "⠸"; "⠼"; "⠴"; "⠦"; "⠧"; "⠇"; "⠏"|]

let frames_ascii = [|"|"; "/"; "-"; "\\"|]

let render_with_backend backend t =
  let frames =
    if Miaou_widgets_display.Widgets.prefer_ascii ~backend () then frames_ascii
    else frames_unicode
  in
  let frame_count = Array.length frames in
  let glyph = frames.(t.idx mod frame_count) in
  let label = match t.label with None -> "" | Some s -> " " ^ s in
  let content = Printf.sprintf "%s%s" glyph label in
  let v = Miaou_helpers.Helpers.visible_chars_count content in
  if v <= t.width then content
  else
    let idx = Miaou_helpers.Helpers.visible_byte_index_of_pos content t.width in
    String.sub content 0 idx

let render
    ?(backend : Miaou_widgets_display.Widgets.backend =
      Miaou_widgets_display.Widgets.get_backend ()) t =
  render_with_backend backend t
