(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)
(* Pane splitting helpers for Miaou widgets *)

[@@@warning "-32-34-37-69"]

(* reuse visible/ansi helpers and glyphs from display widgets/helpers *)
module Helpers = Miaou_helpers.Helpers

let visible_chars_count = Miaou_helpers.Helpers.visible_chars_count

let visible_byte_index_of_pos = Miaou_helpers.Helpers.visible_byte_index_of_pos

let insert_before_reset = Miaou_helpers.Helpers.insert_before_reset

let has_trailing_reset = Miaou_helpers.Helpers.has_trailing_reset

let repeat s n =
  let buf = Buffer.create (max 0 n * String.length s) in
  for _ = 1 to max 0 n do
    Buffer.add_string buf s
  done ;
  Buffer.contents buf

let ansi = Miaou_widgets_display.Widgets.ansi

(* Canonical glyphs and unicode-border flag come from display Widgets so behavior is
	consistent across all widgets. Keep local short names for convenience. *)
let use_ascii_borders = Miaou_widgets_display.Widgets.use_ascii_borders

let glyph_corner_tl = Miaou_widgets_display.Widgets.glyph_corner_tl

let glyph_corner_tr = Miaou_widgets_display.Widgets.glyph_corner_tr

let glyph_corner_bl = Miaou_widgets_display.Widgets.glyph_corner_bl

let glyph_corner_br = Miaou_widgets_display.Widgets.glyph_corner_br

let glyph_hline = Miaou_widgets_display.Widgets.glyph_hline

let glyph_vline = Miaou_widgets_display.Widgets.glyph_vline

let pad_right s n =
  let v = visible_chars_count s in
  if v >= n then s else Miaou_helpers.Helpers.pad_to_width s n ' '

let trunc_visible n s =
  let v = visible_chars_count s in
  if v <= n then s
  else
    let byte_idx = visible_byte_index_of_pos s (max 0 (n - 1)) in
    let prefix = String.sub s 0 byte_idx in
    prefix ^ "…"

let split_lines s = String.split_on_char '\n' s

let normalize_lines width wrap lines =
  let rec wrap_line w acc line =
    if line = "" then List.rev ("" :: acc)
    else
      let v = visible_chars_count line in
      if v <= w then List.rev (line :: acc)
      else if wrap then
        let byte_idx = visible_byte_index_of_pos line w in
        let part = String.sub line 0 byte_idx in
        let rest = String.sub line byte_idx (String.length line - byte_idx) in
        wrap_line w (part :: acc) rest
      else List.rev (trunc_visible w line :: acc)
  in
  let rec go acc = function
    | [] -> List.rev acc
    | x :: xs ->
        let parts = wrap_line width [] x in
        go (List.rev_append parts acc) xs
  in
  go [] lines

let make_rows ~lines ~height =
  let rec go acc i =
    if i >= height then List.rev acc
    else
      let row = if i < List.length lines then List.nth lines i else "" in
      go (row :: acc) (i + 1)
  in
  go [] 0

let split_vertical_with_left_width ~width ~left_pad ~right_pad ~border ~wrap
    ~sep ~left ~right ~left_width =
  let total_w = width in
  let inner_w = if border then total_w - 4 else total_w in
  let left_w = max 0 (min inner_w left_width) in
  let right_w = inner_w - left_w in
  (* reserve left_pad/right_pad columns as padding inside each pane *)
  let left_content_w = max 0 (left_w - left_pad) in
  let right_content_w = max 0 (right_w - right_pad) in
  let left_lines_raw = normalize_lines left_content_w wrap (split_lines left) in
  let right_lines_raw =
    normalize_lines right_content_w wrap (split_lines right)
  in
  let left_lines =
    List.map (fun l -> String.make left_pad ' ' ^ l) left_lines_raw
  in
  let right_lines =
    List.map (fun l -> l ^ String.make right_pad ' ') right_lines_raw
  in
  let height = max (List.length left_lines) (List.length right_lines) in
  let left_rows = make_rows ~lines:left_lines ~height in
  let right_rows = make_rows ~lines:right_lines ~height in
  let rows =
    List.mapi
      (fun i l ->
        pad_right l left_w ^ sep ^ pad_right (List.nth right_rows i) right_w)
      left_rows
  in
  let body = Helpers.concat_lines rows in
  if border then
    let top =
      glyph_corner_tl ^ repeat glyph_hline (total_w - 2) ^ glyph_corner_tr
    in
    let bot =
      glyph_corner_bl ^ repeat glyph_hline (total_w - 2) ^ glyph_corner_br
    in
    top ^ "\n" ^ body ^ "\n" ^ bot
  else body

(* Backwards-compatible wrapper: choose half/half split when left width not specified. *)
let split_vertical ~width ~left_pad ~right_pad ~border ~wrap ~sep ~left ~right =
  let inner_w = if border then width - 4 else width in
  let left_w = inner_w / 2 in
  split_vertical_with_left_width
    ~width
    ~left_pad
    ~right_pad
    ~border
    ~wrap
    ~sep
    ~left
    ~right
    ~left_width:left_w

let split_horizontal ~height ~top_pad ~bottom_pad ~border ~wrap ~sep ~top
    ~bottom =
  let top_lines = split_lines top in
  let bottom_lines = split_lines bottom in
  (* apply vertical padding lines *)
  let top_padded = List.init top_pad (fun _ -> "") @ top_lines in
  let bottom_padded = bottom_lines @ List.init bottom_pad (fun _ -> "") in
  let top_s = Helpers.concat_lines top_padded in
  let bottom_s = Helpers.concat_lines bottom_padded in
  let frame_width = max 10 height in
  let _ = wrap in
  if border then
    let top_frame =
      glyph_corner_tl ^ repeat glyph_hline (frame_width - 2) ^ glyph_corner_tr
    in
    top_frame ^ "\n" ^ top_s ^ "\n" ^ sep ^ "\n" ^ bottom_s
  else top_s ^ "\n" ^ sep ^ "\n" ^ bottom_s
