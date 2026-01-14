(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)
[@@@warning "-32-34-37-69"]

(* A small, Bubble Tea–styled progress bar using the palette gradient. *)

type t = {
  width : int; (* visible columns of the bar portion *)
  progress : float; (* 0.0 .. 1.0 *)
  label : string option; (* optional label shown to the left *)
  title : string option; (* used for centered/modal variants *)
}

let clamp01 x = if x < 0. then 0. else if x > 1. then 1. else x

let open_inline ~width ?label () =
  {width = max 1 width; progress = 0.; label; title = None}

let open_centered ~title ~width () =
  {width = max 1 width; progress = 0.; label = None; title = Some title}

let set_progress w p = {w with progress = clamp01 p}

let get_progress w = w.progress

let set_label w l = {w with label = l}

(* Use centralized helpers for ANSI/UTF-8 handling via display Widgets. *)
let visible_length = Miaou_helpers.Helpers.visible_chars_count

let render_bar ~width ~progress : string =
  let left =
    if Lazy.force Miaou_widgets_display.Widgets.use_ascii_borders then "["
    else "▕"
  in
  let right =
    if Lazy.force Miaou_widgets_display.Widgets.use_ascii_borders then "]"
    else "▏"
  in
  let inner_w = max 1 (width - 2) in
  if Lazy.force Miaou_widgets_display.Widgets.use_ascii_borders then
    let filled = int_of_float (floor (float_of_int inner_w *. progress)) in
    let empty = max 0 (inner_w - filled) in
    let filled_str = String.make filled '#' in
    let empty_str = String.make empty ' ' in
    left ^ filled_str ^ empty_str ^ right
  else
    let exact_filled_len = float_of_int inner_w *. progress in
    let whole_cells = int_of_float exact_filled_len in
    let remainder = exact_filled_len -. float_of_int whole_cells in
    let partial_blocks = [|""; "▏"; "▎"; "▍"; "▌"; "▋"; "▊"; "▉"|] in
    let partial_idx =
      int_of_float (remainder *. float_of_int (Array.length partial_blocks))
    in
    let tip_char =
      if whole_cells < inner_w then partial_blocks.(partial_idx) else ""
    in
    let filled_uncolored =
      let buf = Buffer.create ((whole_cells + 2) * 3) in
      for _ = 1 to whole_cells do
        Buffer.add_string buf "█"
      done ;
      Buffer.add_string buf tip_char ;
      Buffer.contents buf
    in
    let filled_colored =
      Miaou_widgets_display.Palette.purple_gradient_line
        Miaou_widgets_display.Palette.Right
        filled_uncolored
    in
    let filled_visible_w = whole_cells + if tip_char = "" then 0 else 1 in
    let empty_w = max 0 (inner_w - filled_visible_w) in
    let empty_str = String.make empty_w ' ' in
    left ^ filled_colored ^ empty_str ^ right

let render_terminal w ~cols:_ =
  let bar = render_bar ~width:w.width ~progress:w.progress in
  let pct = int_of_float (floor ((100. *. w.progress) +. 0.5)) in
  let pct_s = Printf.sprintf "%3d%%" pct in
  let pct_colored = Miaou_widgets_display.Palette.fg_steel pct_s in
  let bar_and_pct = bar ^ " " ^ pct_colored in
  match (w.label, w.title) with
  | Some lbl, _ ->
      (* Render label to the left, clipped to avoid wrapping. *)
      let clip_to n s =
        if visible_length s <= n then s
        else
          let idx =
            Miaou_helpers.Helpers.visible_byte_index_of_pos s (max 0 (n - 1))
          in
          String.sub s 0 idx ^ "…"
      in
      let lbl' = clip_to 24 (Miaou_widgets_display.Palette.fg_steel lbl) in
      lbl' ^ "  " ^ bar_and_pct
  | None, Some t ->
      (* Centered variant: include a small title header line. Outer centering is
				 handled by caller. *)
      let title = Miaou_widgets_display.Widgets.titleize t in
      let buf =
        Buffer.create (String.length title + String.length bar_and_pct + 1)
      in
      Buffer.add_string buf title ;
      Buffer.add_char buf '\n' ;
      Buffer.add_string buf bar_and_pct ;
      Buffer.contents buf
  | None, None -> bar_and_pct

let render w ~cols =
  match Miaou_widgets_display.Widgets.get_backend () with
  | `Sdl ->
      Progress_widget_sdl.render
        ~width:w.width
        ~progress:w.progress
        ~label:w.label
        ~title:w.title
        ~cols
  | `Terminal -> render_terminal w ~cols

let handle_key w ~key:_ = w
