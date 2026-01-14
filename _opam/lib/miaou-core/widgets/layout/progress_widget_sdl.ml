(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(* SDL-specific rendering for the progress bar: smooth turquoise gradient with
   a bright leading edge. *)

module W = Miaou_widgets_display.Widgets
module Palette = Miaou_widgets_display.Palette

(* Smooth turquoise gradient using continuous RGB blended and mapped to 38;5. *)

let rgb_of_code code =
  if code < 16 then
    let ansi_palette =
      [|
        (0, 0, 0);
        (128, 0, 0);
        (0, 128, 0);
        (128, 128, 0);
        (0, 0, 128);
        (128, 0, 128);
        (0, 128, 128);
        (192, 192, 192);
        (128, 128, 128);
        (255, 0, 0);
        (0, 255, 0);
        (255, 255, 0);
        (0, 0, 255);
        (255, 0, 255);
        (0, 255, 255);
        (255, 255, 255);
      |]
    in
    ansi_palette.(code)
  else if code >= 16 && code < 232 then
    let idx = code - 16 in
    let r = idx / 36 in
    let g = idx / 6 mod 6 in
    let b = idx mod 6 in
    let to_int c = if c = 0 then 0 else 55 + (c * 40) in
    (to_int r, to_int g, to_int b)
  else
    let level = 8 + ((code - 232) * 10) in
    (level, level, level)

let nearest_code r g b =
  let best = ref 0 in
  let best_d = ref max_int in
  for code = 0 to 255 do
    let cr, cg, cb = rgb_of_code code in
    let dr = cr - r in
    let dg = cg - g in
    let db = cb - b in
    let d = (dr * dr) + (dg * dg) + (db * db) in
    if d < !best_d then (
      best := code ;
      best_d := d)
  done ;
  !best

let lerp a b t = int_of_float (float a +. (float (b - a) *. t))

let color_rgb i total =
  let t =
    if total <= 1 then 0. else float_of_int i /. float_of_int (total - 1)
  in
  let start = (20, 170, 180) in
  let finish = (110, 255, 235) in
  let r0, g0, b0 = start in
  let r1, g1, b1 = finish in
  (lerp r0 r1 t, lerp g0 g1 t, lerp b0 b1 t)

let render_bar ~inner_w ~progress =
  let buf = Buffer.create (inner_w * 4) in
  let exact = progress *. float_of_int inner_w in
  for i = 0 to inner_w - 1 do
    let cr, cg, cb = color_rgb i inner_w in
    let base_r, base_g, base_b = (10, 20, 24) in
    let x = float i +. 0.5 in
    let p = exact in
    let strength =
      if p <= x -. 0.5 then 0.12
      else if p >= x +. 0.5 then 1.0
      else
        let t = (p -. (x -. 0.5)) /. 1.0 in
        0.12 +. (0.88 *. t)
    in
    let mix c_active c_base =
      int_of_float
        ((float c_active *. strength) +. (float c_base *. (1. -. strength)))
    in
    let r' = mix cr base_r in
    let g' = mix cg base_g in
    let b' = mix cb base_b in
    let code = nearest_code r' g' b' in
    let ansi =
      if strength > 0.15 then W.bg code " "
      else W.bg (nearest_code base_r base_g base_b) " "
    in
    Buffer.add_string buf ansi
  done ;
  Buffer.contents buf

let render ~width ~progress ~label ~title ~cols:_ =
  let bar_w = max 6 (width - 2) in
  let pct = int_of_float (floor ((100. *. progress) +. 0.5)) in
  let bar = render_bar ~inner_w:bar_w ~progress in
  let pct_s = Palette.fg_secondary (Printf.sprintf "%3d%%" pct) in
  let line = bar ^ " " ^ pct_s in
  let two_lines a b =
    let buf = Buffer.create (String.length a + String.length b + 1) in
    Buffer.add_string buf a ;
    Buffer.add_char buf '\n' ;
    Buffer.add_string buf b ;
    Buffer.contents buf
  in
  match (label, title) with
  | Some lbl, _ -> Palette.fg_steel lbl ^ "  " ^ line
  | None, Some t -> two_lines (W.titleize t) line
  | None, None -> line
