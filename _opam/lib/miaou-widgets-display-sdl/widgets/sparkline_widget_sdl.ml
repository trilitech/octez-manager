(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(* SDL-enhanced sparkline widget with smooth vector rendering *)

module W = Miaou_widgets_display.Widgets
module Sparkline_widget = Miaou_widgets_display.Sparkline_widget

type sdl_render_info = {
  renderer : Tsdl.Sdl.renderer;
  x : int;
  y : int;
  width : int;
  height : int;
  char_w : int;
  char_h : int;
}

(* Parse ANSI color code to RGB *)
let ansi_to_rgb ansi_code =
  match ansi_code with
  | "30" -> (0, 0, 0) (* black *)
  | "31" -> (220, 50, 47) (* red *)
  | "32" -> (133, 153, 0) (* green *)
  | "33" -> (181, 137, 0) (* yellow *)
  | "34" -> (38, 139, 210) (* blue *)
  | "35" -> (211, 54, 130) (* magenta *)
  | "36" -> (42, 161, 152) (* cyan *)
  | "37" -> (238, 232, 213) (* white *)
  | "90" -> (88, 110, 117) (* bright black *)
  | "91" -> (220, 50, 47) (* bright red *)
  | "92" -> (133, 153, 0) (* bright green *)
  | "93" -> (181, 137, 0) (* bright yellow *)
  | "94" -> (38, 139, 210) (* bright blue *)
  | "95" -> (211, 54, 130) (* bright magenta *)
  | "96" -> (42, 161, 152) (* bright cyan *)
  | "97" -> (253, 246, 227) (* bright white *)
  | _ -> (133, 153, 0)
(* default green *)

(* Render sparkline with SDL using smooth lines and filled area *)
let render_sdl info sparkline ~focus:_ ~show_value:_ ?color ?(thresholds = [])
    () =
  let open Tsdl.Sdl in
  let renderer = info.renderer in

  if Sparkline_widget.is_empty sparkline then ()
  else
    let values = Sparkline_widget.get_data sparkline in
    let min_val, max_val, _current = Sparkline_widget.get_bounds sparkline in

    let range = max_val -. min_val in
    let pixel_width = info.width * info.char_w in
    let pixel_height = info.height * info.char_h in

    (* Map value to pixel Y coordinate (inverted: 0 = top) *)
    let value_to_y v =
      if range = 0. then pixel_height / 2
      else
        let normalized = (v -. min_val) /. range in
        pixel_height - int_of_float (normalized *. float_of_int pixel_height)
    in

    (* Determine color *)
    let r, g, b =
      match color with Some c -> ansi_to_rgb c | None -> (133, 153, 0)
      (* default green *)
    in

    (* Draw filled area under curve with transparency *)
    let _ = set_render_draw_blend_mode renderer Blend.mode_blend in
    let _ = set_render_draw_color renderer r g b 64 in

    let num_values = List.length values in
    let x_step = float_of_int pixel_width /. float_of_int (max num_values 1) in

    List.iteri
      (fun i value ->
        let x = info.x + int_of_float (float_of_int i *. x_step) in
        let y = info.y + value_to_y value in
        let bottom = info.y + pixel_height in
        let height = max 1 (bottom - y) in
        let rect =
          Rect.create ~x ~y ~w:(max 2 (int_of_float x_step)) ~h:height
        in
        ignore (render_fill_rect renderer (Some rect)))
      values ;

    (* Draw smooth line on top *)
    let _ = set_render_draw_color renderer r g b 255 in

    let points = Array.of_list values in
    for i = 0 to Array.length points - 2 do
      let x1 = info.x + int_of_float (float_of_int i *. x_step) in
      let y1 = info.y + value_to_y points.(i) in
      let x2 = info.x + int_of_float (float_of_int (i + 1) *. x_step) in
      let y2 = info.y + value_to_y points.(i + 1) in
      ignore (render_draw_line renderer x1 y1 x2 y2)
    done ;

    (* Draw threshold lines if any *)
    List.iter
      (fun (thresh : Sparkline_widget.threshold) ->
        if thresh.value >= min_val && thresh.value <= max_val then
          let tr, tg, tb = ansi_to_rgb thresh.color in
          let _ = set_render_draw_color renderer tr tg tb 128 in
          let y = info.y + value_to_y thresh.value in
          ignore (render_draw_line renderer info.x y (info.x + pixel_width) y))
      thresholds

(* Fallback to text-based rendering - returns the text output *)
let render sparkline ~focus ~show_value ?color ?(thresholds = []) () =
  Sparkline_widget.render sparkline ~focus ~show_value ?color ~thresholds ()

let render_with_label sparkline ~label ~focus ?color ?(thresholds = []) () =
  Sparkline_widget.render_with_label
    sparkline
    ~label
    ~focus
    ?color
    ~thresholds
    ()
