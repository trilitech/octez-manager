(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(* SDL-enhanced line chart widget with smooth anti-aliased rendering *)

module W = Miaou_widgets_display.Widgets
module Line_chart_widget = Miaou_widgets_display.Line_chart_widget
module Chart_utils = Miaou_widgets_display.Chart_utils

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
  | "30" -> (0, 0, 0)
  | "31" -> (220, 50, 47) (* red *)
  | "32" -> (133, 153, 0) (* green *)
  | "33" -> (181, 137, 0) (* yellow *)
  | "34" -> (38, 139, 210) (* blue *)
  | "35" -> (211, 54, 130) (* magenta *)
  | "36" -> (42, 161, 152) (* cyan *)
  | "37" -> (238, 232, 213) (* white *)
  | "90" -> (88, 110, 117)
  | "91" -> (220, 50, 47)
  | "92" -> (133, 153, 0)
  | "93" -> (181, 137, 0)
  | "94" -> (38, 139, 210)
  | "95" -> (211, 54, 130)
  | "96" -> (42, 161, 152)
  | "97" -> (253, 246, 227)
  | _ -> (133, 153, 0)

(* Calculate bounds from all series *)
let calculate_bounds series_list =
  let all_points =
    List.concat_map (fun (s : Line_chart_widget.series) -> s.points) series_list
  in
  if all_points = [] then (0., 1., 0., 1.)
  else
    let xs = List.map (fun (p : Line_chart_widget.point) -> p.x) all_points in
    let ys = List.map (fun (p : Line_chart_widget.point) -> p.y) all_points in
    let x_min, x_max = Chart_utils.bounds xs in
    let y_min, y_max = Chart_utils.bounds ys in
    (x_min, x_max, y_min, y_max)

(* Render a single series with SDL *)
let render_series_sdl info series x_min x_max y_min y_max ?thresholds () =
  let open Tsdl.Sdl in
  let renderer = info.renderer in

  let pixel_width = info.width * info.char_w in
  let pixel_height = info.height * info.char_h in

  let x_range = x_max -. x_min in
  let y_range = y_max -. y_min in

  (* Map data coordinates to pixel coordinates *)
  let map_x x =
    if x_range = 0. then pixel_width / 2
    else int_of_float ((x -. x_min) /. x_range *. float_of_int pixel_width)
  in

  let map_y y =
    if y_range = 0. then pixel_height / 2
    else
      pixel_height
      - int_of_float ((y -. y_min) /. y_range *. float_of_int pixel_height)
  in

  (* Determine series color *)
  let r, g, b =
    match (series : Line_chart_widget.series).color with
    | Some c -> ansi_to_rgb c
    | None -> (133, 153, 0)
  in

  (* Draw filled area under curve *)
  let _ = set_render_draw_blend_mode renderer Blend.mode_blend in
  let _ = set_render_draw_color renderer r g b 48 in

  let points = (series : Line_chart_widget.series).points in
  List.iteri
    (fun i (point : Line_chart_widget.point) ->
      let px = info.x + map_x point.x in
      let py = info.y + map_y point.y in
      let bottom = info.y + pixel_height in
      let height = max 1 (bottom - py) in
      let rect = Rect.create ~x:px ~y:py ~w:2 ~h:height in
      ignore (render_fill_rect renderer (Some rect)) ;

      (* Draw line to next point *)
      match List.nth_opt points (i + 1) with
      | Some next_point ->
          let next_px = info.x + map_x next_point.x in
          let next_py = info.y + map_y next_point.y in

          (* Determine color (check thresholds) *)
          let pr, pg, pb =
            match point.color with
            | Some c -> ansi_to_rgb c
            | None -> (
                match thresholds with
                | Some thresh_list -> (
                    let sorted =
                      List.sort
                        (fun (a : Line_chart_widget.threshold)
                             (b : Line_chart_widget.threshold)
                           -> Float.compare b.value a.value)
                        thresh_list
                    in
                    match
                      List.find_opt
                        (fun (t : Line_chart_widget.threshold) ->
                          point.y > t.value)
                        sorted
                    with
                    | Some t -> ansi_to_rgb t.color
                    | None -> (r, g, b))
                | None -> (r, g, b))
          in

          (* Draw filled polygon for smooth gradient *)
          let _ = set_render_draw_color renderer pr pg pb 48 in
          let fill_points =
            [(px, py); (next_px, next_py); (next_px, bottom); (px, bottom)]
          in
          List.iter
            (fun (x1, y1) -> ignore (render_draw_point renderer x1 y1))
            fill_points ;

          (* Draw line *)
          let _ = set_render_draw_color renderer pr pg pb 255 in
          ignore (render_draw_line renderer px py next_px next_py)
      | None -> ())
    points ;

  (* Draw data points as small circles (approximated with filled rects) *)
  let _ = set_render_draw_color renderer r g b 255 in
  List.iter
    (fun (point : Line_chart_widget.point) ->
      let px = info.x + map_x point.x in
      let py = info.y + map_y point.y in
      let point_size = 3 in
      let rect =
        Rect.create
          ~x:(px - (point_size / 2))
          ~y:(py - (point_size / 2))
          ~w:point_size
          ~h:point_size
      in
      ignore (render_fill_rect renderer (Some rect)))
    points

(* Render grid lines *)
let render_grid_sdl info _x_min _x_max _y_min _y_max =
  let open Tsdl.Sdl in
  let renderer = info.renderer in
  let _ = set_render_draw_blend_mode renderer Blend.mode_blend in
  let _ = set_render_draw_color renderer 80 80 80 64 in

  let pixel_width = info.width * info.char_w in
  let pixel_height = info.height * info.char_h in

  (* Vertical grid lines *)
  for i = 1 to 9 do
    let x = info.x + (pixel_width * i / 10) in
    ignore (render_draw_line renderer x info.y x (info.y + pixel_height))
  done ;

  (* Horizontal grid lines *)
  for i = 1 to 9 do
    let y = info.y + (pixel_height * i / 10) in
    ignore (render_draw_line renderer info.x y (info.x + pixel_width) y)
  done

(* Render axes *)
let render_axes_sdl info =
  let open Tsdl.Sdl in
  let renderer = info.renderer in
  let _ = set_render_draw_color renderer 180 180 180 255 in

  let pixel_width = info.width * info.char_w in
  let pixel_height = info.height * info.char_h in

  (* X axis (bottom) *)
  ignore
    (render_draw_line
       renderer
       info.x
       (info.y + pixel_height)
       (info.x + pixel_width)
       (info.y + pixel_height)) ;

  (* Y axis (left) *)
  ignore
    (render_draw_line renderer info.x info.y info.x (info.y + pixel_height))

(* Main SDL render function *)
let render_sdl info chart ~show_axes ~show_grid ?thresholds () =
  let x_min, x_max, y_min, y_max =
    calculate_bounds (Line_chart_widget.get_series chart)
  in

  (* Render grid first (background) *)
  if show_grid then render_grid_sdl info x_min x_max y_min y_max ;

  (* Render axes *)
  if show_axes then render_axes_sdl info ;

  (* Render each series *)
  List.iter
    (fun series ->
      render_series_sdl info series x_min x_max y_min y_max ?thresholds ())
    (Line_chart_widget.get_series chart)

(* Fallback to text-based rendering *)
let render chart ~show_axes ~show_grid ?thresholds () =
  Line_chart_widget.render chart ~show_axes ~show_grid ?thresholds ()
