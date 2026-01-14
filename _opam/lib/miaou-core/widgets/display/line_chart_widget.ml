(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module W = Widgets

type point = {x : float; y : float; color : string option}

type series = {label : string; points : point list; color : string option}

type threshold = {value : float; color : string}

type render_mode = ASCII | Braille

type axis_config = {
  show_labels : bool;
  x_label : string;
  y_label : string;
  x_ticks : int;
  y_ticks : int;
}

let default_axis_config =
  {show_labels = false; x_label = ""; y_label = ""; x_ticks = 5; y_ticks = 5}

type t = {
  width : int;
  height : int;
  series : series list;
  title : string option;
  axis_config : axis_config;
}

let create ~width ~height ~series ?title ?(axis_config = default_axis_config) ()
    =
  {width; height; series; title; axis_config}

let update_series t ~label ~points =
  let series =
    List.map (fun s -> if s.label = label then {s with points} else s) t.series
  in
  {t with series}

let add_point t ~label ~point =
  let series =
    List.map
      (fun s ->
        if s.label = label then {s with points = point :: s.points} else s)
      t.series
  in
  (* Reverse points list when rendering if order matters, or keep as-is for perf *)
  {t with series}

let set_axis_config t axis_config = {t with axis_config}

(* Grid canvas: mutable character grid *)
type cell = {mutable char : string; mutable style : string option}

let make_grid width height =
  Array.init height (fun _ ->
      Array.init width (fun _ -> {char = " "; style = None}))

let set_cell grid x y char =
  if y >= 0 && y < Array.length grid && x >= 0 && x < Array.length grid.(0) then
    grid.(y).(x).char <- char

let set_cell_styled grid x y char style =
  if y >= 0 && y < Array.length grid && x >= 0 && x < Array.length grid.(0) then (
    grid.(y).(x).char <- char ;
    grid.(y).(x).style <- Some style)

(* Coordinate mapping *)
let map_x x x_min x_max width =
  let range = x_max -. x_min in
  if range = 0. then width / 2
  else int_of_float ((x -. x_min) /. range *. float_of_int (width - 1))

let map_y y y_min y_max height =
  let range = y_max -. y_min in
  if range = 0. then height / 2
  else
    (* Invert Y because terminal coordinates go top-down *)
    height - 1
    - int_of_float ((y -. y_min) /. range *. float_of_int (height - 1))

(* Simplified line drawing *)
let draw_line grid x1 y1 x2 y2 char style =
  let dx = abs (x2 - x1) in
  let dy = abs (y2 - y1) in
  let sx = if x1 < x2 then 1 else -1 in
  let sy = if y1 < y2 then 1 else -1 in
  let rec loop x y err =
    (match style with
    | Some s -> set_cell_styled grid x y char s
    | None -> set_cell grid x y char) ;
    if x = x2 && y = y2 then ()
    else
      let e2 = 2 * err in
      let x', err' = if e2 > -dy then (x + sx, err - dy) else (x, err) in
      let y', err'' = if e2 < dx then (y + sy, err' + dx) else (y, err') in
      loop x' y' err''
  in
  loop x1 y1 (dx - dy)

(* Render axes *)
let render_axes grid _t _x_min _x_max _y_min _y_max =
  let width = Array.length grid.(0) in
  let height = Array.length grid in

  (* Y-axis (left edge) *)
  for y = 0 to height - 1 do
    set_cell grid 0 y (if W.prefer_ascii () then "|" else "│")
  done ;
  set_cell grid 0 (height - 1) (if W.prefer_ascii () then "+" else "└") ;

  (* X-axis (bottom edge) *)
  for x = 0 to width - 1 do
    set_cell grid x (height - 1) (if W.prefer_ascii () then "-" else "─")
  done ;
  set_cell grid 0 (height - 1) (if W.prefer_ascii () then "+" else "└")

(* Render grid lines *)
let render_grid_lines grid t =
  let width = Array.length grid.(0) in
  let height = Array.length grid in
  let x_step = max 1 (width / (t.axis_config.x_ticks + 1)) in
  let y_step = max 1 (height / (t.axis_config.y_ticks + 1)) in

  (* Vertical grid lines *)
  for i = 1 to t.axis_config.x_ticks do
    let x = min (width - 1) (i * x_step) in
    for y = 0 to height - 2 do
      if grid.(y).(x).char = " " then
        set_cell grid x y (if W.prefer_ascii () then ":" else "┊")
    done
  done ;

  (* Horizontal grid lines *)
  for i = 1 to t.axis_config.y_ticks do
    let y = min (height - 2) (i * y_step) in
    for x = 1 to width - 1 do
      if grid.(y).(x).char = " " then
        set_cell grid x y (if W.prefer_ascii () then "." else "┈")
    done
  done

let get_color ~thresholds ~series_color (point : point) : string option =
  let a = List.sort (fun a b -> Float.compare b.value a.value) thresholds in
  match point.color with
  | Some _ -> point.color
  | None -> (
      match List.find_opt (fun t -> point.y > t.value) a with
      | Some t -> Some t.color
      | None -> series_color)

(* Plot a single series *)
let plot_series grid (series : series) x_min x_max y_min y_max width height
    symbol ~thresholds =
  List.iteri
    (fun idx (point : point) ->
      let x = map_x point.x x_min x_max width in
      let y = map_y point.y y_min y_max height in
      let color = get_color ~thresholds ~series_color:series.color point in
      (match color with
      | Some c -> set_cell_styled grid x y symbol c
      | None -> set_cell grid x y symbol) ;
      (* Draw line to next point *)
      match List.nth_opt series.points (idx + 1) with
      | Some next_point ->
          let next_x = map_x next_point.x x_min x_max width in
          let next_y = map_y next_point.y y_min y_max height in
          let line_char = if W.prefer_ascii () then "-" else "●" in
          draw_line grid x y next_x next_y line_char color
      | None -> ())
    series.points

(* Calculate data bounds *)
let calculate_bounds series_list =
  let all_points = List.concat_map (fun s -> s.points) series_list in
  if all_points = [] then (0., 0., 0., 0.)
  else
    let xs = List.map (fun p -> p.x) all_points in
    let ys = List.map (fun p -> p.y) all_points in
    let x_min, x_max = Chart_utils.bounds xs in
    let y_min, y_max = Chart_utils.bounds ys in
    (x_min, x_max, y_min, y_max)

let render t ~show_axes ~show_grid ?(thresholds = []) ?(mode = ASCII) () =
  match mode with
  | ASCII -> (
      let grid = make_grid t.width t.height in
      let x_min, x_max, y_min, y_max = calculate_bounds t.series in

      (* Render grid lines first (background) *)
      if show_grid then render_grid_lines grid t ;

      (* Render axes *)
      if show_axes then render_axes grid t x_min x_max y_min y_max ;

      (* Plot symbols for different series *)
      let symbols = [|"●"; "■"; "▲"; "◆"; "★"|] in

      (* Plot each series *)
      List.iteri
        (fun idx series ->
          let symbol = symbols.(idx mod Array.length symbols) in
          plot_series
            grid
            series
            x_min
            x_max
            y_min
            y_max
            t.width
            t.height
            symbol
            ~thresholds)
        t.series ;

      (* Convert grid to string *)
      let lines =
        Array.to_list grid
        |> List.map (fun row ->
            let buf = Buffer.create (t.width * 10) in
            Array.iter
              (fun cell ->
                match cell.style with
                | Some style -> Buffer.add_string buf (W.ansi style cell.char)
                | None -> Buffer.add_string buf cell.char)
              row ;
            Buffer.contents buf)
      in

      (* Add title if present *)
      match t.title with
      | Some title ->
          let buf =
            Buffer.create
              (String.length title + (List.length lines * t.width) + 1)
          in
          Buffer.add_string buf (W.bold title) ;
          Buffer.add_char buf '\n' ;
          List.iteri
            (fun i line ->
              if i > 0 then Buffer.add_char buf '\n' ;
              Buffer.add_string buf line)
            lines ;
          Buffer.contents buf
      | None ->
          let buf = Buffer.create (List.length lines * (t.width + 1)) in
          List.iteri
            (fun i line ->
              if i > 0 then Buffer.add_char buf '\n' ;
              Buffer.add_string buf line)
            lines ;
          Buffer.contents buf)
  | Braille -> (
      (* Use braille canvas for higher resolution *)
      let canvas = Braille_canvas.create ~width:t.width ~height:t.height in
      let width_cells, height_cells = Braille_canvas.get_dimensions canvas in
      let has_colors =
        thresholds <> []
        || List.exists
             (fun (s : series) ->
               s.color <> None
               || List.exists (fun (p : point) -> p.color <> None) s.points)
             t.series
      in
      let styles =
        if has_colors then
          Some (Array.make_matrix height_cells width_cells None)
        else None
      in
      let dot_width, dot_height = Braille_canvas.get_dot_dimensions canvas in
      let x_min, x_max, y_min, y_max = calculate_bounds t.series in
      let x_range = x_max -. x_min in
      let y_range = y_max -. y_min in
      let inv_x = if x_range = 0. then 0. else 1. /. x_range
      and inv_y = if y_range = 0. then 0. else 1. /. y_range in

      (* Map coordinate to dot position *)
      let map_x x =
        if x_range = 0. then dot_width / 2
        else int_of_float ((x -. x_min) *. inv_x *. float_of_int (dot_width - 1))
      in

      let map_y y =
        if y_range = 0. then dot_height / 2
        else
          (* Invert Y because terminal coordinates go top-down *)
          dot_height - 1
          - int_of_float ((y -. y_min) *. inv_y *. float_of_int (dot_height - 1))
      in

      (* Plot each series *)
      let set_styled_dot x y color =
        (match styles with
        | Some s ->
            let cell_x = x / 2 in
            let cell_y = y / 4 in
            if cell_y < height_cells && cell_x < width_cells then
              s.(cell_y).(cell_x) <- color
        | None -> ()) ;
        Braille_canvas.set_dot canvas ~x ~y
      in

      let draw_line_styled ~x0 ~y0 ~x1 ~y1 color =
        let dx = abs (x1 - x0) in
        let dy = abs (y1 - y0) in
        let sx = if x0 < x1 then 1 else -1 in
        let sy = if y0 < y1 then 1 else -1 in
        let rec loop x y err =
          set_styled_dot x y color ;
          if x = x1 && y = y1 then ()
          else
            let e2 = 2 * err in
            let x', err' = if e2 > -dy then (x + sx, err - dy) else (x, err) in
            let y', err'' =
              if e2 < dx then (y + sy, err' + dx) else (y, err')
            in
            loop x' y' err''
        in
        loop x0 y0 (dx - dy)
      in

      List.iter
        (fun series ->
          let sorted_thresholds =
            if has_colors then
              List.sort (fun a b -> Float.compare b.value a.value) thresholds
            else []
          in
          let get_color_cached (point : point) : string option =
            match point.color with
            | Some c -> Some c
            | None -> (
                match
                  List.find_opt (fun t -> point.y > t.value) sorted_thresholds
                with
                | Some t -> Some t.color
                | None -> (series : series).color)
          in
          List.iteri
            (fun idx point ->
              let x = map_x point.x in
              let y = map_y point.y in
              let color = if has_colors then get_color_cached point else None in
              set_styled_dot x y color ;
              (* Draw line to next point *)
              match List.nth_opt series.points (idx + 1) with
              | Some next_point ->
                  let next_x = map_x next_point.x in
                  let next_y = map_y next_point.y in
                  draw_line_styled ~x0:x ~y0:y ~x1:next_x ~y1:next_y color
              | None -> ())
            series.points)
        t.series ;

      let chart_output =
        match styles with
        | Some s -> Chart_utils.render_braille_with_colors canvas s
        | None -> Braille_canvas.render canvas
      in

      (* Add title if present *)
      match t.title with
      | Some title -> W.bold title ^ "\n" ^ chart_output
      | None -> chart_output)

(* Accessor functions for SDL rendering *)
let get_series t = t.series

let get_title t = t.title

let get_dimensions t = (t.width, t.height)
