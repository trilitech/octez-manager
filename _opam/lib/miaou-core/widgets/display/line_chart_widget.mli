(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Line chart widget for multi-line time-series visualization with axes and grid.

    Line charts display one or more data series on a coordinate plane with
    labeled axes and optional grid lines.

    {1 Usage Example}

    {[
      let points = [
        { x = 0.; y = 10. };
        { x = 1.; y = 15. };
        { x = 2.; y = 12. };
        { x = 3.; y = 18. };
      ] in
      let chart = Line_chart_widget.create
        ~width:60
        ~height:15
        ~series:[{ label = "Sales"; points; color = None }]
        ~title:"Monthly Sales"
        () in
      Line_chart_widget.render chart ~show_axes:true ~show_grid:false
    ]}

    Output (approximate):
    {v
    Monthly Sales
    18 │       ●
    15 │   ●──╯
    12 │ ●─╯
    10 │─╯
     0 └─────────
       0   1   2   3
    v}
*)

(** A 2D data point with an optional override color. *)
type point = {x : float; y : float; color : string option}

(** A data series with a label, points, and optional default color for its points(ANSI color code). *)
type series = {label : string; points : point list; color : string option}

(** A threshold for coloring points above a certain value. *)
type threshold = {value : float; color : string}

(** Rendering mode for line charts. *)
type render_mode =
  | ASCII  (** Use standard Unicode symbols (●■▲◆★) - one character per cell *)
  | Braille
      (** Use Unicode Braille patterns - higher resolution (2x4 dots per cell) *)

(** Axis configuration for labels and tick marks. *)
type axis_config = {
  show_labels : bool;
  x_label : string;
  y_label : string;
  x_ticks : int;
  y_ticks : int;
}

(** The line chart widget type. *)
type t

(** Create a line chart.
    - [width] Chart width in characters.
    - [height] Chart height in rows.
    - [series] List of data series to plot.
    - [title] Optional chart title.
    - [axis_config] Optional axis configuration (default: no labels, 5 ticks each). *)
val create :
  width:int ->
  height:int ->
  series:series list ->
  ?title:string ->
  ?axis_config:axis_config ->
  unit ->
  t

(** Render the chart.
    - [show_axes] Whether to draw axes with ticks and labels.
    - [show_grid] Whether to draw background grid lines.
    - [thresholds] Optional list of thresholds for coloring points.
      Points with a [y] value greater than a threshold's [value] will be
      colored with the threshold's [color]. If multiple thresholds are
      exceeded, the one with the highest value is used. Point-specific
      colors and series colors have precedence.
    - [mode] Rendering mode (default: ASCII). Use [Braille] for higher
      resolution with smoother lines. *)
val render :
  t ->
  show_axes:bool ->
  show_grid:bool ->
  ?thresholds:threshold list ->
  ?mode:render_mode ->
  unit ->
  string

(** Update a series by label. Returns updated chart. *)
val update_series : t -> label:string -> points:point list -> t

(** Add a single point to a series. Returns updated chart. *)
val add_point : t -> label:string -> point:point -> t

(** Configure axes. Returns updated chart. *)
val set_axis_config : t -> axis_config -> t

(** Get all series from the chart. *)
val get_series : t -> series list

(** Get the chart title. *)
val get_title : t -> string option

(** Get chart dimensions: (width, height). *)
val get_dimensions : t -> int * int
