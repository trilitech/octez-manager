(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Vertical bar chart widget for comparing values across categories.

    Bar charts display values as vertical bars, ideal for comparing
    discrete categories or showing rankings.

    {1 Usage Example}

    {[
      let data = [
        ("Mon", 45.0);
        ("Tue", 67.0);
        ("Wed", 82.0);
        ("Thu", 55.0);
        ("Fri", 90.0);
      ] in
      let chart = Bar_chart_widget.create
        ~width:50
        ~height:12
        ~data
        ~title:"Sales by Day"
        () in
      Bar_chart_widget.render chart ~show_values:true
    ]}

    Output (approximate):
    {v
        Sales by Day
    100 │     ┃
     80 │   ┃ ┃
     60 │ ┃ ┃ ┃
     40 │ ┃ ┃ ┃ ┃ ┃
        └──────────
         Mon Tue Wed
    v}
*)

(** A bar with a label, a value, and an optional color. *)
type bar = string * float * string option

(** A threshold for coloring bars with a value above it. *)
type threshold = {value : float; color : string}

(** Rendering mode for bar charts. *)
type render_mode =
  | ASCII  (** Use block characters (█▀) - standard resolution *)
  | Braille
      (** Use Unicode Braille patterns - higher resolution (2x4 dots per cell) *)

(** The bar chart widget type. *)
type t

(** Create a vertical bar chart.
    - [width] Chart width in characters.
    - [height] Chart height in rows.
    - [data] List of bars to display. Each bar is a tuple of
      [(label, value, optional_color)].
    - [title] Optional chart title.
    - [color] Optional default ANSI color code for bars.
    - [min_value] Optional fixed minimum (default: 0 or data min).
    - [max_value] Optional fixed maximum (default: auto-scale to data max). *)
val create :
  width:int ->
  height:int ->
  data:bar list ->
  ?title:string ->
  ?color:string ->
  ?min_value:float ->
  ?max_value:float ->
  unit ->
  t

(** Render the bar chart.
    - [show_values] If true, display numeric values on or above bars.
    - [thresholds] Optional list of thresholds for coloring bars.
      Bars with a value greater than a threshold's [value] will be
      colored with the threshold's [color]. If multiple thresholds are
      exceeded, the one with the highest value is used. Bar-specific
      colors have precedence over threshold colors, which have precedence
      over the default color.
    - [mode] Rendering mode (default: ASCII). Use [Braille] for higher
      resolution with smoother bars. *)
val render :
  t ->
  show_values:bool ->
  ?thresholds:threshold list ->
  ?mode:render_mode ->
  unit ->
  string

(** Update the chart data. Returns updated chart. *)
val update_data : t -> data:bar list -> t

(** Set the default bar color. Returns updated chart. *)
val set_default_color : t -> color:string -> t
