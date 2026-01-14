(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Shared utilities for chart widgets. *)

(** Find min and max in a list of floats. Returns (min, max).
    Returns (0., 0.) for empty list. *)
val bounds : float list -> float * float

(** Scale a value from data range [min_val, max_val] to display range [0, display_max].
    Returns 0 if range is zero. *)
val scale :
  value:float -> min_val:float -> max_val:float -> display_max:int -> int

(** Format a float for display with smart precision.
    - Large values (>= 1000): no decimal places
    - Medium values (>= 10): 1 decimal place
    - Small values: 2 decimal places *)
val format_value : float -> string

(** Format axis label with units.
    Example: format_label ~value:42.5 ~unit_:"MB" = "42.5MB" *)
val format_label : value:float -> unit_:string -> string

(** Generate evenly-spaced tick positions.
    Example: tick_positions ~count:5 ~max:100 = [0; 25; 50; 75; 100] *)
val tick_positions : count:int -> max:int -> int list

(** Round to nice numbers for axis labels (1, 2, 5, 10, 20, 50, 100, etc.).
    - [round_up]: if true, round up; otherwise round to nearest. *)
val nice_number : float -> round_up:bool -> float

(** Render a braille canvas with ANSI color styling from a 2D style matrix.
    Common pattern used by sparkline, line_chart, and bar_chart widgets.
    
    [styles] is a 2D array where styles.(y).(x) contains an optional color string.
    If a color is present, the braille character at (x, y) will be wrapped in ANSI codes. *)
val render_braille_with_colors :
  Braille_canvas.t -> string option array array -> string
