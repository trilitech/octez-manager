(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Braille canvas for high-resolution terminal graphics.

    This module provides a canvas abstraction using Unicode Braille patterns
    (U+2800–U+28FF) where each terminal character cell contains a 2×4 grid of
    dots, effectively quadrupling the vertical resolution compared to
    character-based rendering.

    {1 Braille Pattern Layout}

    Each braille character encodes 8 dots arranged as:
    {v
      1 4
      2 5
      3 6
      7 8
    v}

    Dot positions are numbered 1-8 and mapped to Unicode offsets:
    - Dot 1: +0x01, Dot 2: +0x02, Dot 3: +0x04, Dot 4: +0x08
    - Dot 5: +0x10, Dot 6: +0x20, Dot 7: +0x40, Dot 8: +0x80

    {1 Usage Example}

    {[
      let canvas = Braille_canvas.create ~width:10 ~height:8 in
      Braille_canvas.set_dot canvas ~x:5 ~y:3 ;
      Braille_canvas.set_dot canvas ~x:6 ~y:4 ;
      let output = Braille_canvas.render canvas in
      print_endline output
    ]}
*)

(** The braille canvas type. *)
type t

(** Create a new braille canvas.
    - [width] Width in terminal cells (each cell = 2 dots wide).
    - [height] Height in terminal cells (each cell = 4 dots tall).
    The actual dot resolution is [width * 2] × [height * 4]. *)
val create : width:int -> height:int -> t

(** Set a single dot at the given coordinates.
    - [x] Horizontal dot position (0 to width*2-1).
    - [y] Vertical dot position (0 to height*4-1).
    Out-of-bounds coordinates are silently ignored. *)
val set_dot : t -> x:int -> y:int -> unit

(** Clear a single dot at the given coordinates. *)
val clear_dot : t -> x:int -> y:int -> unit

(** Check if a dot is set at the given coordinates.
    Returns [false] for out-of-bounds coordinates. *)
val get_dot : t -> x:int -> y:int -> bool

(** Clear all dots in the canvas. *)
val clear : t -> unit

(** Draw a line between two points using Bresenham's algorithm. *)
val draw_line : t -> x0:int -> y0:int -> x1:int -> y1:int -> unit

(** Render the canvas to a string with newlines separating rows.
    Each cell is rendered as a braille character (U+2800–U+28FF). *)
val render : t -> string

(** Render the canvas with a per-cell transformation, allowing styling.
    The callback [f] receives the cell coordinates (in cells, not dots) and
    the rendered braille character, and must return the string to emit. *)
val render_with : t -> f:(x:int -> y:int -> string -> string) -> string

(** Add a bitmask directly to a cell (OR assignment). Intended for optimized
    rendering paths that precompute braille patterns. *)
val add_cell_bits : t -> cell_x:int -> cell_y:int -> int -> unit

(** Get canvas dimensions in cells (not dots). *)
val get_dimensions : t -> int * int

(** Get canvas dimensions in dots (actual resolution). *)
val get_dot_dimensions : t -> int * int
