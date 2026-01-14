(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** ANSI escape sequence writer for the Matrix driver.

    Converts diff changes into optimized ANSI escape sequences.
    Tracks current style state to minimize redundant SGR emissions.
*)

(** Writer state tracking current style. *)
type t

(** Create a new writer with default style. *)
val create : unit -> t

(** Reset writer to initial state. *)
val reset : t -> unit

(** Render a list of changes to an ANSI string. *)
val render : t -> Matrix_diff.change list -> string

(** {2 ANSI Control Sequences} *)

(** Hide cursor: ESC[?25l *)
val cursor_hide : string

(** Show cursor: ESC[?25h *)
val cursor_show : string

(** Move cursor to home position (0,0): ESC[H *)
val cursor_home : string

(** Move cursor to position (row, col), 1-indexed for ANSI: ESC[row;colH *)
val cursor_move : row:int -> col:int -> string

(** Reset all attributes: ESC[0m *)
val reset_style : string

(** Generate SGR sequence for a style. *)
val style_to_sgr : Matrix_cell.style -> string
