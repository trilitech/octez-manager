(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** ANSI escape sequence parser for the Matrix driver.

    Parses ANSI-formatted strings (as produced by MIAOU widgets) into
    cell buffers. Handles:
    - 256-color foreground/background (ESC[38;5;Nm, ESC[48;5;Nm)
    - Basic colors (ESC[30-37m, ESC[40-47m)
    - Bold, dim, underline, reverse
    - Reset (ESC[0m)
    - UTF-8 characters including multi-byte

    State Machine:
    - Normal: reading visible characters
    - EscapeStart: saw ESC (\027)
    - CSI: saw ESC[, accumulating parameters until 'm'
*)

(** Parser state, maintains current style across parse calls. *)
type t

(** Create a new parser with default style. *)
val create : unit -> t

(** Reset parser to initial state (default style). *)
val reset : t -> unit

(** Get the current style being applied by the parser. *)
val current_style : t -> Matrix_cell.style

(** Parse an ANSI string into the buffer starting at (row, col).
    Returns the (row, col) after the last written character.
    Handles newlines by advancing row and resetting col to 0. *)
val parse_into :
  t -> Matrix_buffer.t -> row:int -> col:int -> string -> int * int

(** Parse a single line (no newline handling) into buffer.
    Returns the column after last character. *)
val parse_line : t -> Matrix_buffer.t -> row:int -> col:int -> string -> int

(** Parse into buffer using batch_ops for thread-safe access.
    Use within [Matrix_buffer.with_back_buffer]. *)
val parse_into_batch :
  t -> Matrix_buffer.batch_ops -> row:int -> col:int -> string -> int * int

(** Parse string and return list of (char, style) pairs for inspection.
    Useful for testing. Does not write to buffer. *)
val parse_to_cells : t -> string -> (string * Matrix_cell.style) list

(** Count visible characters in an ANSI string (excluding escape sequences). *)
val visible_length : string -> int
