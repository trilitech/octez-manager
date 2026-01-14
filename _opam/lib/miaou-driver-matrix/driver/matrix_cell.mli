(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Terminal cell representation for the Matrix driver.

    A cell represents a single character position on the terminal with
    its associated styling (colors, bold, dim, etc.). *)

(** Style attributes for a cell. *)
type style = {
  fg : int;  (** Foreground color: -1 = default, 0-255 = 256-color palette *)
  bg : int;  (** Background color: -1 = default, 0-255 = 256-color palette *)
  bold : bool;
  dim : bool;
  underline : bool;
  reverse : bool;
}

(** A terminal cell containing a character and its style. *)
type t = {
  mutable char : string;  (** UTF-8 grapheme cluster, " " for empty/space *)
  mutable style : style;
}

(** Default style: no colors, no attributes. *)
val default_style : style

(** Empty cell: space character with default style. *)
val empty : unit -> t

(** Create a new cell with given character and style. *)
val create : char:string -> style:style -> t

(** Create a copy of a cell. *)
val copy : t -> t

(** Reset a cell to empty state (space with default style). *)
val reset : t -> unit

(** Invalidate a cell so it never equals any valid cell.
    Used for force-redraw scenarios where we need to ensure
    the diff outputs changes even for cells that might otherwise
    appear unchanged. Sets char to \x00 which never appears in
    normal content. *)
val invalidate : t -> unit

(** Check if two styles are equal. *)
val style_equal : style -> style -> bool

(** Check if two cells are equal (same char and style). *)
val equal : t -> t -> bool

(** Check if a cell is empty (space with default style). *)
val is_empty : t -> bool
