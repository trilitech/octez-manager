(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Pure page layout rendering utilities for TUI view modules.

    These functions handle the structural rendering of pages (header, footer,
    content area, background fill) without any I/O, Context, or Eio calls. *)

(** Apply themed background fill to rendered content.
    Pads each line to full width and applies the theme's background color. *)
val apply_themed_background : size:LTerm_geom.size -> string -> string

(** Create a themed horizontal separator line. *)
val themed_separator : cols:int -> string

(** Render themed footer from key/value pairs.
    Wraps segments across lines to fit within the given column width. *)
val render_themed_footer : cols:int -> (string * string) list -> string list

(** Standard page layout with header, separator, content, separator, footer.

    @param size The terminal size
    @param header List of header lines (rendered at top)
    @param footer List of footer lines (rendered at bottom)
    @param child Function that renders the main content given available size *)
val render_layout :
  size:LTerm_geom.size ->
  header:string list ->
  footer:string list ->
  child:(LTerm_geom.size -> string) ->
  string
