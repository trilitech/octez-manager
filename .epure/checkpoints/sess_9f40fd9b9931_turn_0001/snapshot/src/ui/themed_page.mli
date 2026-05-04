(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Wrapper functor that adds automatic themed background fill to any page.

    This ensures the page's background is filled with the theme's background
    color, which is essential for light mode to display correctly.

    This functor also includes global shortcut handling and metrics tracking,
    so it replaces both [Monitored_page] functionality.

    Usage:
    {[
      module Page = struct
        (* ... normal page implementation ... *)
      end

      module Themed = Themed_page.Make(Page)(struct
        let page_name = "my-page"
      end)

      let page () = (module Themed : Miaou.Core.Tui_page.PAGE_SIG)
    ]} *)

(** Apply themed background fill to rendered content.
    Pads each line to full width and applies the theme's background color.
    Can be used directly for custom page implementations. *)
val apply_themed_background : size:LTerm_geom.size -> string -> string

(** Create a themed horizontal separator line *)
val themed_separator : cols:int -> string

(** Render themed footer from key/value pairs.
    Wraps segments across lines to fit within the given column width.
    Returns a list of lines (max 2) ready for use with [render_layout]. *)
val render_themed_footer : cols:int -> (string * string) list -> string list

(** Standard page layout with header, separator, content, separator, footer.
    This is a themed replacement for [Vsection.render].

    The layout structure is:
    - Header lines (fixed height)
    - Separator (1 line, themed)
    - Content area (fills remaining space)
    - Separator (1 line, themed)
    - Footer lines (fixed height, optional)

    The entire output has the themed background applied.

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

module Make : functor
  (P : Miaou.Core.Tui_page.PAGE_SIG)
  (_ : sig
     val page_name : string
   end)
  ->
  Miaou.Core.Tui_page.PAGE_SIG with type state = P.state and type msg = P.msg
