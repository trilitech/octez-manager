(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Pure page layout rendering utilities for TUI view modules.

    These functions handle the structural rendering of pages (header, footer,
    content area, background fill) without any I/O, Context, or Eio calls.

    All page layout functions that were previously in [Themed_page] delegate
    to this module. View modules in [octez_manager_ui_views] use this module
    directly. *)

module Widgets = Miaou_widgets_display.Widgets
module Style = Miaou_style.Style
module Style_context = Miaou_style.Style_context
module Flex = Miaou_widgets_layout.Flex_layout

(** Apply themed background fill to rendered content.
    Pads each line to full width and applies the theme's background color. *)
let apply_themed_background ~size content =
  let bg_style = Style_context.background () in
  let resolved = Style.to_resolved bg_style in
  let cols = size.LTerm_geom.cols in
  let rows = size.LTerm_geom.rows in
  let lines = String.split_on_char '\n' content in
  let line_count = List.length lines in
  let lines =
    if line_count < rows then
      lines @ List.init (rows - line_count) (fun _ -> "")
    else if line_count > rows then List.filteri (fun i _ -> i < rows) lines
    else lines
  in
  let lines =
    if resolved.Style.r_bg < 0 then lines
    else
      List.map
        (fun line ->
          let padded = Widgets.pad_to_cols_line ~cols line in
          Widgets.apply_bg_fill ~bg:resolved.Style.r_bg padded)
        lines
  in
  String.concat "\n" lines

(** Create a themed horizontal separator line. *)
let themed_separator ~cols = Widgets.themed_border (Widgets.hr ~width:cols ())

(** Render themed footer from key/value pairs.
    Wraps segments across lines to fit within the given column width. *)
let render_themed_footer ~cols (pairs : (string * string) list) : string list =
  if pairs = [] then []
  else
    let segments =
      List.map
        (fun (k, v) ->
          Widgets.themed_secondary (k ^ ": ") ^ Widgets.themed_text v)
        pairs
    in
    let space = "    " in
    let lines = ref [] in
    let current = ref "" in
    let add_line () =
      if !current <> "" then (
        lines := !current :: !lines ;
        current := "")
    in
    List.iter
      (fun seg ->
        if !current = "" then current := seg
        else
          let candidate = !current ^ space ^ seg in
          if Widgets.visible_chars_count candidate > cols then (
            add_line () ;
            current := seg)
          else current := candidate)
      segments ;
    add_line () ;
    let max_lines = 2 in
    let result = List.rev !lines in
    if List.length result <= max_lines then result
    else List.filteri (fun i _ -> i < max_lines) result

(** Standard page layout with header, separator, content, separator, footer.

    @param size The terminal size
    @param header List of header lines (rendered at top)
    @param footer List of footer lines (rendered at bottom)
    @param child Function that renders the main content given available size *)
let render_layout ~size ~(header : string list) ~(footer : string list)
    ~(child : LTerm_geom.size -> string) : string =
  let cols = size.LTerm_geom.cols in
  let separator = themed_separator ~cols in
  let header_block = String.concat "\n" header in
  let footer_block = String.concat "\n" footer in
  let header_rows = List.length header in
  let footer_rows = List.length footer in
  let item ~render ~basis : Flex.child = {render; basis; cross = None} in
  let layout =
    Flex.create
      ~direction:Flex.Column
      ([
         item ~render:(fun ~size:_ -> header_block) ~basis:(Flex.Px header_rows);
         item ~render:(fun ~size:_ -> separator) ~basis:(Flex.Px 1);
         item ~render:(fun ~size -> child size) ~basis:Flex.Fill;
         item ~render:(fun ~size:_ -> separator) ~basis:(Flex.Px 1);
       ]
      @
      if footer_rows > 0 then
        [
          item ~render:(fun ~size:_ -> footer_block) ~basis:(Flex.Px footer_rows);
        ]
      else [])
  in
  let content = Flex.render layout ~size in
  apply_themed_background ~size content
