(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Wrapper functor that adds automatic themed background fill to any page.

    This ensures the page's background is filled with the theme's background
    color, which is essential for light mode to display correctly. *)

module Widgets = Miaou_widgets_display.Widgets
module Style = Miaou_style.Style
module Style_context = Miaou_style.Style_context
module Flex = Miaou_widgets_layout.Flex_layout

(** Apply themed background fill to rendered content.
    Pads each line to full width and applies the theme's background color.

    Note: Foreground theming for uncolored text is now handled automatically
    by Miaou's renderer (Widgets.apply_themed_foreground). *)
let apply_themed_background ~size content =
  let bg_style = Style_context.background () in
  let resolved = Style.to_resolved bg_style in
  let cols = size.LTerm_geom.cols in
  let rows = size.LTerm_geom.rows in
  let lines = String.split_on_char '\n' content in
  let line_count = List.length lines in
  (* Pad or truncate to exact row count *)
  let lines =
    if line_count < rows then
      lines @ List.init (rows - line_count) (fun _ -> "")
    else if line_count > rows then List.filteri (fun i _ -> i < rows) lines
    else lines
  in
  (* Apply background fill if theme specifies one *)
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

(** Create a themed horizontal separator line *)
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
    This is a themed replacement for Vsection.render.

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

module Make
    (P : Miaou.Core.Tui_page.PAGE_SIG)
    (Config : sig
      val page_name : string
    end) :
  Miaou.Core.Tui_page.PAGE_SIG with type state = P.state and type msg = P.msg =
struct
  type state = P.state

  type msg = P.msg

  type key_binding = state Miaou.Core.Tui_page.key_binding_desc

  type pstate = P.pstate

  let init = P.init

  let update = P.update

  (* Wrap view with themed background, footer, and metrics tracking *)
  let view ps ~focus ~size =
    Metrics.record_render ~page:Config.page_name (fun () ->
        let cols = size.LTerm_geom.cols in
        let content = P.view ps ~focus ~size in
        (* Get key_hints from underlying page and render themed footer *)
        let hints = P.key_hints ps in
        let footer_pairs =
          List.map
            (fun (h : Miaou.Core.Tui_page.key_hint) -> (h.key, h.help))
            hints
        in
        let footer_lines = render_themed_footer ~cols footer_pairs in
        let footer_rows = List.length footer_lines in
        (* If page has hints, append footer to content *)
        let content_with_footer =
          if footer_rows = 0 then content
          else
            let separator = themed_separator ~cols in
            let footer_block = String.concat "\n" footer_lines in
            content ^ "\n" ^ separator ^ "\n" ^ footer_block
        in
        apply_themed_background ~size content_with_footer)

  let move = P.move

  let refresh = P.refresh

  let service_select = P.service_select

  let service_cycle = P.service_cycle

  let back = P.back

  let handle_modal_key = P.handle_modal_key

  let handle_key ps key ~size =
    if Miaou.Core.Modal_manager.has_active () then P.handle_key ps key ~size
    else
      match Global_shortcuts.handle key with
      | Global_shortcuts.Handled -> ps
      | Global_shortcuts.NotGlobal -> P.handle_key ps key ~size

  let on_key ps key ~size =
    if Miaou.Core.Modal_manager.has_active () then P.on_key ps key ~size
    else
      let key_str = Miaou.Core.Keys.to_string key in
      match Global_shortcuts.handle key_str with
      | Global_shortcuts.Handled -> (ps, Miaou_interfaces.Key_event.Handled)
      | Global_shortcuts.NotGlobal -> P.on_key ps key ~size

  let on_modal_key = P.on_modal_key

  (* Return empty key_hints so Miaou doesn't render its own footer.
     The footer is rendered by our view function above. *)
  let key_hints _ps = []

  let keymap = P.keymap

  let handled_keys = P.handled_keys

  let has_modal = P.has_modal
end
