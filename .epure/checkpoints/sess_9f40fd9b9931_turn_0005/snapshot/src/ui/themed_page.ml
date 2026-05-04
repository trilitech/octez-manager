(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Wrapper functor that adds automatic themed background fill to any page.

    This ensures the page's background is filled with the theme's background
    color, which is essential for light mode to display correctly. *)

(** Pure page layout rendering functions are in [Page_layout], which lives in
    the [octez_manager_ui_views] library (no Eio dependency). This module
    re-exports them as aliases for backward compatibility and adds the
    [Make] functor that wraps pages with themed background and key hints. *)

let apply_themed_background = Page_layout.apply_themed_background

let themed_separator = Page_layout.themed_separator

let render_themed_footer = Page_layout.render_themed_footer

let render_layout = Page_layout.render_layout

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
        (* Register this page's keymap for the help modal *)
        let keymap_pairs =
          List.map
            (fun (kb : key_binding) -> (kb.Miaou.Core.Tui_page.key, kb.help))
            (P.keymap ps)
        in
        Context.register_active_page_keymap (fun () -> keymap_pairs) ;
        (* Render page with themed background and footer *)
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
    Metrics.mark_input_event () ;
    (* Bypass global shortcuts when a modal is active OR when the page itself
       reports a modal-like state (e.g. pager in search/help input mode). *)
    if Miaou.Core.Modal_manager.has_active () || P.has_modal ps then
      P.handle_key ps key ~size
    else
      match Global_shortcuts.handle key with
      | Global_shortcuts.Handled -> ps
      | Global_shortcuts.NotGlobal -> P.handle_key ps key ~size

  let on_key ps key ~size =
    Metrics.mark_input_event () ;
    if Miaou.Core.Modal_manager.has_active () || P.has_modal ps then
      P.on_key ps key ~size
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
