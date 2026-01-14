(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

[@@@coverage off]

module Logger_capability = Miaou_interfaces.Logger_capability
open Miaou_core.Tui_page
module Capture = Miaou_core.Tui_capture
module Khs = Miaou_internals.Key_handler_stack
module Modal_manager = Miaou_core.Modal_manager
module Narrow_modal = Miaou_core.Narrow_modal
module Fibers = Miaou_helpers.Fiber_runtime
module Helpers = Miaou_helpers.Helpers

let narrow_warned = ref false

let clear_and_render (type page_state)
    (module Page : PAGE_SIG with type state = page_state) ~detect_size
    ~last_out_ref ~last_size st key_stack =
  (match Logger_capability.get () with
  | Some logger when Sys.getenv_opt "MIAOU_DEBUG" = Some "1" ->
      logger.logf Debug "DRIVER: clear_and_render tick"
  | _ -> ()) ;
  let size = detect_size () in
  let header_lines =
    if size.LTerm_geom.cols < 80 then
      [
        Miaou_widgets_display.Widgets.warning_banner
          ~cols:size.cols
          (Printf.sprintf
             "Narrow terminal: %d cols (< 80). Some UI may be truncated."
             size.cols);
      ]
    else []
  in
  let prev_cols = !last_size.LTerm_geom.cols in
  if
    (size.cols < 80 && not !narrow_warned)
    || (size.cols < 80 && prev_cols >= 80 && not !narrow_warned)
  then (
    (match Logger_capability.get () with
    | Some logger ->
        logger.logf
          Warning
          (Printf.sprintf
             "WIDTH_CROSSING: prev=%d new=%d (showing narrow modal)"
             prev_cols
             size.cols)
    | None -> ()) ;
    narrow_warned := true ;
    Modal_manager.push
      (module Narrow_modal.Page)
      ~init:(Narrow_modal.Page.init ())
      ~ui:
        {
          title = "Narrow terminal";
          left = Some 2;
          max_width = None;
          dim_background = true;
        }
      ~commit_on:[]
      ~cancel_on:[]
      ~on_close:(fun (_ : Narrow_modal.Page.state Miaou_core.Navigation.t) _ ->
        ()) ;
    Modal_manager.set_consume_next_key () ;
    let my_title = "Narrow terminal" in
    Fibers.spawn (fun env ->
        Eio.Time.sleep env#clock 5.0 ;
        match Modal_manager.top_title_opt () with
        | Some t when t = my_title -> Modal_manager.close_top `Cancel
        | _ -> ())) ;
  if
    size.LTerm_geom.rows <> !last_size.LTerm_geom.rows
    || size.LTerm_geom.cols <> !last_size.LTerm_geom.cols
  then last_out_ref := "" ;
  Modal_manager.set_current_size size.LTerm_geom.rows size.LTerm_geom.cols ;
  let body = Page.view st ~focus:true ~size in
  let title_opt =
    match String.index_opt body '\n' with
    | None -> None
    | Some idx -> Some (String.sub body 0 idx)
  in
  let main_out =
    match title_opt with
    | Some t when String.length t > 0 ->
        let wrapped_footer =
          Miaou_widgets_display.Widgets.footer_hints_wrapped_capped
            ~cols:size.cols
            ~max_lines:3
            (Khs.top_bindings key_stack)
        in
        Miaou_widgets_display.Widgets.render_frame
          ~title:t
          ~header:header_lines
          ~cols:size.cols
          ~body:
            (String.sub
               body
               (min (String.length body) (String.length t + 1))
               (max 0 (String.length body - (String.length t + 1))))
          ~footer:wrapped_footer
          ()
    | _ ->
        let wrapped_footer =
          Miaou_widgets_display.Widgets.footer_hints_wrapped_capped
            ~cols:size.cols
            ~max_lines:3
            (Khs.top_bindings key_stack)
        in
        let buf =
          Buffer.create (String.length body + String.length wrapped_footer + 64)
        in
        (match header_lines with
        | [] -> ()
        | lst ->
            Buffer.add_string buf (Helpers.concat_lines lst) ;
            Buffer.add_char buf '\n') ;
        Buffer.add_string buf body ;
        Buffer.add_char buf '\n' ;
        Buffer.add_string buf wrapped_footer ;
        Buffer.contents buf
  in
  let out =
    match
      Miaou_internals.Modal_renderer.render_overlay
        ~cols:(Some size.cols)
        ~base:main_out
        ~rows:size.rows
        ()
    with
    | Some s -> s
    | None -> main_out
  in
  let max_rows = size.LTerm_geom.rows in
  let lines = String.split_on_char '\n' out in
  let out_trimmed =
    if List.length lines <= max_rows then out
    else
      let rec take n lst =
        if n <= 0 then []
        else match lst with [] -> [] | x :: xs -> x :: take (n - 1) xs
      in
      Helpers.concat_lines (take max_rows lines)
  in
  Capture.record_frame
    ~rows:size.LTerm_geom.rows
    ~cols:size.LTerm_geom.cols
    out_trimmed ;
  if out_trimmed <> !last_out_ref then (
    print_string ("\027[2J\027[H" ^ out_trimmed) ;
    Stdlib.flush stdout ;
    last_out_ref := out_trimmed)
  else () ;
  last_size := size
