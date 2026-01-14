(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module Sdl = Tsdl.Sdl
module Capture = Miaou_core.Tui_capture
module Colors = Sdl_colors

let draw_background renderer cfg _char_w _char_h =
  match cfg.Sdl_config.gradient with
  | false ->
      let bg = cfg.bg in
      let _ = Sdl.set_render_draw_color renderer bg.r bg.g bg.b 255 in
      let _ = Sdl.render_clear renderer in
      ()
  | true ->
      let w_res, h_res =
        match Sdl.get_renderer_output_size renderer with
        | Error _ -> (800, 600)
        | Ok (w, h) -> (w, h)
      in
      let grad_start = cfg.bg in
      let grad_end =
        {
          Colors.r = max 0 (grad_start.r - 40);
          g = max 0 (grad_start.g - 40);
          b = max 0 (grad_start.b - 40);
          a = grad_start.a;
        }
      in
      for i = 0 to h_res - 1 do
        let frac = float i /. float h_res in
        let r =
          grad_start.r + int_of_float (frac *. float (grad_end.r - grad_start.r))
        in
        let g =
          grad_start.g + int_of_float (frac *. float (grad_end.g - grad_start.g))
        in
        let b =
          grad_start.b + int_of_float (frac *. float (grad_end.b - grad_start.b))
        in
        let _ = Sdl.set_render_draw_color renderer r g b 255 in
        let _ =
          Sdl.render_draw_line renderer 0 i (w_res - 1) i |> Result.get_ok
        in
        ()
      done

let render_to_sdl renderer font cfg char_w char_h size text =
  (* Draw background FIRST, before any SDL chart rendering *)
  draw_background renderer cfg char_w char_h ;

  (* Text has been rendered with SDL context already set up by caller *)
  let default_state : Colors.ansi_state =
    {fg = cfg.Sdl_config.fg; bg = cfg.bg}
  in
  let clean_text = Colors.strip_ansi_to_text ~default:default_state text in
  Capture.record_frame ~rows:size.LTerm_geom.rows ~cols:size.cols clean_text ;

  (* Render text (without clearing background) *)
  Sdl_text_render.render_lines
    renderer
    font
    ~fg:cfg.fg
    ~bg:cfg.bg
    ~char_w
    ~char_h
    ~clear:false (* Already cleared by draw_background above *)
    ~present:false
    (String.split_on_char '\n' text)

let perform_transition renderer font cfg char_w char_h ~from_lines ~to_lines
    ~size =
  let trans_cfg =
    {
      Sdl_transitions.fg = cfg.Sdl_config.fg;
      bg = cfg.bg;
      gradient = cfg.gradient;
      scale = cfg.scale;
      transition = cfg.transition;
    }
  in
  let draw_bg () = draw_background renderer cfg char_w char_h in
  let render_lines_helper ?clear ?offset ?present lines =
    Sdl_text_render.render_lines
      renderer
      font
      ~fg:cfg.fg
      ~bg:cfg.bg
      ~char_w
      ~char_h
      ?clear
      ?offset
      ?present
      lines
  in
  Sdl_transitions.perform
    renderer
    font
    trans_cfg
    char_w
    char_h
    ~from_lines
    ~to_lines
    ~size
    ~draw_background:draw_bg
    ~render_lines:render_lines_helper
