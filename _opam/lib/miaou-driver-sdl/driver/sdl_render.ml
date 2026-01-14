(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module Sdl = Tsdl.Sdl
module Ttf = Tsdl_ttf.Ttf

let sdl_fail prefix msg = failwith (Printf.sprintf "%s: %s" prefix msg)

let with_sdl init_fn =
  match Sdl.init Sdl.Init.(video) with
  | Error (`Msg e) -> sdl_fail "SDL init" e
  | Ok () -> (
      ignore (Sdl.set_hint Sdl.Hint.render_scale_quality "linear") ;
      match Ttf.init () with
      | Error (`Msg e) ->
          let () = Sdl.quit () in
          sdl_fail "SDL_ttf init" e
      | Ok () -> (
          try
            let res = init_fn () in
            Ttf.quit () ;
            Sdl.quit () ;
            res
          with e ->
            Ttf.quit () ;
            Sdl.quit () ;
            raise e))

let color_to_sdl ({Sdl_colors.r; g; b; a} : Sdl_colors.color) : Sdl.color =
  Sdl.Color.create ~r ~g ~b ~a

let create_renderer window =
  match
    Sdl.create_renderer
      ~index:(-1)
      ~flags:Sdl.Renderer.(accelerated + presentvsync + targettexture)
      window
  with
  | Error (`Msg e) -> sdl_fail "create_renderer" e
  | Ok r -> r

let render_lines renderer font ~(fg : Sdl_colors.color) ~(bg : Sdl_colors.color)
    ~(char_w : int) ~(char_h : int) ?(clear = true) ?(offset = 0)
    ?(present = true) lines =
  if clear then
    match Sdl.set_render_draw_color renderer bg.r bg.g bg.b bg.a with
    | Error (`Msg e) -> sdl_fail "set_render_draw_color" e
    | Ok () -> (
        match Sdl.render_clear renderer with
        | Error (`Msg e) -> sdl_fail "render_clear" e
        | Ok () -> ())
  else () ;
  let rec render_row y = function
    | [] -> ()
    | line :: rest ->
        let default_state : Sdl_ansi.ansi_state = {fg; bg} in
        let padded =
          if offset <= 0 then line else String.make offset ' ' ^ line
        in
        let segments =
          Sdl_ansi.parse_ansi_segments ~default:default_state padded
        in
        let rec render_seg x = function
          | [] -> ()
          | seg :: tail when String.length seg.Sdl_ansi.text = 0 ->
              render_seg x tail
          | seg :: tail -> (
              let fg_color = seg.Sdl_ansi.fg in
              let bg_color = seg.Sdl_ansi.bg in
              let txt = seg.Sdl_ansi.text in
              let txt_w, txt_h =
                match Ttf.size_utf8 font txt with
                | Ok (w, h) -> (max w char_w, max h char_h)
                | Error _ -> (String.length txt * char_w, char_h)
              in
              if bg_color <> bg then (
                let _ =
                  Sdl.set_render_draw_color
                    renderer
                    bg_color.r
                    bg_color.g
                    bg_color.b
                    bg_color.a
                in
                let rect = Sdl.Rect.create ~x:(12 + x) ~y ~w:txt_w ~h:txt_h in
                ignore (Sdl.render_fill_rect renderer (Some rect)) ;
                ignore (Sdl.set_render_draw_color renderer bg.r bg.g bg.b bg.a)) ;
              match
                Ttf.render_utf8_blended font txt (color_to_sdl fg_color)
              with
              | Error (`Msg e) ->
                  (try Sdl.log "render_utf8_blended failed for '%s': %s" txt e
                   with _ -> ()) ;
                  render_seg x tail
              | Ok surface -> (
                  let texture =
                    match Sdl.create_texture_from_surface renderer surface with
                    | Error (`Msg e) ->
                        Sdl.free_surface surface ;
                        sdl_fail "create_texture_from_surface" e
                    | Ok t -> t
                  in
                  match Sdl.query_texture texture with
                  | Error (`Msg e) ->
                      Sdl.destroy_texture texture ;
                      Sdl.free_surface surface ;
                      sdl_fail "query_texture" e
                  | Ok (_, _, (w, h)) ->
                      let dst = Sdl.Rect.create ~x:(12 + x) ~y ~w ~h in
                      ignore (Sdl.render_copy renderer ~dst texture) ;
                      Sdl.destroy_texture texture ;
                      Sdl.free_surface surface ;
                      render_seg (x + txt_w) tail))
        in
        render_seg 0 segments ;
        render_row (y + char_h) rest
  in
  render_row 10 lines ;
  if present then ignore (Sdl.render_present renderer) else ()

let draw_background renderer cfg _char_w char_h =
  let {Sdl_font.fg; bg; gradient; _} = cfg in
  match Sdl.set_render_draw_color renderer bg.r bg.g bg.b bg.a with
  | Error (`Msg e) -> sdl_fail "set_render_draw_color (bg)" e
  | Ok () -> (
      match Sdl.render_clear renderer with
      | Error (`Msg e) -> sdl_fail "render_clear" e
      | Ok () ->
          if gradient then
            let top_color = fg in
            let bot_color = bg in
            let alpha_step = 1 in
            let alpha_max = 60 in
            let steps = alpha_max / alpha_step in
            for i = 0 to steps - 1 do
              let alpha = i * alpha_step in
              let r = top_color.r + ((bot_color.r - top_color.r) * i / steps) in
              let g = top_color.g + ((bot_color.g - top_color.g) * i / steps) in
              let b = top_color.b + ((bot_color.b - top_color.b) * i / steps) in
              ignore (Sdl.set_render_draw_color renderer r g b (min 255 alpha)) ;
              let y = i * char_h / 5 in
              let rect =
                Sdl.Rect.create ~x:0 ~y ~w:100000 ~h:((char_h / 5) + 2)
              in
              ignore (Sdl.render_fill_rect renderer (Some rect))
            done
          else ())
