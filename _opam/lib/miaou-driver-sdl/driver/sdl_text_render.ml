(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module Sdl = Tsdl.Sdl
module Ttf = Tsdl_ttf.Ttf
module Colors = Sdl_colors

type color = Colors.color

type ansi_state = Colors.ansi_state

let sdl_fail prefix msg = failwith (Printf.sprintf "%s: %s" prefix msg)

let render_lines renderer font ~(fg : color) ~(bg : color) ~(char_w : int)
    ~(char_h : int) ?(clear = true) ?(offset = 0) ?(present = true) lines =
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
        let default_state : ansi_state = {fg; bg} in
        let padded =
          if offset <= 0 then line else String.make offset ' ' ^ line
        in
        let segments =
          Colors.parse_ansi_segments ~default:default_state padded
          |> List.map (fun ((st : ansi_state), txt) -> ((st.fg, st.bg), txt))
        in
        let rec render_seg x = function
          | [] -> ()
          | ((_, _), txt) :: tail when String.length txt = 0 ->
              render_seg x tail
          | ((fg_color, bg_color), txt) :: tail -> (
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
                Ttf.render_utf8_blended font txt (Colors.color_to_sdl fg_color)
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
                      render_seg (x + w) tail))
        in
        render_seg 0 segments ;
        render_row (y + char_h) rest
  in
  render_row 0 lines ;
  if present then Sdl.render_present renderer
