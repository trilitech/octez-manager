(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module Sdl = Tsdl.Sdl
module Ttf = Tsdl_ttf.Ttf
open LTerm_geom

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

let create_renderer window =
  match
    Sdl.create_renderer
      ~index:(-1)
      ~flags:Sdl.Renderer.(accelerated + presentvsync + targettexture)
      window
  with
  | Error (`Msg e) -> sdl_fail "create_renderer" e
  | Ok r -> r

let create_window title ~w ~h =
  match
    Sdl.create_window ~w ~h title Sdl.Window.(shown + resizable + allow_highdpi)
  with
  | Error (`Msg e) -> sdl_fail "create_window" e
  | Ok w -> w

let size_from_window ~char_w ~char_h ~scale window =
  let w, h = Sdl.get_window_size window in
  let cw = max 1 (int_of_float (float char_w *. scale)) in
  let ch = max 1 (int_of_float (float char_h *. scale)) in
  let cols = max 40 (w / cw) in
  let rows = max 12 (h / ch) in
  {rows; cols}
