(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module Sdl = Tsdl.Sdl
module Ttf = Tsdl_ttf.Ttf

val sdl_fail : string -> string -> 'a

val with_sdl : (unit -> 'a) -> 'a

val create_renderer : Sdl.window -> Sdl.renderer

val render_lines :
  Sdl.renderer ->
  Ttf.font ->
  fg:Sdl_colors.color ->
  bg:Sdl_colors.color ->
  char_w:int ->
  char_h:int ->
  ?clear:bool ->
  ?offset:int ->
  ?present:bool ->
  string list ->
  unit

val draw_background : Sdl.renderer -> Sdl_font.config -> int -> int -> unit
