(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type config = {
  font_path : string option;
  font_size : int;
  window_title : string;
  fg : Sdl_colors.color;
  bg : Sdl_colors.color;
  gradient : bool;
  scale : float;
  transition : [`None | `Slide | `Fade | `Explode | `Random];
}

val font_candidates : string list

val pick_font_path : config -> (string, string) result

val detect_display_scale : unit -> float

val default_config : config
