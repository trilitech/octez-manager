(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** SDL-enhanced sparkline widget with smooth vector rendering.
    
    This module provides SDL-optimized rendering for sparklines with:
    - Smooth anti-aliased lines
    - Filled area under the curve
    - Gradient support
    - Higher precision than character-based rendering *)

(** SDL rendering context information *)
type sdl_render_info = {
  renderer : Tsdl.Sdl.renderer;
  x : int;  (** Top-left X position in pixels *)
  y : int;  (** Top-left Y position in pixels *)
  width : int;  (** Width in character cells *)
  height : int;  (** Height in character cells *)
  char_w : int;  (** Character cell width in pixels *)
  char_h : int;  (** Character cell height in pixels *)
}

(** Render sparkline directly to SDL renderer with enhanced graphics.
    - [info] SDL rendering context
    - [focus] Whether to highlight (currently has minimal effect in SDL mode)
    - [show_value] If true, value display handled separately (not rendered here)
    - [color] Optional ANSI color code for the sparkline
    - [thresholds] Optional thresholds for coloring segments *)
val render_sdl :
  sdl_render_info ->
  Miaou_widgets_display.Sparkline_widget.t ->
  focus:bool ->
  show_value:bool ->
  ?color:string ->
  ?thresholds:Miaou_widgets_display.Sparkline_widget.threshold list ->
  unit ->
  unit

(** Fallback to text-based rendering for terminal mode.
    Returns the rendered sparkline as a string. *)
val render :
  Miaou_widgets_display.Sparkline_widget.t ->
  focus:bool ->
  show_value:bool ->
  ?color:string ->
  ?thresholds:Miaou_widgets_display.Sparkline_widget.threshold list ->
  unit ->
  string

(** Render with a label prefix (text mode only). *)
val render_with_label :
  Miaou_widgets_display.Sparkline_widget.t ->
  label:string ->
  focus:bool ->
  ?color:string ->
  ?thresholds:Miaou_widgets_display.Sparkline_widget.threshold list ->
  unit ->
  string
