(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** SDL-enhanced line chart widget with smooth anti-aliased rendering.
    
    This module provides SDL-optimized rendering for line charts with:
    - Smooth vector lines with anti-aliasing
    - Filled areas under curves with transparency
    - Precise pixel-level rendering
    - Smooth grid lines and axes *)

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

(** Render line chart directly to SDL renderer with enhanced graphics.
    - [info] SDL rendering context
    - [show_axes] Whether to draw axes
    - [show_grid] Whether to draw background grid lines
    - [thresholds] Optional thresholds for coloring points/lines *)
val render_sdl :
  sdl_render_info ->
  Miaou_widgets_display.Line_chart_widget.t ->
  show_axes:bool ->
  show_grid:bool ->
  ?thresholds:Miaou_widgets_display.Line_chart_widget.threshold list ->
  unit ->
  unit

(** Fallback to text-based rendering for terminal mode.
    Returns the rendered chart as a string. *)
val render :
  Miaou_widgets_display.Line_chart_widget.t ->
  show_axes:bool ->
  show_grid:bool ->
  ?thresholds:Miaou_widgets_display.Line_chart_widget.threshold list ->
  unit ->
  string
