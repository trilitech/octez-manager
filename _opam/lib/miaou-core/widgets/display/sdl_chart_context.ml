(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(* Global SDL rendering context for chart widgets
   This allows chart widgets to detect SDL availability and render directly
   without changing the PAGE_SIG interface or creating circular dependencies. 
   
   We store SDL types as abstract values to avoid hard tsdl dependency here.
   The SDL driver and chart rendering code will cast appropriately. *)

type sdl_context = {
  renderer_obj : Obj.t; (* Tsdl.Sdl.renderer *)
  font_obj : Obj.t; (* Tsdl_ttf.Ttf.font *)
  char_w : int;
  char_h : int;
  mutable y_offset : int;
  enabled : bool;
      (* Set to false during transitions to avoid duplicate rendering *)
  cols : int; (* Terminal width in columns *)
  frame_id : int; (* Increments each frame *)
}

let current_context : sdl_context option ref = ref None

let frame_counter = ref 0

let set_context_obj ~renderer ~font ~char_w ~char_h ~y_offset ~cols
    ?(enabled = true) () =
  incr frame_counter ;
  current_context :=
    Some
      {
        renderer_obj = Obj.repr renderer;
        font_obj = Obj.repr font;
        char_w;
        char_h;
        y_offset;
        enabled;
        cols;
        frame_id = !frame_counter;
      }

let clear_context () = current_context := None

let get_context () =
  match !current_context with
  | Some ctx when ctx.enabled -> Some ctx
  | _ -> None

(* Extract renderer from context - caller must cast *)
let get_renderer ctx = Obj.obj ctx.renderer_obj

let get_font ctx = Obj.obj ctx.font_obj

let get_frame_id ctx = ctx.frame_id

(* Abstract SDL operations - registered by SDL driver at runtime.
   This allows widgets to perform SDL operations without compile-time tsdl dependency.
   All types are abstract (using Obj.t) to avoid importing tsdl. *)
module Sdl_ops = struct
  (* Function references - set by SDL driver when loaded *)
  let create_texture_ref : (Obj.t -> int -> int -> Obj.t option) ref =
    ref (fun _ _ _ -> None)

  let set_render_target_ref : (Obj.t -> Obj.t option -> unit) ref =
    ref (fun _ _ -> ())

  let set_render_draw_color_ref :
      (Obj.t -> int -> int -> int -> int -> unit) ref =
    ref (fun _ _ _ _ _ -> ())

  let render_fill_rect_ref : (Obj.t -> int -> int -> int -> int -> unit) ref =
    ref (fun _ _ _ _ _ -> ())

  let render_copy_ref : (Obj.t -> Obj.t -> int -> int -> int -> int -> unit) ref
      =
    ref (fun _ _ _ _ _ _ -> ())

  (* Public accessors *)
  let create_texture renderer w h = !create_texture_ref renderer w h

  let set_render_target renderer target = !set_render_target_ref renderer target

  let set_render_draw_color renderer r g b a =
    !set_render_draw_color_ref renderer r g b a

  let render_fill_rect renderer x y w h = !render_fill_rect_ref renderer x y w h

  let render_copy renderer texture x y w h =
    !render_copy_ref renderer texture x y w h

  (* Registration functions - called by SDL driver *)
  let register_create_texture f = create_texture_ref := f

  let register_set_render_target f = set_render_target_ref := f

  let register_set_render_draw_color f = set_render_draw_color_ref := f

  let register_render_fill_rect f = render_fill_rect_ref := f

  let register_render_copy f = render_copy_ref := f
end
