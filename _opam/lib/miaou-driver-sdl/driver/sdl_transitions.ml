(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module Sdl = Tsdl.Sdl
module Ttf = Tsdl_ttf.Ttf
module Colors = Sdl_colors
open LTerm_geom

type config = {
  fg : Colors.color;
  bg : Colors.color;
  gradient : bool;
  scale : float;
  transition : [`None | `Slide | `Fade | `Explode | `Random];
}

let slide renderer _font _cfg _char_w _char_h ~from_lines ~to_lines ~size
    ~(draw_background : unit -> unit)
    ~(render_lines :
       ?clear:bool -> ?offset:int -> ?present:bool -> string list -> unit) =
  let steps = 24 in
  let width = size.cols in
  for step = 0 to steps do
    draw_background () ;
    let off_old = -(step * width / steps) in
    let off_new = width - (step * width / steps) in
    render_lines ~clear:false ~offset:off_old ~present:false from_lines ;
    render_lines ~clear:false ~offset:off_new ~present:false to_lines ;
    ignore (Sdl.render_present renderer) ;
    Sdl.delay 16l
  done

let fade renderer _font cfg char_w char_h ~from_lines ~to_lines ~size
    ~(draw_background : unit -> unit)
    ~(render_lines :
       ?clear:bool -> ?offset:int -> ?present:bool -> string list -> unit) =
  let steps = 24 in
  let bg = cfg.bg in
  let blend_rect =
    Sdl.Rect.create ~x:0 ~y:0 ~w:(size.cols * char_w) ~h:(size.rows * char_h)
  in
  let with_overlay alpha lines =
    draw_background () ;
    render_lines ~clear:false ~present:false lines ;
    ignore (Sdl.set_render_draw_blend_mode renderer Sdl.Blend.mode_blend) ;
    ignore (Sdl.set_render_draw_color renderer bg.r bg.g bg.b alpha) ;
    ignore (Sdl.render_fill_rect renderer (Some blend_rect)) ;
    ignore (Sdl.render_present renderer) ;
    Sdl.delay 12l
  in
  for step = 0 to steps do
    let alpha = int_of_float (255. *. (float step /. float steps)) in
    with_overlay alpha from_lines
  done ;
  for step = 0 to steps do
    let alpha = 255 - int_of_float (255. *. (float step /. float steps)) in
    with_overlay alpha to_lines
  done

let fade_soft renderer _font cfg char_w char_h ~from_lines ~to_lines ~size
    ~(draw_background : unit -> unit)
    ~(render_lines :
       ?clear:bool -> ?offset:int -> ?present:bool -> string list -> unit) =
  let steps = 32 in
  let bg = cfg.bg in
  let blend_rect =
    Sdl.Rect.create ~x:0 ~y:0 ~w:(size.cols * char_w) ~h:(size.rows * char_h)
  in
  let phase lines alpha =
    draw_background () ;
    render_lines ~clear:false ~present:false lines ;
    ignore (Sdl.set_render_draw_blend_mode renderer Sdl.Blend.mode_blend) ;
    ignore (Sdl.set_render_draw_color renderer bg.r bg.g bg.b alpha) ;
    ignore (Sdl.render_fill_rect renderer (Some blend_rect)) ;
    ignore (Sdl.render_present renderer) ;
    Sdl.delay 14l
  in
  for step = 0 to steps do
    let t = float step /. float steps in
    let alpha = int_of_float (255. *. (t *. t)) in
    phase from_lines alpha
  done ;
  for step = 0 to steps do
    let t = float step /. float steps in
    let alpha = int_of_float (255. *. (1. -. (t *. t))) in
    phase to_lines alpha
  done

let explode renderer _font cfg char_w char_h ~from_lines ~to_lines _size
    ~(draw_background : unit -> unit)
    ~(render_lines :
       ?clear:bool -> ?offset:int -> ?present:bool -> string list -> unit) =
  let steps = 30 in
  let dt = 0.018 in
  let gravity = 300.0 in
  let max_particles = 2200 in
  let particles =
    let acc = ref [] in
    let count = ref 0 in
    List.iteri
      (fun row line ->
        String.iteri
          (fun col ch ->
            if !count < max_particles && ch <> ' ' then (
              incr count ;
              let x0 = float_of_int ((col * char_w) + (char_w / 2)) in
              let y0 = float_of_int ((row * char_h) + (char_h / 2)) in
              let angle = Random.float (2.0 *. Float.pi) in
              let speed = 140.0 +. Random.float 260.0 in
              let vx = speed *. cos angle in
              let vy = (speed *. sin angle) -. 40.0 in
              let tint =
                let jitter v =
                  let off = Random.int 30 in
                  max 0 (min 255 (v + off))
                in
                {
                  cfg.fg with
                  r = jitter cfg.fg.r;
                  g = jitter cfg.fg.g;
                  b = jitter cfg.fg.b;
                }
              in
              acc := (ref x0, ref y0, vx, vy, tint) :: !acc))
          line)
      from_lines ;
    !acc
  in
  let draw_particles t =
    ignore (Sdl.set_render_draw_blend_mode renderer Sdl.Blend.mode_blend) ;
    List.iter
      (fun (x, y, vx, vy, c) ->
        let alpha = int_of_float (255. *. max 0.0 (1.0 -. t)) in
        let new_vy = vy +. (gravity *. dt *. float steps /. float steps) in
        x := !x +. (vx *. dt) ;
        y := !y +. (new_vy *. dt) ;
        ignore
          (Sdl.set_render_draw_color
             renderer
             Colors.(c.r)
             Colors.(c.g)
             Colors.(c.b)
             alpha) ;
        let rect =
          Sdl.Rect.create
            ~x:(int_of_float !x)
            ~y:(int_of_float !y)
            ~w:(max 1 (char_w / 2))
            ~h:(max 1 (char_h / 2))
        in
        ignore (Sdl.render_fill_rect renderer (Some rect)))
      particles
  in
  for step = 0 to steps do
    let t = float step /. float steps in
    draw_background () ;
    if step < steps / 3 then render_lines ~clear:false ~present:false from_lines ;
    draw_particles t ;
    if step >= steps / 2 then render_lines ~clear:false ~present:false to_lines ;
    ignore (Sdl.render_present renderer) ;
    Sdl.delay 14l
  done

let pick_random () =
  let options = [`Slide; `Fade; `Explode] in
  let idx = Random.int (List.length options) in
  List.nth options idx

let perform renderer font cfg char_w char_h ~from_lines ~to_lines ~size
    ~draw_background ~render_lines =
  let kind =
    match cfg.transition with
    | `Random -> pick_random ()
    | (`Slide | `Fade | `Explode | `None) as other -> other
  in
  match kind with
  | `Slide ->
      slide
        renderer
        font
        cfg
        char_w
        char_h
        ~from_lines
        ~to_lines
        ~size
        ~draw_background
        ~render_lines
  | `Fade ->
      let choose_soft = Random.int 2 = 0 in
      if choose_soft then
        fade_soft
          renderer
          font
          cfg
          char_w
          char_h
          ~from_lines
          ~to_lines
          ~size
          ~draw_background
          ~render_lines
      else
        fade
          renderer
          font
          cfg
          char_w
          char_h
          ~from_lines
          ~to_lines
          ~size
          ~draw_background
          ~render_lines
  | `Explode ->
      explode
        renderer
        font
        cfg
        char_w
        char_h
        ~from_lines
        ~to_lines
        size
        ~draw_background
        ~render_lines
  | `None -> ()
