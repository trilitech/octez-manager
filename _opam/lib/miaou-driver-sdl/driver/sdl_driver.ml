(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

[@@@warning "-32-34-37-69"]

module Logger_capability = Miaou_interfaces.Logger_capability
module Capture = Miaou_core.Tui_capture
module Modal_manager = Miaou_core.Modal_manager
module Registry = Miaou_core.Registry
module Driver_common = Miaou_driver_common.Driver_common
module Fibers = Miaou_helpers.Fiber_runtime

module Page_transition = Miaou_driver_common.Driver_common.Page_transition_utils

open Miaou_core.Tui_page
module Ttf = Tsdl_ttf.Ttf
module Sdl = Tsdl.Sdl
module Colors = Sdl_colors
open LTerm_geom

let () = Random.self_init ()

let available = true

(* Debug overlay - shows FPS/TPS when MIAOU_OVERLAY is set *)
let overlay_enabled =
  lazy
    (match Sys.getenv_opt "MIAOU_OVERLAY" with
    | Some ("1" | "true" | "TRUE" | "yes" | "YES") -> true
    | _ -> false)

type fps_tracker = {
  mutable frame_count : int;
  mutable last_time : float;
  mutable current_fps : float;
}

let create_fps_tracker () =
  {frame_count = 0; last_time = Unix.gettimeofday (); current_fps = 0.0}

let update_fps tracker =
  tracker.frame_count <- tracker.frame_count + 1 ;
  let now = Unix.gettimeofday () in
  let elapsed = now -. tracker.last_time in
  if elapsed >= 1.0 then begin
    tracker.current_fps <- float_of_int tracker.frame_count /. elapsed ;
    tracker.frame_count <- 0 ;
    tracker.last_time <- now
  end

type color = Colors.color

(* TODO: Architecture improvement - GADT-Based Render Tree Abstraction
   =====================================================================
   
   Current approach: Chart widgets have dual rendering methods (text + SDL).
   Pages return strings from view(). SDL rendering uses global context with
   Obj.magic to pass renderer without circular dependencies. This works but
   is not type-safe and requires widget-specific SDL integration.
   
   Future improvement: GADT-based render tree with typed backends
   ---------------------------------------------------------------
   Instead of widgets returning strings, use a typed intermediate representation:
   
   type _ backend = 
     | Terminal : terminal_config -> string backend
     | SDL : sdl_config -> unit backend
   
   type 'a render_tree =
     | Text : string -> 'a render_tree
     | Box : { x:int; y:int; width:int; height:int; style:style } -> 'a render_tree
     | Sparkline : sparkline_spec -> 'a render_tree
     | LineChart : chart_spec -> 'a render_tree
     | VStack : 'a render_tree list -> 'a render_tree
     | HStack : 'a render_tree list -> 'a render_tree
   
   type interpreter = {
     render: 'a. 'a backend -> 'a render_tree -> 'a
   }
   
   Benefits:
   - Type-safe: Backend selection checked at compile time
   - Clean: No global mutable state or Obj.magic needed
   - Extensible: Easy to add new backends (HTML, SVG, PNG export)
   - Composable: Render trees can be analyzed, transformed, optimized
   - Testable: Can inspect trees without rendering
   - No duplication: Single widget implementation, multiple interpreters
   - Future-proof: Ready for server-side rendering, image generation, etc.
   
   Implementation path:
   1. Define render tree types in miaou_core (GADTs for type safety)
   2. Create interpreter interface and terminal/SDL implementations
   3. Update one widget as proof-of-concept (e.g., sparkline)
   4. Gradually migrate other widgets
   5. Update PAGE_SIG to return render trees
   6. Update all pages' view functions
   7. Remove old text-based APIs once migration complete
   
   Challenges:
   - Breaking change: All widgets and pages need updates
   - Learning curve: GADTs may be unfamiliar to contributors
   - Initial effort: ~500-1000 LOC across the codebase
   - Migration period: Need to support both APIs temporarily
   
   Estimated timeline:
   - Phase 1 (types + proof-of-concept): 1-2 days
   - Phase 2 (widget migration): 3-5 days
   - Phase 3 (page migration): 2-3 days
   - Phase 4 (cleanup + tests): 1-2 days
   
   Trade-off: Current approach (global SDL context + Obj.magic) works now
   and doesn't block future migration. The GADT approach is the "right way"
   long-term but requires significant refactoring effort.
*)

type ansi_state = Colors.ansi_state

type config = {
  font_path : string option;
  font_size : int;
  window_title : string;
  fg : color;
  bg : color;
  gradient : bool;
  scale : float;
  transition : [`None | `Slide | `Fade | `Explode | `Random];
}

let detect_display_scale () =
  match Sdl.get_display_dpi 0 with
  | Ok (_ddpi, hdpi, _vdpi) ->
      let scale = hdpi /. 96.0 in
      let clamped = max 1.0 (min 3.0 scale) in
      Some clamped
  | Error _ -> None

let default_config =
  {
    font_path = None;
    font_size =
      (match Sys.getenv_opt "MIAOU_SDL_FONT_SIZE" with
      | Some s -> ( try int_of_string s with _ -> 16)
      | None -> 16);
    window_title =
      Sys.getenv_opt "MIAOU_SDL_WINDOW_TITLE" |> Option.value ~default:"Miaou";
    fg = Colors.{r = 235; g = 235; b = 235; a = 255};
    bg = Colors.{r = 20; g = 20; b = 20; a = 255};
    gradient =
      (match Sys.getenv_opt "MIAOU_SDL_GRADIENT" with
      | Some v ->
          let v = String.lowercase_ascii (String.trim v) in
          not (v = "0" || v = "false" || v = "off")
      | None -> true);
    scale =
      (match Sys.getenv_opt "MIAOU_SDL_SCALE" with
      | Some v -> ( try float_of_string v with _ -> 2.0)
      | None -> detect_display_scale () |> Option.value ~default:2.0);
    transition =
      (match Sys.getenv_opt "MIAOU_SDL_TRANSITION" with
      | Some v ->
          let v = String.lowercase_ascii (String.trim v) in
          if v = "slide" then `Slide
          else if v = "fade" then `Fade
          else if v = "explode" then `Explode
          else if v = "random" then `Random
          else `None
      | None -> `Slide);
  }

let font_candidates =
  [
    "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf";
    "/usr/share/fonts/truetype/liberation/LiberationMono-Regular.ttf";
    "/Library/Fonts/Menlo-Regular.ttf";
    "/System/Library/Fonts/Menlo.ttc";
    "/usr/share/fonts/TTF/DejaVuSansMono.ttf";
  ]

let pick_font_path (cfg : config) =
  match cfg.font_path with
  | Some p when Sys.file_exists p -> Ok p
  | Some p ->
      Error
        (Printf.sprintf
           "Configured MIAOU_SDL_FONT does not exist: %s (current working \
            directory: %s)"
           p
           (Sys.getcwd ()))
  | None -> (
      match Sys.getenv_opt "MIAOU_SDL_FONT" with
      | Some env when Sys.file_exists env -> Ok env
      | Some env ->
          Error
            (Printf.sprintf
               "Configured MIAOU_SDL_FONT does not exist: %s (cwd: %s)"
               env
               (Sys.getcwd ()))
      | None -> (
          (* Pick the first available candidate; if none exist, report all. *)
          let available = List.filter Sys.file_exists font_candidates in
          match available with
          | p :: _ -> Ok p
          | [] ->
              Error
                (Printf.sprintf
                   "Could not find any monospaced font. Provide \
                    MIAOU_SDL_FONT=<path> to a .ttf file. Probed: %s"
                   (String.concat ", " font_candidates))))

let sdl_fail prefix msg = failwith (Printf.sprintf "%s: %s" prefix msg)

(* Register SDL operations with the abstract Sdl_ops module in widgets.display.
   This allows widgets to perform SDL operations without compile-time tsdl dependency. *)
let register_sdl_ops () =
  let module Ops = Miaou_widgets_display.Sdl_chart_context.Sdl_ops in
  Ops.register_create_texture (fun renderer_obj w h ->
      let renderer : Sdl.renderer = Obj.obj renderer_obj in
      match
        Sdl.create_texture
          renderer
          Sdl.Pixel.format_argb8888
          Sdl.Texture.access_target
          ~w
          ~h
      with
      | Error _ -> None
      | Ok tex -> Some (Obj.repr tex)) ;

  Ops.register_set_render_target (fun renderer_obj target_opt ->
      let renderer : Sdl.renderer = Obj.obj renderer_obj in
      let target = Option.map (fun t -> Obj.obj t) target_opt in
      ignore (Sdl.set_render_target renderer target)) ;

  Ops.register_set_render_draw_color (fun renderer_obj r g b a ->
      let renderer : Sdl.renderer = Obj.obj renderer_obj in
      ignore (Sdl.set_render_draw_color renderer r g b a)) ;

  Ops.register_render_fill_rect (fun renderer_obj x y w h ->
      let renderer : Sdl.renderer = Obj.obj renderer_obj in
      let rect = Sdl.Rect.create ~x ~y ~w ~h in
      ignore (Sdl.render_fill_rect renderer (Some rect))) ;

  Ops.register_render_copy (fun renderer_obj texture_obj x y w h ->
      let renderer : Sdl.renderer = Obj.obj renderer_obj in
      let texture : Sdl.texture = Obj.obj texture_obj in
      let dst_rect = Sdl.Rect.create ~x ~y ~w ~h in
      ignore (Sdl.render_copy renderer texture ~dst:dst_rect))

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
            register_sdl_ops () ;
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

let render_lines renderer font ~(fg : color) ~(bg : color) ~(char_w : int)
    ~(char_h : int) ?(clear = true) ?(offset = 0) ?(present = true) lines =
  (* Clear background first when requested. *)
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
          |> List.map (fun ((ps : ansi_state), txt) -> ((ps.fg, ps.bg), txt))
        in
        let rec render_seg x = function
          | [] -> ()
          | ((_, _), txt) :: tail when String.length txt = 0 ->
              render_seg x tail
          | ((fg_color, bg_color), txt) :: tail -> (
              (* Draw background block if non-default. *)
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
                (* Restore default draw color for subsequent clears. *)
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
                      render_seg (x + txt_w) tail))
        in
        render_seg 0 segments ;
        render_row (y + char_h) rest
  in
  render_row 10 lines ;
  if present then ignore (Sdl.render_present renderer) else ()

let shift_lines lines dx =
  List.map
    (fun line ->
      if dx >= 0 then String.make dx ' ' ^ line
      else
        let drop = min (String.length line) (-dx) in
        if drop >= String.length line then ""
        else String.sub line drop (String.length line - drop))
    lines

let draw_background renderer cfg _char_w char_h =
  if cfg.gradient then
    let w, h =
      match Sdl.get_renderer_output_size renderer with
      | Ok (w, h) -> (w, h)
      | Error (`Msg e) -> sdl_fail "get_renderer_output_size" e
    in
    let steps =
      max 1 (int_of_float (float h /. max 1.0 (float char_h *. cfg.scale)))
    in
    let lerp a b t =
      let a = float_of_int a in
      let b = float_of_int b in
      int_of_float (a +. ((b -. a) *. t))
    in
    let target =
      let dim v = max 0 (v - 25) in
      Colors.
        {r = dim cfg.bg.r; g = dim cfg.bg.g; b = dim cfg.bg.b; a = cfg.bg.a}
    in
    for i = 0 to steps do
      let t = float_of_int i /. float_of_int (max 1 steps) in
      let col =
        Colors.
          {
            r = lerp cfg.bg.r target.r t;
            g = lerp cfg.bg.g target.g t;
            b = lerp cfg.bg.b target.b t;
            a = cfg.bg.a;
          }
      in
      ignore (Sdl.set_render_draw_color renderer col.r col.g col.b col.a) ;
      let y = int_of_float (float (i * char_h) *. cfg.scale) in
      ignore
        (Sdl.render_fill_rect
           renderer
           (Some
              (Sdl.Rect.create
                 ~x:0
                 ~y
                 ~w:(int_of_float (float w /. cfg.scale))
                 ~h:(int_of_float (float char_h *. cfg.scale)))))
    done
  else
    match
      Sdl.set_render_draw_color renderer cfg.bg.r cfg.bg.g cfg.bg.b cfg.bg.a
    with
    | Ok () -> ignore (Sdl.render_clear renderer)
    | Error (`Msg e) -> sdl_fail "render_clear" e

let perform_transition renderer font cfg char_w char_h ~from_lines ~to_lines
    ~size =
  let trans_cfg =
    {
      Sdl_transitions.fg = cfg.fg;
      bg = cfg.bg;
      gradient = cfg.gradient;
      scale = cfg.scale;
      transition = cfg.transition;
    }
  in
  let draw_background () = draw_background renderer cfg char_w char_h in
  let render_lines_helper ?clear ?offset ?present lines =
    render_lines
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
    ~draw_background
    ~render_lines:render_lines_helper

let string_of_event_text e =
  match Sdl.Event.(get e typ |> enum) with
  | `Text_input -> Some Sdl.Event.(get e text_input_text)
  | _ -> None

let keyname_of_scancode sc =
  match Sdl.Scancode.enum sc with
  | `Return -> Some "Enter"
  | `Up -> Some "Up"
  | `Down -> Some "Down"
  | `Left -> Some "Left"
  | `Right -> Some "Right"
  | `Tab -> Some "Tab"
  | `Backspace -> Some "Backspace"
  | `Escape -> Some "Esc"
  | `Delete -> Some "Delete"
  | `Space -> Some " "
  | `A -> Some "a"
  | `B -> Some "b"
  | `C -> Some "c"
  | `D -> Some "d"
  | `E -> Some "e"
  | `F -> Some "f"
  | `G -> Some "g"
  | `H -> Some "h"
  | `I -> Some "i"
  | `J -> Some "j"
  | `K -> Some "k"
  | `L -> Some "l"
  | `M -> Some "m"
  | `N -> Some "n"
  | `O -> Some "o"
  | `P -> Some "p"
  | `Q -> Some "q"
  | `R -> Some "r"
  | `S -> Some "s"
  | `T -> Some "t"
  | `U -> Some "u"
  | `V -> Some "v"
  | `W -> Some "w"
  | `X -> Some "x"
  | `Y -> Some "y"
  | `Z -> Some "z"
  | _ -> None

type next_action = Refresh | Quit | Key of string

let poll_event ~timeout_ms ~on_resize =
  let e = Sdl.Event.create () in
  let start = Sdl.get_ticks () in
  let rec loop () =
    match Sdl.poll_event (Some e) with
    | true -> (
        match Sdl.Event.(get e typ |> enum) with
        | `Quit -> Quit
        | `Window_event ->
            on_resize () ;
            Refresh
        | `Key_down -> (
            let repeat = Sdl.Event.get e Sdl.Event.keyboard_repeat <> 0 in
            match
              Sdl.Event.(get e keyboard_scancode |> keyname_of_scancode)
            with
            | Some k ->
                (* Treat repeat as a second press only for navigation keys; drop
                   repeats for Esc/Enter/Space to avoid double firing. *)
                let is_nav =
                  match k with
                  | "Up" | "Down" | "Left" | "Right" | "Tab" -> true
                  | _ -> false
                in
                if repeat && not is_nav then loop () else Key k
            | None -> loop ())
        | `Text_input -> (
            match string_of_event_text e with
            | Some " " -> loop ()
            | Some txt -> Key txt
            | None -> loop ())
        | _ -> loop ())
    | false ->
        let elapsed_ms = Int32.(to_int (sub (Sdl.get_ticks ()) start)) in
        if elapsed_ms > timeout_ms then Refresh
        else (
          Sdl.delay 1l ;
          loop ())
  in
  loop ()

let render_page (type s) (module P : PAGE_SIG with type state = s)
    (ps : s Page_transition.Navigation.t) size =
  let base = P.view ps ~focus:true ~size in
  Driver_common.Modal_utils.render_with_modal_overlay
    ~view:base
    ~rows:size.rows
    ~cols:size.cols

let run_with_sdl (initial_page : (module PAGE_SIG)) (cfg : config) :
    [`Quit | `SwitchTo of string] =
  let available = true in
  ignore available ;
  Fibers.with_page_scope (fun () ->
      with_sdl @@ fun () ->
      Miaou_widgets_display.Widgets.set_backend `Sdl ;
      let font_path =
        match pick_font_path cfg with Ok p -> p | Error msg -> failwith msg
      in
      let font =
        match Ttf.open_font font_path cfg.font_size with
        | Error (`Msg e) ->
            sdl_fail "open_font" (Printf.sprintf "font=%s: %s" font_path e)
        | Ok f -> f
      in
      let char_w, char_h =
        match Ttf.size_utf8 font "M" with
        | Error (`Msg e) -> sdl_fail "size_utf8" e
        | Ok (w, h) -> (max 8 w, max 12 h)
      in
      let win =
        Sdl_window.create_window
          cfg.window_title
          ~w:(80 * char_w)
          ~h:(30 * char_h)
      in
      let renderer = create_renderer win in
      ignore (Sdl.render_set_scale renderer cfg.scale cfg.scale) ;
      ignore (Sdl.start_text_input ()) ;
      let size_ref =
        ref (Sdl_window.size_from_window ~char_w ~char_h ~scale:cfg.scale win)
      in
      let update_size () =
        let s =
          Sdl_window.size_from_window ~char_w ~char_h ~scale:cfg.scale win
        in
        size_ref := s ;
        Modal_manager.set_current_size s.rows s.cols
      in
      update_size () ;

      (* FPS tracker for debug overlay *)
      let fps_tracker = create_fps_tracker () in

      let render_and_draw (type s) (module P : PAGE_SIG with type state = s)
          (ps : s Page_transition.Navigation.t) =
        let size = !size_ref in
        update_fps fps_tracker ;

        (* Draw background FIRST, before any SDL chart rendering *)
        draw_background renderer cfg char_w char_h ;

        (* Set up SDL rendering context for enhanced chart widgets *)
        Miaou_widgets_display.Sdl_chart_context.set_context_obj
          ~renderer
          ~font
          ~char_w
          ~char_h
          ~y_offset:(char_h * 4)
            (* Start at line 4 - SDL charts stay at original position *)
          ~cols:size.cols
          ~enabled:true
          () ;

        (* Render page (may use SDL context for charts) *)
        let text = render_page (module P) ps size in

        (* Clear SDL context *)
        Miaou_widgets_display.Sdl_chart_context.clear_context () ;

        let default_state : ansi_state = {fg = cfg.fg; bg = cfg.bg} in
        let clean_text =
          Colors.strip_ansi_to_text ~default:default_state text
        in
        Capture.record_frame ~rows:size.rows ~cols:size.cols clean_text ;

        (* Render text (without clearing background) *)
        render_lines
          renderer
          font
          ~fg:cfg.fg
          ~bg:cfg.bg
          ~char_w
          ~char_h
          ~clear:false (* Already cleared by draw_background above *)
          ~present:false
          (String.split_on_char '\n' text) ;

        (* Render debug overlay if enabled *)
        if Lazy.force overlay_enabled then begin
          let overlay_text =
            Printf.sprintf "sdl FPS:%.0f" fps_tracker.current_fps
          in
          let x = (size.cols - String.length overlay_text - 1) * char_w in
          let y = 0 in
          (* Render with dim gray color *)
          let dim_gray = {Colors.r = 128; g = 128; b = 128; a = 255} in
          ignore
            (Sdl.set_render_draw_color
               renderer
               dim_gray.r
               dim_gray.g
               dim_gray.b
               dim_gray.a) ;
          match
            Ttf.render_utf8_solid
              font
              overlay_text
              (Sdl.Color.create ~r:128 ~g:128 ~b:128 ~a:255)
          with
          | Error _ -> ()
          | Ok surface -> (
              match Sdl.create_texture_from_surface renderer surface with
              | Error _ -> Sdl.free_surface surface
              | Ok texture ->
                  let _, _, (w, h) =
                    Result.get_ok (Sdl.query_texture texture)
                  in
                  let dst = Sdl.Rect.create ~x ~y ~w ~h in
                  ignore (Sdl.render_copy renderer texture ~dst) ;
                  Sdl.destroy_texture texture ;
                  Sdl.free_surface surface)
        end ;

        ignore (Sdl.render_present renderer)
      in

      let rec loop : type s.
          (module PAGE_SIG with type state = s) ->
          s Page_transition.Navigation.t ->
          [`Quit | `SwitchTo of string] =
       fun (module P : PAGE_SIG with type state = s)
           (ps : s Page_transition.Navigation.t)
         ->
        render_and_draw (module P) ps ;
        match poll_event ~timeout_ms:16 ~on_resize:update_size with
        | Quit -> `Quit
        | Refresh ->
            let ps' = P.service_cycle (P.refresh ps) 0 in
            Page_transition.handle_next_page
              (module P)
              ps'
              {
                on_quit = (fun () -> `Quit);
                on_same_page = (fun () -> loop (module P) ps');
                on_new_page =
                  (fun (type a)
                       (module Next : PAGE_SIG with type state = a)
                       (ps_to : a Page_transition.Navigation.t)
                     ->
                    let size = !size_ref in
                    (* Disable SDL chart rendering during transition text capture *)
                    Miaou_widgets_display.Sdl_chart_context.set_context_obj
                      ~renderer
                      ~font
                      ~char_w
                      ~char_h
                      ~y_offset:0
                      ~cols:size.cols
                      ~enabled:false
                      () ;
                    let from_text = render_page (module P) ps size in
                    let to_text = render_page (module Next) ps_to size in
                    Miaou_widgets_display.Sdl_chart_context.clear_context () ;
                    perform_transition
                      renderer
                      font
                      cfg
                      char_w
                      char_h
                      ~from_lines:(String.split_on_char '\n' from_text)
                      ~to_lines:(String.split_on_char '\n' to_text)
                      ~size ;
                    loop (module Next) ps_to);
              }
        | Key k ->
            (match Logger_capability.get () with
            | Some logger ->
                logger.logf Debug (Printf.sprintf "SDL driver key=%s" k)
            | None -> ()) ;
            let forced_switch =
              String.length k > 11
              && String.sub k 0 11 = "__SWITCH__:"
              && Sys.getenv_opt "MIAOU_TEST_ALLOW_FORCED_SWITCH" = Some "1"
            in
            if not forced_switch then Capture.record_keystroke k ;
            if forced_switch then
              let name = String.sub k 11 (String.length k - 11) in
              match Registry.find name with
              | Some (module Next : PAGE_SIG) ->
                  let ps_to = Next.init () in
                  let size = !size_ref in
                  (* Disable SDL chart rendering during transition text capture *)
                  Miaou_widgets_display.Sdl_chart_context.set_context_obj
                    ~renderer
                    ~font
                    ~char_w
                    ~char_h
                    ~y_offset:0
                    ~cols:size.cols
                    ~enabled:false
                    () ;
                  let from_text = render_page (module P) ps size in
                  let to_text = render_page (module Next) ps_to size in
                  Miaou_widgets_display.Sdl_chart_context.clear_context () ;
                  perform_transition
                    renderer
                    font
                    cfg
                    char_w
                    char_h
                    ~from_lines:(String.split_on_char '\n' from_text)
                    ~to_lines:(String.split_on_char '\n' to_text)
                    ~size ;
                  loop (module Next) ps_to
              | None -> `Quit
            else
              let size = !size_ref in
              let ps' =
                (* Handle keys like lambda-term and matrix drivers:
                   1. Modal_manager modals first
                   2. Page-level modals (Page.has_modal)
                   3. Enter -> Page.enter
                   4. Esc -> Page.handle_key
                   5. Navigation keys -> Page.handle_key
                   6. Other keys -> keymap first, then handle_key *)
                if Modal_manager.has_active () then (
                  Modal_manager.handle_key k ;
                  ps)
                else if P.has_modal ps then P.handle_modal_key ps k ~size
                else if k = "Enter" then P.handle_key ps "Enter" ~size
                else if k = "Esc" || k = "Escape" then P.handle_key ps k ~size
                else if
                  k = "Up" || k = "Down" || k = "Left" || k = "Right"
                  || k = "Tab" || k = "Shift-Tab"
                then P.handle_key ps k ~size
                else
                  (* Try keymap first, fall back to handle_key *)
                  let keymap = P.keymap ps in
                  let keymap_match =
                    List.find_opt
                      (fun (kb : P.key_binding) -> kb.key = k)
                      keymap
                  in
                  match keymap_match with
                  | Some kb when not kb.display_only -> kb.action ps
                  | _ -> P.handle_key ps k ~size
              in
              (* Run service_cycle after handling key, like other drivers *)
              let ps' = P.service_cycle ps' 0 in
              render_and_draw (module P) ps' ;
              Page_transition.handle_next_page
                (module P)
                ps'
                {
                  on_quit = (fun () -> `Quit);
                  on_same_page = (fun () -> loop (module P) ps');
                  on_new_page =
                    (fun (type a)
                         (module Next : PAGE_SIG with type state = a)
                         (ps_to : a Page_transition.Navigation.t)
                       ->
                      let size = !size_ref in
                      (* Disable SDL chart rendering during transition text capture *)
                      Miaou_widgets_display.Sdl_chart_context.set_context_obj
                        ~renderer
                        ~font
                        ~char_w
                        ~char_h
                        ~y_offset:0
                        ~cols:size.cols
                        ~enabled:false
                        () ;
                      let from_text = render_page (module P) ps' size in
                      let to_text = render_page (module Next) ps_to size in
                      Miaou_widgets_display.Sdl_chart_context.clear_context () ;
                      perform_transition
                        renderer
                        font
                        cfg
                        char_w
                        char_h
                        ~from_lines:(String.split_on_char '\n' from_text)
                        ~to_lines:(String.split_on_char '\n' to_text)
                        ~size ;
                      loop (module Next) ps_to);
                }
      in
      Fun.protect
        ~finally:(fun () ->
          Miaou_widgets_display.Widgets.set_backend `Terminal ;
          Ttf.close_font font ;
          Sdl.destroy_renderer renderer ;
          Sdl.destroy_window win)
        (fun () ->
          let module P0 : PAGE_SIG = (val initial_page) in
          loop (module P0) (P0.init ())))

let run ?config initial_page =
  let cfg = Option.value ~default:default_config config in
  run_with_sdl initial_page cfg
