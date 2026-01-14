(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module W = Widgets

type pixel = {r : int; g : int; b : int}

type t = {
  width : int;
  height : int;
  pixels : pixel array array;
  file_path : string option;
  mutable sdl_texture : Obj.t option;
      (* Cached SDL texture - stored as Obj.t to avoid tsdl dependency *)
}

let rgb_to_ansi_256 r g b =
  (* Convert RGB (0-255) to ANSI 256 color code *)
  if r = g && g = b then
    (* Grayscale *)
    if r < 8 then 16 else if r > 248 then 231 else 232 + ((r - 8) / 10)
  else
    (* Color cube: 16 + 36*r + 6*g + b where r,g,b are in 0-5 *)
    let r' = r * 6 / 256 in
    let g' = g * 6 / 256 in
    let b' = b * 6 / 256 in
    16 + (36 * r') + (6 * g') + b'

let pixel_to_ansi_fg pixel =
  Printf.sprintf "\027[38;5;%dm" (rgb_to_ansi_256 pixel.r pixel.g pixel.b)

let pixel_to_ansi_bg pixel =
  Printf.sprintf "\027[48;5;%dm" (rgb_to_ansi_256 pixel.r pixel.g pixel.b)

let ansi_reset = "\027[0m"

let create_from_rgb ~width ~height ~rgb_data () =
  let pixels =
    Array.init height (fun y ->
        Array.init width (fun x ->
            let offset = ((y * width) + x) * 3 in
            {
              r = Char.code (Bytes.get rgb_data offset);
              g = Char.code (Bytes.get rgb_data (offset + 1));
              b = Char.code (Bytes.get rgb_data (offset + 2));
            }))
  in
  {width; height; pixels; file_path = None; sdl_texture = None}

let load_from_file path ~max_width ~max_height () =
  try
    (* Load image using imagelib *)
    let img = ImageLib_unix.openfile path in
    let orig_width = img.width in
    let orig_height = img.height in

    (* Calculate scaled dimensions preserving aspect ratio *)
    (* Note: terminal chars are roughly 2:1 (height:width), so we adjust *)
    let term_max_height = max_height * 2 in
    (* double height since we use half-blocks *)
    let width_scale = float_of_int max_width /. float_of_int orig_width in
    let height_scale =
      float_of_int term_max_height /. float_of_int orig_height
    in
    let scale = min width_scale height_scale in
    let new_width = max 1 (int_of_float (float_of_int orig_width *. scale)) in
    let new_height = max 1 (int_of_float (float_of_int orig_height *. scale)) in

    (* Simple nearest-neighbor scaling *)
    let pixels =
      Array.init new_height (fun y ->
          Array.init new_width (fun x ->
              let src_x =
                min (orig_width - 1) (int_of_float (float_of_int x /. scale))
              in
              let src_y =
                min (orig_height - 1) (int_of_float (float_of_int y /. scale))
              in
              (* ImageLib read_rgba uses a continuation *)
              Image.read_rgba img src_x src_y (fun r g b _a -> {r; g; b})))
    in

    Ok
      {
        width = new_width;
        height = new_height;
        pixels;
        file_path = Some path;
        sdl_texture = None;
      }
  with e ->
    Error (Printf.sprintf "Failed to load image: %s" (Printexc.to_string e))

(* SDL rendering helper - uses Obj.magic to avoid compile-time tsdl dependency.
   This code only runs when SDL context is available at runtime. *)
let render_image_sdl_cached t ctx =
  let open Sdl_chart_context in
  let renderer = get_renderer ctx in
  let char_w = ctx.char_w in
  let char_h = ctx.char_h in
  let x_pixels = 10 * char_w in
  let y_pixels = ctx.y_offset + (5 * char_h) in
  let scale = 1 in
  let tex_width = t.width * scale in
  let tex_height = t.height * scale in

  (* Check if texture exists and is valid *)
  let needs_create =
    match t.sdl_texture with None -> true | Some _ -> false
  in

  (* Create texture if needed - this block uses dynamic SDL calls *)
  if needs_create then begin
    (* These functions are resolved at runtime when SDL driver is loaded *)
    let create_texture = Sdl_chart_context.Sdl_ops.create_texture in
    let set_render_target = Sdl_chart_context.Sdl_ops.set_render_target in
    let set_render_draw_color =
      Sdl_chart_context.Sdl_ops.set_render_draw_color
    in
    let render_fill_rect = Sdl_chart_context.Sdl_ops.render_fill_rect in

    match create_texture renderer tex_width tex_height with
    | None -> ()
    | Some texture ->
        set_render_target renderer (Some texture) ;
        for py = 0 to t.height - 1 do
          for px = 0 to t.width - 1 do
            let pixel = t.pixels.(py).(px) in
            set_render_draw_color renderer pixel.r pixel.g pixel.b 255 ;
            render_fill_rect renderer (px * scale) (py * scale) scale scale
          done
        done ;
        set_render_target renderer None ;
        t.sdl_texture <- Some (Obj.repr texture)
  end ;

  (* Render the texture if it exists *)
  match t.sdl_texture with
  | Some texture_obj ->
      let render_copy = Sdl_chart_context.Sdl_ops.render_copy in
      render_copy
        renderer
        (Obj.obj texture_obj)
        x_pixels
        y_pixels
        tex_width
        tex_height
  | None -> ()

let render ?(crop_center = 1.0) t ~focus:_ =
  (* Check if SDL rendering is available *)
  let ctx_status = Sdl_chart_context.get_context () in
  match ctx_status with
  | Some ctx ->
      (* SDL mode: use cached texture rendering *)
      render_image_sdl_cached t ctx ;

      (* Return placeholder that reserves space for the image *)
      let placeholder_lines = (t.height / 2) + 5 in
      let buf = Buffer.create (placeholder_lines * 2) in
      for i = 0 to placeholder_lines - 1 do
        if i > 0 then Buffer.add_char buf '\n'
      done ;
      Buffer.contents buf
  | None ->
      (* Terminal mode: use colored half-blocks *)
      let crop_width = int_of_float (float_of_int t.width *. crop_center) in
      let crop_height = int_of_float (float_of_int t.height *. crop_center) in
      let crop_x = (t.width - crop_width) / 2 in
      let crop_y = (t.height - crop_height) / 2 in

      let upper_half = if W.prefer_ascii () then "#" else "▀" in
      let full_block = if W.prefer_ascii () then "#" else "█" in

      let char_height = (crop_height + 1) / 2 in
      (* 2 pixels per char row *)
      let result_buf = Buffer.create (char_height * ((crop_width * 20) + 1)) in

      for char_y = 0 to char_height - 1 do
        if char_y > 0 then Buffer.add_char result_buf '\n' ;

        for x = 0 to crop_width - 1 do
          let pixel_y_upper = crop_y + (char_y * 2) in
          let pixel_y_lower = pixel_y_upper + 1 in
          let pixel_x = crop_x + x in

          let has_upper = pixel_y_upper < t.height && pixel_x < t.width in
          let has_lower = pixel_y_lower < t.height && pixel_x < t.width in

          match (has_upper, has_lower) with
          | false, false -> Buffer.add_char result_buf ' '
          | true, false ->
              (* Only upper pixel *)
              let pixel = t.pixels.(pixel_y_upper).(pixel_x) in
              Buffer.add_string result_buf (pixel_to_ansi_fg pixel) ;
              Buffer.add_string result_buf full_block ;
              Buffer.add_string result_buf ansi_reset
          | false, true ->
              (* Only lower pixel *)
              let pixel = t.pixels.(pixel_y_lower).(pixel_x) in
              Buffer.add_string result_buf (pixel_to_ansi_fg pixel) ;
              Buffer.add_string result_buf full_block ;
              Buffer.add_string result_buf ansi_reset
          | true, true ->
              (* Both pixels *)
              let upper_pixel = t.pixels.(pixel_y_upper).(pixel_x) in
              let lower_pixel = t.pixels.(pixel_y_lower).(pixel_x) in
              if upper_pixel = lower_pixel then (
                (* Same color, use full block *)
                Buffer.add_string result_buf (pixel_to_ansi_fg upper_pixel) ;
                Buffer.add_string result_buf full_block ;
                Buffer.add_string result_buf ansi_reset)
              else (
                (* Different colors, use half-block with fg+bg *)
                Buffer.add_string result_buf (pixel_to_ansi_fg upper_pixel) ;
                Buffer.add_string result_buf (pixel_to_ansi_bg lower_pixel) ;
                Buffer.add_string result_buf upper_half ;
                Buffer.add_string result_buf ansi_reset)
        done ;
        Buffer.add_string result_buf ansi_reset
        (* Ensure reset at end of line *)
      done ;

      Buffer.contents result_buf

let get_dimensions t = (t.width, t.height)

let get_file_path t = t.file_path

let get_pixel t ~x ~y =
  if x < 0 || x >= t.width || y < 0 || y >= t.height then
    invalid_arg
      (Printf.sprintf
         "get_pixel: coordinates (%d, %d) out of bounds (width=%d, height=%d)"
         x
         y
         t.width
         t.height)
  else t.pixels.(y).(x)

let get_pixels t = t.pixels

let render_sdl t ~x ~y ~scale =
  (* Returns image data for SDL rendering
     Format: (x, y, width, height, pixel_data)
     where pixel_data is the raw pixel array for direct SDL texture upload *)
  (x, y, t.width * scale, t.height * scale, t.pixels)
