(** SDL renderer for image widget with texture caching *)

module Img = Miaou_widgets_display.Image_widget

type cached_texture = {
  texture : Tsdl.Sdl.texture;
  width : int;
  height : int;
  scale : int;
}

let texture_cache : (Img.t, cached_texture) Hashtbl.t = Hashtbl.create 8

let create_cached_texture renderer img_widget scale =
  let open Tsdl.Sdl in
  let width, height = Img.get_dimensions img_widget in
  let tex_width = width * scale in
  let tex_height = height * scale in

  match
    create_texture
      renderer
      Pixel.format_argb8888
      Texture.access_target
      ~w:tex_width
      ~h:tex_height
  with
  | Error (`Msg e) -> failwith ("Failed to create texture: " ^ e)
  | Ok texture ->
      (* Set texture as render target and draw pixels *)
      let _ = set_render_target renderer (Some texture) in

      for py = 0 to height - 1 do
        for px = 0 to width - 1 do
          let pixel = Img.get_pixel img_widget ~x:px ~y:py in
          let _ = set_render_draw_color renderer pixel.r pixel.g pixel.b 255 in
          let rect =
            Rect.create ~x:(px * scale) ~y:(py * scale) ~w:scale ~h:scale
          in
          let _ = render_fill_rect renderer (Some rect) in
          ()
        done
      done ;

      (* Restore default render target *)
      let _ = set_render_target renderer None in
      {texture; width = tex_width; height = tex_height; scale}

let render renderer img_widget ~x ~y ~scale =
  let open Tsdl.Sdl in
  (* Get or create cached texture *)
  let cached =
    match Hashtbl.find_opt texture_cache img_widget with
    | Some c when c.scale = scale -> c
    | _ ->
        (* Create new texture and cache it *)
        let c = create_cached_texture renderer img_widget scale in
        Hashtbl.replace texture_cache img_widget c ;
        c
  in

  (* Render the cached texture *)
  let dst_rect = Rect.create ~x ~y ~w:cached.width ~h:cached.height in
  let _ = render_copy renderer cached.texture ~dst:dst_rect in
  ()
