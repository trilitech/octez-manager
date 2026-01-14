(** SDL renderer for QR code widget *)

module QR = Miaou_widgets_display.Qr_code_widget

let render renderer qr_widget ~x ~y ~module_size =
  let open Tsdl.Sdl in
  let size = fst (QR.get_dimensions qr_widget) in

  (* Quiet zone: 4 modules on each side for proper QR spec *)
  let quiet_zone = 4 in
  let total_size = (size + (quiet_zone * 2)) * module_size in

  (* Draw white background (quiet zone) *)
  let _ = set_render_draw_color renderer 255 255 255 255 in
  let bg_rect = Rect.create ~x ~y ~w:total_size ~h:total_size in
  let _ = render_fill_rect renderer (Some bg_rect) in

  (* Render QR code modules *)
  let _ = set_render_draw_color renderer 0 0 0 255 in
  for row = 0 to size - 1 do
    for col = 0 to size - 1 do
      let is_dark = QR.get_module qr_widget ~x:col ~y:row in
      if is_dark then
        let rect =
          Rect.create
            ~x:(x + ((quiet_zone + col) * module_size))
            ~y:(y + ((quiet_zone + row) * module_size))
            ~w:module_size
            ~h:module_size
        in
        let _ = render_fill_rect renderer (Some rect) in
        ()
    done
  done
