(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module Helpers = Miaou_helpers.Helpers
module W = Widgets

type t = {data : string; matrix : bool array array; scale : int}

let create ~data ?(scale = 1) () =
  try
    match Qrc.encode data with
    | None -> Error "QR code generation failed: data too large"
    | Some qr ->
        let size = Qrc.Matrix.w qr in
        let matrix =
          Array.init size (fun y ->
              Array.init size (fun x -> Qrc.Matrix.get qr ~x ~y))
        in
        Ok {data; matrix; scale}
  with e ->
    Error
      (Printf.sprintf "QR code generation failed: %s" (Printexc.to_string e))

let update_data t ~data = create ~data ~scale:t.scale ()

let get_dimensions t =
  let size = Array.length t.matrix in
  (size * t.scale, size * t.scale)

let get_data t = t.data

let get_module t ~x ~y =
  let size = Array.length t.matrix in
  if x < 0 || x >= size || y < 0 || y >= size then
    invalid_arg
      (Printf.sprintf
         "get_module: coordinates (%d, %d) out of bounds (size=%d)"
         x
         y
         size)
  else t.matrix.(y).(x)

let render t ~focus:_ =
  let size = Array.length t.matrix in
  let scale = t.scale in

  (* Use half-block characters to make QR codes more square *)
  (* Terminal chars are ~2:1 (height:width), so we render 2 rows per character *)
  (* QR codes require Unicode blocks - ASCII fallback would break square aspect ratio *)
  let upper_half = "▀" in
  let lower_half = "▄" in
  let full_block = "█" in
  let empty = " " in

  (* Add quiet zone (2 modules minimum for small QR codes) *)
  let quiet_zone = 2 * scale in
  let total_width = (size + 4) * scale in
  let char_height = (size + (quiet_zone * 2) + 1) / 2 in
  (* 2 modules per char row *)

  let lines = ref [] in

  for char_y = 0 to char_height - 1 do
    let row = Buffer.create (total_width * 4) in
    (* estimate with escape codes *)
    for x = 0 to total_width - 1 do
      let module_y_upper = char_y * 2 in
      let module_y_lower = (char_y * 2) + 1 in

      (* Determine which zone we're in *)
      let get_module y x =
        if
          y < quiet_zone
          || y >= size + quiet_zone
          || x < quiet_zone
          || x >= size + quiet_zone
        then false (* quiet zone = white *)
        else t.matrix.(y - quiet_zone).(x - quiet_zone)
      in

      let upper_dark =
        if module_y_upper < size + (quiet_zone * 2) then
          get_module module_y_upper x
        else false
      in
      let lower_dark =
        if module_y_lower < size + (quiet_zone * 2) then
          get_module module_y_lower x
        else false
      in

      let char =
        match (upper_dark, lower_dark) with
        | false, false -> empty
        | true, true -> full_block
        | true, false -> upper_half
        | false, true -> lower_half
      in

      for _ = 1 to scale do
        Buffer.add_string row char
      done
    done ;
    lines := Buffer.contents row :: !lines
  done ;

  Helpers.concat_lines (List.rev !lines)
