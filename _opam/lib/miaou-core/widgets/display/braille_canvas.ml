(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(* Braille canvas implementation using Unicode Braille patterns (U+2800–U+28FF).
   Each cell contains an 8-dot pattern arranged as a 2×4 grid. *)

type t = {
  width : int; (* Width in cells *)
  height : int; (* Height in cells *)
  cells : int array array; (* Each int is a braille pattern bitmask (0-255) *)
}

(* Braille dot positions in the 2×4 grid:
   Column 0: dots 1,2,3,7 (left column)
   Column 1: dots 4,5,6,8 (right column)

   Braille Unicode offset mapping:
   Dot 1 (row 0, col 0): bit 0 (0x01)
   Dot 2 (row 1, col 0): bit 1 (0x02)
   Dot 3 (row 2, col 0): bit 2 (0x04)
   Dot 4 (row 0, col 1): bit 3 (0x08)
   Dot 5 (row 1, col 1): bit 4 (0x10)
   Dot 6 (row 2, col 1): bit 5 (0x20)
   Dot 7 (row 3, col 0): bit 6 (0x40)
   Dot 8 (row 3, col 1): bit 7 (0x80)
*)

let braille_base = 0x2800

(* Precompute UTF-8 encoded braille glyphs for all 256 patterns to avoid
   per-call encoding overhead in render_with. *)
let braille_glyphs : string array =
  Array.init 256 (fun pattern ->
      let unicode_point = braille_base + pattern in
      if unicode_point <= 0x7F then String.make 1 (Char.chr unicode_point)
      else if unicode_point <= 0x7FF then
        String.init 2 (fun i ->
            match i with
            | 0 -> Char.chr (0xC0 lor (unicode_point lsr 6))
            | _ -> Char.chr (0x80 lor (unicode_point land 0x3F)))
      else if unicode_point <= 0xFFFF then
        String.init 3 (fun i ->
            match i with
            | 0 -> Char.chr (0xE0 lor (unicode_point lsr 12))
            | 1 -> Char.chr (0x80 lor ((unicode_point lsr 6) land 0x3F))
            | _ -> Char.chr (0x80 lor (unicode_point land 0x3F)))
      else
        String.init 4 (fun i ->
            match i with
            | 0 -> Char.chr (0xF0 lor (unicode_point lsr 18))
            | 1 -> Char.chr (0x80 lor ((unicode_point lsr 12) land 0x3F))
            | 2 -> Char.chr (0x80 lor ((unicode_point lsr 6) land 0x3F))
            | _ -> Char.chr (0x80 lor (unicode_point land 0x3F))))

(* Map dot position within a cell to bit offset *)
let dot_to_bit ~dot_x ~dot_y =
  match (dot_y, dot_x) with
  | 0, 0 -> 0x01 (* Dot 1 *)
  | 1, 0 -> 0x02 (* Dot 2 *)
  | 2, 0 -> 0x04 (* Dot 3 *)
  | 0, 1 -> 0x08 (* Dot 4 *)
  | 1, 1 -> 0x10 (* Dot 5 *)
  | 2, 1 -> 0x20 (* Dot 6 *)
  | 3, 0 -> 0x40 (* Dot 7 *)
  | 3, 1 -> 0x80 (* Dot 8 *)
  | _ -> 0 (* Invalid position *)

let create ~width ~height =
  {
    width;
    height;
    cells = Array.make_matrix height width 0;
    (* Initialize all cells to 0x2800 (blank braille) *)
  }

let get_dimensions t = (t.width, t.height)

let get_dot_dimensions t = (t.width * 2, t.height * 4)

let set_dot t ~x ~y =
  if x >= 0 && y >= 0 then
    let cell_x = x / 2 in
    let cell_y = y / 4 in
    if cell_y < t.height && cell_x < t.width then
      let dot_x = x mod 2 in
      let dot_y = y mod 4 in
      let bit = dot_to_bit ~dot_x ~dot_y in
      t.cells.(cell_y).(cell_x) <- t.cells.(cell_y).(cell_x) lor bit

let clear_dot t ~x ~y =
  if x >= 0 && y >= 0 then
    let cell_x = x / 2 in
    let cell_y = y / 4 in
    if cell_y < t.height && cell_x < t.width then
      let dot_x = x mod 2 in
      let dot_y = y mod 4 in
      let bit = dot_to_bit ~dot_x ~dot_y in
      t.cells.(cell_y).(cell_x) <- t.cells.(cell_y).(cell_x) land lnot bit

let get_dot t ~x ~y =
  if x >= 0 && y >= 0 then
    let cell_x = x / 2 in
    let cell_y = y / 4 in
    if cell_y < t.height && cell_x < t.width then
      let dot_x = x mod 2 in
      let dot_y = y mod 4 in
      let bit = dot_to_bit ~dot_x ~dot_y in
      t.cells.(cell_y).(cell_x) land bit <> 0
    else false
  else false

let clear t =
  for y = 0 to t.height - 1 do
    for x = 0 to t.width - 1 do
      t.cells.(y).(x) <- 0
    done
  done

let draw_line t ~x0 ~y0 ~x1 ~y1 =
  let dx = abs (x1 - x0) in
  let dy = abs (y1 - y0) in
  let sx = if x0 < x1 then 1 else -1 in
  let sy = if y0 < y1 then 1 else -1 in
  let rec loop x y err =
    set_dot t ~x ~y ;
    if x = x1 && y = y1 then ()
    else
      let e2 = 2 * err in
      let x', err' = if e2 > -dy then (x + sx, err - dy) else (x, err) in
      let y', err'' = if e2 < dx then (y + sy, err' + dx) else (y, err') in
      loop x' y' err''
  in
  loop x0 y0 (dx - dy)

let render t =
  let buf = Buffer.create (t.height * ((t.width * 3) + 1)) in
  (* Each braille char is 3 bytes in UTF-8, plus newlines *)
  for y = 0 to t.height - 1 do
    if y > 0 then Buffer.add_char buf '\n' ;
    for x = 0 to t.width - 1 do
      (* Use precomputed UTF-8 encoded glyph for significant speedup *)
      Buffer.add_string buf braille_glyphs.(t.cells.(y).(x))
    done
  done ;
  Buffer.contents buf

let render_with t ~f =
  let buf = Buffer.create (t.height * ((t.width * 3) + 1)) in
  for y = 0 to t.height - 1 do
    if y > 0 then Buffer.add_char buf '\n' ;
    for x = 0 to t.width - 1 do
      let raw = braille_glyphs.(t.cells.(y).(x)) in
      Buffer.add_string buf (f ~x ~y raw)
    done
  done ;
  Buffer.contents buf

let add_cell_bits t ~cell_x ~cell_y bits =
  if cell_x >= 0 && cell_x < t.width && cell_y >= 0 && cell_y < t.height then
    t.cells.(cell_y).(cell_x) <- t.cells.(cell_y).(cell_x) lor bits
