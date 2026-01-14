(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module Sdl = Tsdl.Sdl

type color = {r : int; g : int; b : int; a : int}

type ansi_state = {fg : color; bg : color}

let ansi_palette =
  [|
    {r = 0; g = 0; b = 0; a = 255};
    {r = 205; g = 49; b = 49; a = 255};
    {r = 13; g = 188; b = 121; a = 255};
    {r = 229; g = 229; b = 16; a = 255};
    {r = 36; g = 114; b = 200; a = 255};
    {r = 188; g = 63; b = 188; a = 255};
    {r = 17; g = 168; b = 205; a = 255};
    {r = 229; g = 229; b = 229; a = 255};
  |]

let ansi_bright_palette =
  [|
    {r = 102; g = 102; b = 102; a = 255};
    {r = 241; g = 76; b = 76; a = 255};
    {r = 35; g = 209; b = 139; a = 255};
    {r = 245; g = 245; b = 67; a = 255};
    {r = 59; g = 142; b = 234; a = 255};
    {r = 214; g = 112; b = 214; a = 255};
    {r = 41; g = 184; b = 219; a = 255};
    {r = 229; g = 229; b = 229; a = 255};
  |]

let color_to_sdl ({r; g; b; a} : color) : Sdl.color =
  Sdl.Color.create ~r ~g ~b ~a

let clamp lo hi v = if v < lo then lo else if v > hi then hi else v

let color256 idx =
  let idx = clamp 0 255 idx in
  if idx < 16 then
    let base = ansi_palette in
    let bright = ansi_bright_palette in
    if idx < 8 then base.(idx) else bright.(idx - 8)
  else if idx < 232 then
    let n = idx - 16 in
    let r = n / 36 in
    let g = n / 6 mod 6 in
    let b = n mod 6 in
    let to_int c = if c = 0 then 0 else 55 + (c * 40) in
    {r = to_int r; g = to_int g; b = to_int b; a = 255}
  else
    let level = 8 + ((idx - 232) * 10) in
    {r = level; g = level; b = level; a = 255}

let apply_sgr_code ~(default : ansi_state) state code =
  match code with
  | 0 -> {fg = default.fg; bg = default.bg}
  | 39 -> {state with fg = default.fg}
  | 49 -> {state with bg = default.bg}
  | c when c >= 30 && c <= 37 ->
      let idx = c - 30 in
      {state with fg = ansi_palette.(idx)}
  | c when c >= 90 && c <= 97 ->
      let idx = c - 90 in
      {state with fg = ansi_bright_palette.(idx)}
  | c when c >= 40 && c <= 47 ->
      let idx = c - 40 in
      {state with bg = ansi_palette.(idx)}
  | c when c >= 100 && c <= 107 ->
      let idx = c - 100 in
      {state with bg = ansi_bright_palette.(idx)}
  | _ -> state

let apply_extended_sgr ~(default : ansi_state) state codes =
  match codes with
  | 38 :: 5 :: n :: tl ->
      let fg = color256 n in
      List.fold_left (apply_sgr_code ~default) {state with fg} tl
  | 48 :: 5 :: n :: tl ->
      let bg = color256 n in
      List.fold_left (apply_sgr_code ~default) {state with bg} tl
  | lst -> List.fold_left (apply_sgr_code ~default) state lst

(* Get the byte length of a UTF-8 character from its first byte *)
let utf8_char_len byte =
  let b = Char.code byte in
  if b land 0x80 = 0 then 1 (* 0xxxxxxx - ASCII *)
  else if b land 0xE0 = 0xC0 then 2 (* 110xxxxx *)
  else if b land 0xF0 = 0xE0 then 3 (* 1110xxxx *)
  else if b land 0xF8 = 0xF0 then 4 (* 11110xxx *)
  else 1 (* Invalid UTF-8, treat as single byte *)

let parse_ansi_segments ~(default : ansi_state) (s : string) =
  let len = String.length s in
  let buf = Buffer.create 64 in
  let add_chunk acc state =
    if Buffer.length buf = 0 then acc
    else
      let chunk = Buffer.contents buf in
      Buffer.clear buf ;
      (state, chunk) :: acc
  in
  let rec loop i acc state =
    if i >= len then List.rev (add_chunk acc state)
    else
      match s.[i] with
      | '\027' when i + 1 < len && s.[i + 1] = '[' ->
          let j = ref (i + 2) in
          while !j < len && s.[!j] <> 'm' do
            incr j
          done ;
          if !j >= len then (
            Buffer.add_char buf s.[i] ;
            loop (i + 1) acc state)
          else
            let codes_str = String.sub s (i + 2) (!j - (i + 2)) in
            let codes =
              codes_str |> String.split_on_char ';'
              |> List.filter_map (fun c ->
                  match int_of_string_opt (String.trim c) with
                  | Some v -> Some v
                  | None -> None)
            in
            let state' = apply_extended_sgr ~default state codes in
            let acc' = add_chunk acc state in
            loop (!j + 1) acc' state'
      | '\r' -> loop (i + 1) acc state
      | c ->
          (* Handle UTF-8 multi-byte characters properly *)
          let char_len = utf8_char_len c in
          if i + char_len <= len then begin
            Buffer.add_substring buf s i char_len ;
            loop (i + char_len) acc state
          end
          else begin
            (* Incomplete UTF-8 at end, skip *)
            loop (i + 1) acc state
          end
  in
  loop 0 [] {fg = default.fg; bg = default.bg}

let strip_ansi_to_text ~default s =
  let segments = parse_ansi_segments ~default s in
  let buf = Buffer.create (String.length s) in
  List.iter (fun (_, text) -> Buffer.add_string buf text) segments ;
  Buffer.contents buf
