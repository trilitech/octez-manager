(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let is_utf8_lead b = Char.code b land 0xC0 <> 0x80

(* Check if s[i] starts a CSI sequence (ESC [ ...). *)
let is_esc_start s i =
  i + 1 < String.length s && s.[i] = '\027' && s.[i + 1] = '['

(* Skip characters until 'm' (SGR terminator) is found. *)
let rec skip_ansi_until_m s i =
  if i >= String.length s then i
  else if s.[i] = 'm' then i + 1
  else skip_ansi_until_m s (i + 1)

let visible_chars_count s =
  let rec loop i cnt =
    if i >= String.length s then cnt
    else if is_esc_start s i then
      let j = skip_ansi_until_m s (i + 2) in
      loop j cnt
    else
      let cnt' = if is_utf8_lead s.[i] then cnt + 1 else cnt in
      loop (i + 1) cnt'
  in
  loop 0 0

let visible_byte_index_of_pos s pos =
  let len = String.length s in
  let rec next_char_start i =
    if i >= len then len
    else if Char.code s.[i] land 0xC0 <> 0x80 then i
    else next_char_start (i + 1)
  in
  let rec advance_one_char i =
    if i >= len then len
    else if is_esc_start s i then
      let j = skip_ansi_until_m s (i + 2) in
      advance_one_char j
    else
      let i0 = next_char_start i in
      let j = ref (i0 + 1) in
      while !j < len && Char.code s.[!j] land 0xC0 = 0x80 do
        incr j
      done ;
      !j
  in
  let rec loop i cnt =
    if cnt = pos then i
    else if i >= len then len
    else
      let j = advance_one_char i in
      loop j (cnt + 1)
  in
  loop 0 0

let has_trailing_reset s =
  let l = String.length s in
  l >= 4 && String.sub s (l - 4) 4 = "\027[0m"

let insert_before_reset s tail =
  let l = String.length s in
  if has_trailing_reset s then
    let prefix = String.sub s 0 (l - 4) in
    prefix ^ tail ^ "\027[0m"
  else s ^ tail

let pad_to_width s target_width pad_char =
  let v = visible_chars_count s in
  if v >= target_width then s
  else
    let needed = target_width - v in
    let l = String.length s in
    if has_trailing_reset s then (
      let buf = Buffer.create (l + needed + 4) in
      Buffer.add_substring buf s 0 (l - 4) ;
      for _ = 1 to needed do
        Buffer.add_char buf pad_char
      done ;
      Buffer.add_string buf "\027[0m" ;
      Buffer.contents buf)
    else
      let buf = Buffer.create (l + needed) in
      Buffer.add_string buf s ;
      for _ = 1 to needed do
        Buffer.add_char buf pad_char
      done ;
      Buffer.contents buf

let concat_lines lines =
  match lines with
  | [] -> ""
  | hd :: tl ->
      let buf =
        let est =
          List.fold_left (fun acc l -> acc + String.length l + 1) 0 lines
        in
        Buffer.create est
      in
      Buffer.add_string buf hd ;
      List.iter
        (fun l ->
          Buffer.add_char buf '\n' ;
          Buffer.add_string buf l)
        tl ;
      Buffer.contents buf

let concat_with_sep sep parts =
  match parts with
  | [] -> ""
  | hd :: tl ->
      let buf =
        let est =
          List.fold_left (fun acc p -> acc + String.length p) 0 parts
          + (String.length sep * max 0 (List.length parts - 1))
        in
        Buffer.create est
      in
      Buffer.add_string buf hd ;
      List.iter
        (fun p ->
          Buffer.add_string buf sep ;
          Buffer.add_string buf p)
        tl ;
      Buffer.contents buf
