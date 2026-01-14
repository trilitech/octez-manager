(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)
module Helpers = Miaou_helpers.Helpers
open Widgets

type t = {
  title : string option;
  key_width : int option;
  items : (string * string) list;
}

let create ?title ?key_width ?(items = []) () = {title; key_width; items}

let set_items t items = {t with items}

let compute_key_width ?(max_width = 30) items =
  let max_k =
    List.fold_left (fun acc (k, _) -> max acc (visible_chars_count k)) 0 items
  in
  min max_width max_k

let trunc_visible n s =
  let v = visible_chars_count s in
  if v <= n then s
  else
    let byte_idx = visible_byte_index_of_pos s (max 0 (n - 1)) in
    String.sub s 0 byte_idx ^ "…"

let pad s w =
  if visible_chars_count s >= w then trunc_visible w s
  else s ^ String.make (w - visible_chars_count s) ' '

let render ?cols ?(wrap = true) t ~focus:_ =
  let key_w =
    match t.key_width with
    | Some w -> w
    | None -> compute_key_width ~max_width:30 t.items
  in
  let cols = match cols with Some c -> c | None -> 80 in
  let val_width = max 1 (cols - key_w - 2) in
  let render_item (k, v) =
    let key = pad k key_w in
    if not wrap then [key ^ "  " ^ trunc_visible val_width v]
    else
      let wrapped = wrap_text ~width:val_width v in
      match wrapped with
      | [] -> [key ^ "  "]
      | first :: rest ->
          let first_line = key ^ "  " ^ pad first val_width in
          let indent = String.make (key_w + 2) ' ' in
          let tails = List.map (fun l -> indent ^ pad l val_width) rest in
          first_line :: tails
  in
  let lines = List.concat_map render_item t.items in
  match t.title with
  | Some title ->
      let body = Helpers.concat_lines lines in
      let buf = Buffer.create (String.length title + String.length body + 1) in
      Buffer.add_string buf (titleize title) ;
      Buffer.add_char buf '\n' ;
      Buffer.add_string buf body ;
      Buffer.contents buf
  | None -> Helpers.concat_lines lines
