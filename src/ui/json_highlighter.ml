(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

module Widgets = Miaou_widgets_display.Widgets
module Style = Miaou_style.Style
module Style_context = Miaou_style.Style_context

type options = {indent : int; max_depth : int; colors : bool}

let default_options = {indent = 2; max_depth = 20; colors = true}

let make_indent n = String.make n ' '

let render_style ~colors style text =
  if not colors then text
  else
    let resolved = Style.to_resolved style in
    if resolved.r_fg >= 0 then Style.render style text
    else Widgets.themed_text text

let rec format_value ~options ~depth buf (j : Yojson.Safe.t) =
  if depth > options.max_depth then Buffer.add_string buf "..."
  else
    match j with
    | `Null ->
        Buffer.add_string
          buf
          (render_style
             ~colors:options.colors
             (Style_context.text_muted ())
             "null")
    | `Bool b ->
        let s = if b then "true" else "false" in
        Buffer.add_string
          buf
          (render_style ~colors:options.colors (Style_context.success ()) s)
    | `Int i ->
        Buffer.add_string
          buf
          (render_style
             ~colors:options.colors
             (Style_context.info ())
             (string_of_int i))
    | `Float f ->
        let s =
          if Float.is_integer f then Printf.sprintf "%.0f" f
          else Printf.sprintf "%g" f
        in
        Buffer.add_string
          buf
          (render_style ~colors:options.colors (Style_context.info ()) s)
    | `Intlit s ->
        Buffer.add_string
          buf
          (render_style ~colors:options.colors (Style_context.info ()) s)
    | `String s ->
        Buffer.add_string
          buf
          (render_style
             ~colors:options.colors
             (Style_context.warning ())
             ("\"" ^ String.escaped s ^ "\""))
    | `List [] -> Buffer.add_string buf "[]"
    | `List items ->
        Buffer.add_string buf "[\n" ;
        let indent_str = make_indent ((depth + 1) * options.indent) in
        List.iteri
          (fun i item ->
            Buffer.add_string buf indent_str ;
            format_value ~options ~depth:(depth + 1) buf item ;
            if i < List.length items - 1 then Buffer.add_string buf "," ;
            Buffer.add_char buf '\n')
          items ;
        Buffer.add_string buf (make_indent (depth * options.indent)) ;
        Buffer.add_char buf ']'
    | `Assoc [] -> Buffer.add_string buf "{}"
    | `Assoc pairs ->
        Buffer.add_string buf "{\n" ;
        let indent_str = make_indent ((depth + 1) * options.indent) in
        List.iteri
          (fun i (key, value) ->
            Buffer.add_string buf indent_str ;
            Buffer.add_string
              buf
              (render_style
                 ~colors:options.colors
                 (Style_context.text_emphasized ())
                 ("\"" ^ String.escaped key ^ "\"")) ;
            Buffer.add_string buf ": " ;
            format_value ~options ~depth:(depth + 1) buf value ;
            if i < List.length pairs - 1 then Buffer.add_string buf "," ;
            Buffer.add_char buf '\n')
          pairs ;
        Buffer.add_string buf (make_indent (depth * options.indent)) ;
        Buffer.add_char buf '}'
    | _ -> Buffer.add_string buf "null"

let highlight ?(options = default_options) json_string =
  try
    let j = Yojson.Safe.from_string json_string in
    let buf = Buffer.create 1024 in
    format_value ~options ~depth:0 buf j ;
    Ok (Buffer.contents buf)
  with
  | Yojson.Json_error msg -> Error ("JSON parse error: " ^ msg)
  | exn -> Error ("Unexpected error: " ^ Printexc.to_string exn)

let strip_colors text =
  (* Remove ANSI escape sequences: ESC [ ... m *)
  let re = Str.regexp "\027\\[[0-9;]*m" in
  Str.global_replace re "" text
