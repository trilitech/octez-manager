(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

type parse_state =
  | Normal
  | EscapeStart
  | CSI of int list * int (* accumulated params, current param *)

type t = {mutable style : Matrix_cell.style; mutable state : parse_state}

let create () = {style = Matrix_cell.default_style; state = Normal}

let reset t =
  t.style <- Matrix_cell.default_style ;
  t.state <- Normal

let current_style t = t.style

(* Apply SGR (Select Graphic Rendition) parameters to style *)
let apply_sgr params style =
  let rec process params style =
    match params with
    | [] -> style
    | 0 :: rest ->
        (* Reset all *)
        process rest Matrix_cell.default_style
    | 1 :: rest ->
        (* Bold *)
        process rest {style with bold = true}
    | 2 :: rest ->
        (* Dim *)
        process rest {style with dim = true}
    | 4 :: rest ->
        (* Underline *)
        process rest {style with underline = true}
    | 7 :: rest ->
        (* Reverse *)
        process rest {style with reverse = true}
    | 22 :: rest ->
        (* Normal intensity (reset bold/dim) *)
        process rest {style with bold = false; dim = false}
    | 24 :: rest ->
        (* Underline off *)
        process rest {style with underline = false}
    | 27 :: rest ->
        (* Reverse off *)
        process rest {style with reverse = false}
    (* Basic foreground colors 30-37 *)
    | n :: rest when n >= 30 && n <= 37 -> process rest {style with fg = n - 30}
    | 39 :: rest ->
        (* Default foreground *)
        process rest {style with fg = -1}
    (* Basic background colors 40-47 *)
    | n :: rest when n >= 40 && n <= 47 -> process rest {style with bg = n - 40}
    | 49 :: rest ->
        (* Default background *)
        process rest {style with bg = -1}
    (* Bright foreground colors 90-97 *)
    | n :: rest when n >= 90 && n <= 97 ->
        process rest {style with fg = n - 90 + 8}
    (* Bright background colors 100-107 *)
    | n :: rest when n >= 100 && n <= 107 ->
        process rest {style with bg = n - 100 + 8}
    (* 256-color foreground: 38;5;N *)
    | 38 :: 5 :: n :: rest ->
        let fg = if n >= 0 && n <= 255 then n else -1 in
        process rest {style with fg}
    (* 256-color background: 48;5;N *)
    | 48 :: 5 :: n :: rest ->
        let bg = if n >= 0 && n <= 255 then n else -1 in
        process rest {style with bg}
    (* Unknown - skip *)
    | _ :: rest -> process rest style
  in
  process params style

(* Get UTF-8 character length in bytes *)
let utf8_char_length c =
  let code = Char.code c in
  if code land 0x80 = 0 then 1
  else if code land 0xE0 = 0xC0 then 2
  else if code land 0xF0 = 0xE0 then 3
  else if code land 0xF8 = 0xF0 then 4
  else 1 (* Invalid, treat as single byte *)

(* Extract UTF-8 character from string at position *)
let extract_utf8_char s pos =
  if pos >= String.length s then ("", 0)
  else
    let len = utf8_char_length s.[pos] in
    let len = min len (String.length s - pos) in
    (String.sub s pos len, len)

(* Core parsing function - shared state machine logic.
   Takes a callback ~emit_char that receives (char, style) for each visible character.
   Returns the number of visible characters parsed. *)
let parse_core t ~emit_char input =
  let len = String.length input in
  let count = ref 0 in
  let pos = ref 0 in

  while !pos < len do
    match t.state with
    | Normal -> (
        let c = input.[!pos] in
        match c with
        | '\027' ->
            t.state <- EscapeStart ;
            incr pos
        | '\n' | '\r' ->
            (* Skip newlines in single-line parsing *)
            incr pos
        | _ ->
            (* Regular character - extract full UTF-8 *)
            let char, char_len = extract_utf8_char input !pos in
            if char_len > 0 then begin
              emit_char char t.style ;
              incr count ;
              pos := !pos + char_len
            end
            else incr pos)
    | EscapeStart ->
        if !pos < len then (
          match input.[!pos] with
          | '[' ->
              t.state <- CSI ([], 0) ;
              incr pos
          | _ ->
              (* Not a CSI sequence, back to normal *)
              t.state <- Normal ;
              incr pos)
        else t.state <- Normal
    | CSI (params, current) ->
        if !pos < len then (
          let c = input.[!pos] in
          match c with
          | '0' .. '9' ->
              let digit = Char.code c - Char.code '0' in
              t.state <- CSI (params, (current * 10) + digit) ;
              incr pos
          | ';' ->
              t.state <- CSI (params @ [current], 0) ;
              incr pos
          | 'm' ->
              (* SGR complete *)
              let all_params = params @ [current] in
              t.style <- apply_sgr all_params t.style ;
              t.state <- Normal ;
              incr pos
          | 'A' .. 'Z' | 'a' .. 'l' | 'n' .. 'z' ->
              (* Other CSI sequence terminator - ignore *)
              t.state <- Normal ;
              incr pos
          | _ ->
              (* Unknown, abort CSI parsing *)
              t.state <- Normal ;
              incr pos)
        else t.state <- Normal
  done ;
  !count

(* Parse a single line into a buffer at given row/col *)
let parse_line t buf ~row ~col input =
  let col = ref col in
  let emit_char char style =
    Matrix_buffer.set_char buf ~row ~col:!col ~char ~style ;
    incr col
  in
  let _ = parse_core t ~emit_char input in
  !col

(* Parse into buffer, handling newlines *)
let parse_into t buf ~row ~col input =
  let lines = String.split_on_char '\n' input in
  let row = ref row in
  let col = ref col in
  List.iteri
    (fun i line ->
      if i > 0 then begin
        incr row ;
        col := 0
      end ;
      col := parse_line t buf ~row:!row ~col:!col line)
    lines ;
  (!row, !col)

(* Batch version using batch_ops for thread-safe access *)
let parse_line_batch t (ops : Matrix_buffer.batch_ops) ~row ~col input =
  let col = ref col in
  let emit_char char style =
    ops.set_char ~row ~col:!col ~char ~style ;
    incr col
  in
  let _ = parse_core t ~emit_char input in
  !col

let parse_into_batch t ops ~row ~col input =
  let lines = String.split_on_char '\n' input in
  let row = ref row in
  let col = ref col in
  List.iteri
    (fun i line ->
      if i > 0 then begin
        incr row ;
        col := 0
      end ;
      col := parse_line_batch t ops ~row:!row ~col:!col line)
    lines ;
  (!row, !col)

(* Parse to a list of (char, style) pairs *)
let parse_to_cells t input =
  let results = ref [] in
  let emit_char char style = results := (char, style) :: !results in
  let _ = parse_core t ~emit_char input in
  List.rev !results

(* Calculate visible length (chars excluding ANSI codes) *)
let visible_length input =
  let len = String.length input in
  let count = ref 0 in
  let pos = ref 0 in
  let in_escape = ref false in

  while !pos < len do
    let c = input.[!pos] in
    if !in_escape then begin
      if c = 'm' || (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') then
        in_escape := false ;
      incr pos
    end
    else if c = '\027' then begin
      in_escape := true ;
      incr pos
    end
    else if c = '\n' || c = '\r' then incr pos
    else begin
      let char_len = utf8_char_length c in
      incr count ;
      pos := !pos + char_len
    end
  done ;
  !count
