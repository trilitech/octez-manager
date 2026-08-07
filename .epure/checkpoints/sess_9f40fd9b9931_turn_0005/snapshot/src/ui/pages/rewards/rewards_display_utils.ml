(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Display utilities for rewards page tables.

    Provides display-width-aware padding that correctly handles:
    - Multi-byte UTF-8 characters (ꜩ, —, ▸)
    - ANSI escape codes from themed_* functions (zero display width)
*)

(** Strip ANSI escape sequences from a string. *)
let strip_ansi s =
  let len = String.length s in
  let buf = Buffer.create len in
  let rec skip_escape i =
    if i >= len then len
    else
      match s.[i] with
      | 'A' .. 'Z' | 'a' .. 'z' -> i + 1
      | _ -> skip_escape (i + 1)
  in
  let rec loop i =
    if i >= len then ()
    else if s.[i] = '\027' then
      let next =
        if i + 1 < len && s.[i + 1] = '[' then skip_escape (i + 2)
        else skip_escape (i + 1)
      in
      loop next
    else (
      Buffer.add_char buf s.[i] ;
      loop (i + 1))
  in
  loop 0 ;
  Buffer.contents buf

(** Count display width of a string (Unicode scalar values, not bytes).

    This approximates display width by counting Unicode code points.
    Most characters (including ꜩ, —, ▸) are 1 display column.
    Some CJK characters may be 2 columns, but we don't handle that here. *)
let display_width s =
  let len = String.length s in
  let count = ref 0 in
  let i = ref 0 in
  while !i < len do
    let b = Char.code s.[!i] in
    incr count ;
    if b < 0x80 then incr i
    else if b < 0xE0 then i := !i + 2
    else if b < 0xF0 then i := !i + 3
    else i := !i + 4
  done ;
  !count

(** Pad string to target display width (right-aligned).

    Accounts for ANSI codes and multi-byte UTF-8 characters.
    @param width Target display width in columns
    @param s String to pad (may contain ANSI codes) *)
let pad_right width s =
  let stripped = strip_ansi s in
  let w = display_width stripped in
  let padding = max 0 (width - w) in
  s ^ String.make padding ' '

(** Pad string to target display width (left-aligned).

    Accounts for ANSI codes and multi-byte UTF-8 characters.
    @param width Target display width in columns
    @param s String to pad (may contain ANSI codes) *)
let pad_left width s =
  let stripped = strip_ansi s in
  let w = display_width stripped in
  let padding = max 0 (width - w) in
  String.make padding ' ' ^ s
