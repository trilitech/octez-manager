(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type style = {
  fg : int;
  bg : int;
  bold : bool;
  dim : bool;
  underline : bool;
  reverse : bool;
}

type t = {mutable char : string; mutable style : style}

let default_style =
  {
    fg = -1;
    bg = -1;
    bold = false;
    dim = false;
    underline = false;
    reverse = false;
  }

let empty () = {char = " "; style = default_style}

let create ~char ~style = {char; style}

let copy cell = {char = cell.char; style = cell.style}

let reset cell =
  cell.char <- " " ;
  cell.style <- default_style

(* Invalidate a cell so it never equals any valid cell - used for force redraw *)
let invalidate cell =
  cell.char <- "\x00" ;
  cell.style <- default_style

let style_equal a b =
  a.fg = b.fg && a.bg = b.bg && a.bold = b.bold && a.dim = b.dim
  && a.underline = b.underline && a.reverse = b.reverse

let equal a b = a.char = b.char && style_equal a.style b.style

let is_empty cell = cell.char = " " && style_equal cell.style default_style
