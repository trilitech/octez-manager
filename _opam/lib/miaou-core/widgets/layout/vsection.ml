(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)
[@@@warning "-32-34-37-69"]

module Helpers = Miaou_helpers.Helpers

let render ~size ~(header : string list) ~(content_footer : string list)
    ~(child : LTerm_geom.size -> string) : string =
  let rows = size.LTerm_geom.rows in
  let cols = size.LTerm_geom.cols in
  let sep =
    Miaou_widgets_display.Widgets.fg
      238
      (Miaou_widgets_display.Widgets.hr ~width:cols ())
  in
  let header_rows = List.length header in
  (* two separators around child *)
  let footer_rows = List.length content_footer in
  let seps = 2 in
  let inner_rows =
    let avail = rows - header_rows - footer_rows - seps in
    max 1 avail
  in
  let inner_size = {size with LTerm_geom.rows = inner_rows} in
  let child_out_raw = child inner_size in
  let child_lines = String.split_on_char '\n' child_out_raw in
  let cl = List.length child_lines in
  let child_lines_adjusted =
    if cl = inner_rows then child_lines
    else if cl < inner_rows then
      (* Pad with empty lines to consume full inner height. *)
      child_lines @ List.init (inner_rows - cl) (fun _ -> "")
    else
      (* Truncate extra lines to avoid spilling beyond allocated inner height. *)
      let rec take n xs =
        if n <= 0 then []
        else match xs with [] -> [] | x :: tl -> x :: take (n - 1) tl
      in
      take inner_rows child_lines
  in
  let child_out = Helpers.concat_lines child_lines_adjusted in
  let parts = header @ [sep] @ [child_out] @ [sep] @ content_footer in
  Helpers.concat_lines parts
