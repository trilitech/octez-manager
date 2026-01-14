(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

[@@@warning "-32-34-37-69"]

module W = Miaou_widgets_display.Widgets
module H = Miaou_helpers.Helpers
module Helpers = Miaou_helpers.Helpers

type direction = Row | Column

type align_items = Start | Center | End | Stretch

type justify = Start | Center | End | Space_between | Space_around

type spacing = {h : int; v : int}

type padding = {left : int; right : int; top : int; bottom : int}

type basis = Auto | Px of int | Ratio of float | Percent of float | Fill

type size_hint = {width : int option; height : int option}

type child = {
  render : size:LTerm_geom.size -> string;
  basis : basis;
  cross : size_hint option;
}

type t = {
  direction : direction;
  align_items : align_items;
  justify : justify;
  gap : spacing;
  padding : padding;
  children : child list;
}

let default_padding = {left = 0; right = 0; top = 0; bottom = 0}

let default_gap = {h = 0; v = 0}

let create ?(direction = Row) ?(align_items : align_items = Start)
    ?(justify : justify = Start) ?(gap = default_gap)
    ?(padding = default_padding) children =
  {direction; align_items; justify; gap; padding; children}

let clamp v ~min_v ~max_v = max min_v (min max_v v)

let truncate_visible s width =
  let idx = H.visible_byte_index_of_pos s width in
  String.sub s 0 idx

let pad_lines lines ~width =
  List.map
    (fun line ->
      let vis = W.visible_chars_count line in
      if vis >= width then truncate_visible line width
      else line ^ String.make (width - vis) ' ')
    lines

let split_lines s = String.split_on_char '\n' s

let rec take n lst =
  if n <= 0 then []
  else match lst with [] -> [] | x :: xs -> x :: take (n - 1) xs

let pad_block ?(align : align_items = Start) lines ~width ~height =
  let lines =
    lines
    |> List.map (fun line ->
        let vis = W.visible_chars_count line in
        if vis > width then truncate_visible line width else line)
    |> ( function ls when align = Stretch -> pad_lines ls ~width | ls -> ls )
    |> take height
  in
  let missing = max 0 (height - List.length lines) in
  let blanks n = List.init n (fun _ -> String.make width ' ') in
  match align with
  | Start -> lines @ blanks missing
  | End -> blanks missing @ lines
  | Center ->
      let top = missing / 2 in
      blanks top @ lines @ blanks (missing - top)
  | Stretch ->
      (* For now, stretch behaves like start; future iterations could distribute extra lines. *)
      lines @ blanks missing

let compute_sizes direction padding gap size children =
  let available_main =
    match direction with
    | Row ->
        size.LTerm_geom.cols - padding.left - padding.right
        - max 0 ((List.length children - 1) * gap.h)
    | Column ->
        size.LTerm_geom.rows - padding.top - padding.bottom
        - max 0 ((List.length children - 1) * gap.v)
  in
  let fixed, ratios, percents, fills =
    List.fold_left
      (fun (fix, rat, per, fil) c ->
        match c.basis with
        | Px n -> (fix + n, rat, per, fil)
        | Ratio r -> (fix, rat +. r, per, fil)
        | Percent p -> (fix, rat, per +. p, fil)
        | Fill -> (fix, rat, per, fil + 1)
        | Auto -> (fix, rat, per, fil + 1))
      (0, 0., 0., 0)
      children
  in
  let remaining =
    max
      0
      (available_main - fixed
      - int_of_float (percents *. float available_main /. 100.))
  in
  let alloc_child c =
    match c.basis with
    | Px n -> n
    | Ratio r when ratios > 0. -> int_of_float (r /. ratios *. float remaining)
    | Ratio _ -> 0
    | Percent p -> int_of_float (p /. 100. *. float available_main)
    | Fill | Auto -> if fills > 0 then remaining / max 1 fills else remaining
  in
  List.map alloc_child children

let distribute direction padding justify gap child_sizes size =
  let children = List.length child_sizes in
  let inner =
    match direction with
    | Row -> size.LTerm_geom.cols - padding.left - padding.right
    | Column -> size.LTerm_geom.rows - padding.top - padding.bottom
  in
  let used =
    List.fold_left ( + ) 0 child_sizes
    + max 0 ((children - 1) * if direction = Row then gap.h else gap.v)
  in
  let extra = max 0 (inner - used) in
  match justify with
  | Start -> (0, gap, extra)
  | End -> (extra, gap, 0)
  | Center ->
      let lead = extra / 2 in
      (lead, gap, extra - lead)
  | Space_between ->
      if children <= 1 then (0, gap, extra)
      else
        let between =
          if direction = Row then
            {gap with h = gap.h + (extra / (children - 1))}
          else {gap with v = gap.v + (extra / (children - 1))}
        in
        (0, between, extra mod max 1 (children - 1))
  | Space_around ->
      let lead = extra / (children + 1) in
      let between =
        if direction = Row then {gap with h = gap.h + lead}
        else {gap with v = gap.v + lead}
      in
      (lead, between, extra - (lead * (children + 1)))

let render_row t ~size =
  let child_sizes = compute_sizes Row t.padding t.gap size t.children in
  let leading, gap, trailing_extra =
    distribute Row t.padding t.justify t.gap child_sizes size
  in
  let max_h = size.LTerm_geom.rows - t.padding.top - t.padding.bottom in
  let rendered =
    List.map2
      (fun c w ->
        let child_size = {LTerm_geom.rows = max_h; cols = max 0 w} in
        let raw = split_lines (c.render ~size:child_size) in
        let block = pad_block ~align:t.align_items raw ~width:w ~height:max_h in
        block)
      t.children
      child_sizes
  in
  let inner_width = size.LTerm_geom.cols - t.padding.left - t.padding.right in
  let lines =
    List.init max_h (fun row ->
        let buf = Buffer.create (size.LTerm_geom.cols + 2) in
        Buffer.add_string buf (String.make (t.padding.left + leading) ' ') ;
        let rec emit idx blocks =
          match blocks with
          | [] -> ()
          | blk :: rest ->
              let line =
                match List.nth_opt blk row with Some s -> s | None -> ""
              in
              if idx > 0 then Buffer.add_string buf (String.make gap.h ' ') ;
              Buffer.add_string buf line ;
              emit (idx + 1) rest
        in
        emit 0 rendered ;
        let used =
          List.fold_left ( + ) 0 child_sizes
          + max 0 ((List.length child_sizes - 1) * gap.h)
        in
        let consumed = leading + used in
        let remaining = max 0 (inner_width - consumed - trailing_extra) in
        Buffer.add_string
          buf
          (String.make (remaining + t.padding.right + trailing_extra) ' ') ;
        Buffer.contents buf)
  in
  lines

let render_column t ~size =
  let child_sizes = compute_sizes Column t.padding t.gap size t.children in
  let max_w = size.LTerm_geom.cols - t.padding.left - t.padding.right in
  let leading, gap, trailing_extra =
    distribute Column t.padding t.justify t.gap child_sizes size
  in
  let blocks =
    List.map2
      (fun c h ->
        let child_size = {LTerm_geom.rows = max 0 h; cols = max_w} in
        let blk =
          pad_block
            ~align:t.align_items
            (split_lines (c.render ~size:child_size))
            ~width:max_w
            ~height:h
        in
        blk)
      t.children
      child_sizes
  in
  let gap_lines =
    if gap.v <= 0 then [] else List.init gap.v (fun _ -> String.make max_w ' ')
  in
  let rec interleave = function
    | [] -> []
    | [b] -> b
    | b :: rest -> b @ gap_lines @ interleave rest
  in
  let rows = size.LTerm_geom.rows - t.padding.top - t.padding.bottom in
  let spaced =
    interleave blocks |> fun lines ->
    let top = List.init leading (fun _ -> String.make max_w ' ') in
    let bottom = List.init trailing_extra (fun _ -> String.make max_w ' ') in
    top @ lines @ bottom
  in
  let all_lines = take rows spaced in
  let with_pad =
    List.map
      (fun line ->
        let vis = W.visible_chars_count line in
        let trimmed =
          if vis > max_w then truncate_visible line max_w else line
        in
        let trimmed_vis = if vis > max_w then max_w else vis in
        let free = max 0 (max_w - trimmed_vis) in
        let left_pad, right_pad =
          match t.align_items with
          | Start | Stretch -> (0, free)
          | End -> (free, 0)
          | Center ->
              let l = free / 2 in
              (l, free - l)
        in
        String.make (t.padding.left + left_pad) ' '
        ^ trimmed
        ^ String.make (t.padding.right + right_pad) ' ')
      all_lines
  in
  with_pad

let render t ~size =
  let lines =
    match t.direction with
    | Row -> render_row t ~size
    | Column -> render_column t ~size
  in
  Helpers.concat_lines lines
