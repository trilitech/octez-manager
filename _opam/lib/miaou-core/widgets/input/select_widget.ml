(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)
(* Select widget: supports both monomorphic string items and a polymorphic API *)
[@@@warning "-32-34-37-69"]

module Helpers = Miaou_helpers.Helpers

type outer_t = {
  title : string;
  items : string list;
  cursor : int;
  cancelled : bool;
}

let clamp lo hi x = max lo (min hi x)

let move_cursor ~total ~cursor ~delta =
  let max_idx = max 0 (total - 1) in
  clamp 0 max_idx (cursor + delta)

let page_move ~total ~cursor ~page_size ~dir =
  let delta = match dir with `Up -> -page_size | `Down -> page_size in
  move_cursor ~total ~cursor ~delta

let create_inner ?(cursor = 0) ~title ~items () =
  {
    title;
    items;
    cursor = clamp 0 (max 0 (List.length items - 1)) cursor;
    cancelled = false;
  }

let open_centered_inner ?(cursor = 0) ~title ~items () =
  create_inner ~cursor ~title ~items ()

let create_sectioned_inner ?cursor_label ~title
    ~(sections : (string * string list) list) () =
  let sections = List.filter (fun (_h, lst) -> lst <> []) sections in
  let items = List.concat (List.map snd sections) in
  let cursor =
    match cursor_label with
    | None -> 0
    | Some lbl ->
        let rec find i = function
          | [] -> 0
          | x :: xs -> if x = lbl then i else find (i + 1) xs
        in
        find 0 items
  in
  create_inner ~cursor ~title ~items ()

let open_centered_sectioned_inner ?cursor_label ~title ~sections () =
  create_sectioned_inner ?cursor_label ~title ~sections ()

let is_cancelled_inner w = w.cancelled

let reset_cancelled_inner w = {w with cancelled = false}

let value_inner w = List.nth w.items w.cursor

let get_selection_inner w = value_inner w

let handle_key_inner w ~key =
  let total = List.length w.items in
  match key with
  | "Up" -> {w with cursor = move_cursor ~total ~cursor:w.cursor ~delta:(-1)}
  | "Down" -> {w with cursor = move_cursor ~total ~cursor:w.cursor ~delta:1}
  | "PageUp" ->
      {w with cursor = page_move ~total ~cursor:w.cursor ~page_size:8 ~dir:`Up}
  | "PageDown" ->
      {
        w with
        cursor = page_move ~total ~cursor:w.cursor ~page_size:8 ~dir:`Down;
      }
  | "Home" -> {w with cursor = 0}
  | "End" -> {w with cursor = max 0 (total - 1)}
  | "Esc" -> {w with cancelled = true}
  | _ -> w

let render_inner
    ?(backend : Miaou_widgets_display.Widgets.backend =
      Miaou_widgets_display.Widgets.get_backend ()) w ~focus:_
    ~(size : LTerm_geom.size) =
  let open Miaou_widgets_display.Widgets in
  let total = List.length w.items in
  (* Available vertical space after reserving header + footer lines. *)
  let rows_total = size.rows in
  (* Usable rows for items is (modal height - 4) now: header + subtle footer. *)
  let vertical_space =
    let v = rows_total - 4 in
    if v < 6 then 6 else if v > 40 then 40 else v
  in
  (* Width handling: truncate long labels to available width with ellipsis. *)
  let max_width = max 10 (size.cols - 2) in
  let truncate s =
    let len = visible_chars_count s in
    if len <= max_width then s
    else if max_width <= 1 then String.make max_width '.'
    else
      let cut = max 0 (max_width - 1) in
      String.sub s 0 cut ^ "."
  in
  let show_all = total <= vertical_space in
  let max_shown = if show_all then total else vertical_space in
  let start =
    if show_all then 0
    else
      let half = max_shown / 2 in
      let s = w.cursor - half in
      let s = if s < 0 then 0 else s in
      let s = if s + max_shown > total then total - max_shown else s in
      s
  in
  let slice =
    w.items
    |> List.mapi (fun i v -> (i, v))
    |> List.filter (fun (i, _) -> i >= start && i < start + max_shown)
  in
  (* Avoid padding items to the terminal width here. Padding to a large
     max_width (derived from terminal cols) caused the modal renderer to
     later wrap those long lines to the modal content width, producing
     artificial blank lines between entries and breaking scrolling in
     nested/modal contexts. Keep truncation but don't pad; the modal box
     will handle horizontal padding/clipping. *)
  let pad_to_width s = s in
  let body_core =
    List.map
      (fun (i, lbl) ->
        let lbl = truncate lbl |> pad_to_width in
        if i = w.cursor then bg 24 (fg 15 lbl) else lbl)
      slice
  in
  let top_indicator =
    if start > 0 then [dim (glyph_up ~backend () ^ " more")] else []
  in
  let bottom_indicator =
    if start + max_shown < total then [dim (glyph_down ~backend () ^ " more")]
    else []
  in
  let range_hint =
    if show_all then []
    else
      [
        dim
          (Printf.sprintf
             "Items %d-%d of %d"
             (start + 1)
             (min (start + max_shown) total)
             total);
      ]
  in
  let up = glyph_up ~backend () in
  let down = glyph_down ~backend () in
  let header =
    [
      w.title;
      dim
        (Printf.sprintf
           "%s/%s move · PgUp/PgDn page · Home/End jump · Enter confirm · Esc \
            cancel"
           up
           down);
    ]
  in
  Helpers.concat_lines
    (header @ top_indicator @ body_core @ bottom_indicator @ range_hint)

(* Polymorphic-by-default API *)

type 'a t = {
  title : string;
  items : 'a list;
  to_string : 'a -> string;
  inner : outer_t;
  max_visible : int option;
}

let open_centered ?(cursor = 0) ?max_visible ~title ~items ~to_string () : 'a t
    =
  let labels = List.map to_string items in
  let inner = open_centered_inner ~cursor ~title ~items:labels () in
  {title; items; to_string; inner; max_visible}

let open_centered_sectioned ?cursor_label ?max_visible ~title ~sections
    ~to_string () : 'a t =
  let sections_str =
    List.map (fun (h, lst) -> (h, List.map to_string lst)) sections
  in
  let inner =
    create_sectioned_inner ?cursor_label ~title ~sections:sections_str ()
  in
  let items = List.concat (List.map snd sections) in
  {title; items; to_string; inner; max_visible}

(* Size-aware rendering API. New callers may use [render_with_size]. *)
let render_with_size
    ?(backend : Miaou_widgets_display.Widgets.backend =
      Miaou_widgets_display.Widgets.get_backend ()) (w : 'a t) ~focus
    ~(size : LTerm_geom.size) =
  let size =
    match w.max_visible with
    | None -> size
    | Some mv ->
        (* Adjust virtual size rows so vertical_space logic clamps to mv. We add
					 5 back (header+footer reserve) to match internal subtraction. *)
        let rows =
          let desired = mv + 5 in
          if size.rows <= desired then size.rows else desired
        in
        {size with rows}
  in
  render_inner ~backend w.inner ~focus ~size

(* Backwards-compatible [render] which uses a sensible default terminal size
	 when no size is provided by the caller. *)
let render_for_backend backend (w : 'a t) ~focus =
  let default_size : LTerm_geom.size = {rows = 24; cols = 80} in
  render_inner ~backend w.inner ~focus ~size:default_size

let render
    ?(backend : Miaou_widgets_display.Widgets.backend =
      Miaou_widgets_display.Widgets.get_backend ()) (w : 'a t) ~focus =
  render_for_backend backend w ~focus

let handle_key_with_size (w : 'a t) ~key ~size:_ : 'a t =
  (* handle_key_inner is size-agnostic currently; keep behaviour identical
		while providing a size-aware API for future use. *)
  let inner' = handle_key_inner w.inner ~key in
  if inner' == w.inner then w else {w with inner = inner'}

let handle_key (w : 'a t) ~key : 'a t =
  let default_size : LTerm_geom.size = {rows = 24; cols = 80} in
  handle_key_with_size w ~key ~size:default_size

let get_selection (w : 'a t) : 'a option =
  match w.items with [] -> None | _ -> Some (List.nth w.items w.inner.cursor)

let is_cancelled (w : 'a t) = w.inner.cancelled

let reset_cancelled (w : 'a t) =
  {w with inner = {w.inner with cancelled = false}}

(* Convenience: label string for current selection *)
let value (w : 'a t) : string = List.nth w.inner.items w.inner.cursor
