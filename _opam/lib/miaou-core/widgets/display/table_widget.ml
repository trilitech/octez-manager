(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)
module Helpers = Miaou_helpers.Helpers
module W = Widgets
module Palette = Palette

type glyphs = {
  vline : string;
  hline : string;
  corner_tl : string;
  corner_tr : string;
  corner_bl : string;
  corner_br : string;
  top_sep : string;
  mid_left : string;
  mid_sep : string;
  mid_right : string;
  bottom_sep : string;
}

let glyphs_for_backend ?backend () =
  let use_ascii = W.prefer_ascii ?backend () in
  let glyph_corner_tl = if use_ascii then "+" else "┌" in
  let glyph_corner_tr = if use_ascii then "+" else "┐" in
  let glyph_corner_bl = if use_ascii then "+" else "└" in
  let glyph_corner_br = if use_ascii then "+" else "┘" in
  let glyph_hline = if use_ascii then "-" else "─" in
  let glyph_vline = if use_ascii then "|" else "│" in
  let glyph_top_sep = if use_ascii then "+" else "┬" in
  let glyph_mid_left = if use_ascii then "+" else "├" in
  let glyph_mid_sep = if use_ascii then "+" else "┼" in
  let glyph_mid_right = if use_ascii then "+" else "┤" in
  let glyph_bottom_sep = if use_ascii then "+" else "┴" in
  {
    vline = glyph_vline;
    hline = glyph_hline;
    corner_tl = glyph_corner_tl;
    corner_tr = glyph_corner_tr;
    corner_bl = glyph_corner_bl;
    corner_br = glyph_corner_br;
    top_sep = glyph_top_sep;
    mid_left = glyph_mid_left;
    mid_sep = glyph_mid_sep;
    mid_right = glyph_mid_right;
    bottom_sep = glyph_bottom_sep;
  }

let visible_chars_count = W.visible_chars_count

let visible_byte_index_of_pos = W.visible_byte_index_of_pos

let add_repeat buf s n =
  for _ = 1 to n do
    Buffer.add_string buf s
  done

let pad s w =
  let vis = visible_chars_count s in
  if vis <= w then s ^ String.make (w - vis) ' '
  else
    let byte_idx = visible_byte_index_of_pos s (max 0 (w - 1)) in
    String.sub s 0 byte_idx ^ "…"

let col_widths_total = 80

type selection_mode = Row | Col | Cell | None_mode

type column_opts = {max_width : int option; pad_left : int; pad_right : int}

type render_opts = {
  selection_mode : selection_mode;
  highlight_header : bool;
  sort : (int * bool) option;
}

let default_opts = {selection_mode = Row; highlight_header = false; sort = None}

let render_table_sdl ~cols ~header:(h1, h2, h3) ~rows ~cursor ~sel_col:_ ~opts =
  (* More UI-like SDL style: solid rows and badges. Kept as a separate entrypoint
     from the terminal renderer to avoid imposing styling on all tables. *)
  let open Widgets in
  let total_w =
    match cols with Some c -> max 30 (min c 140) | None -> col_widths_total
  in
  let inner_w = max 10 (total_w - 4) in
  let pad_cell s w =
    let vis = visible_chars_count s in
    if vis <= w then s ^ String.make (max 0 (w - vis)) ' '
    else
      let idx = visible_byte_index_of_pos s (max 0 (w - 1)) in
      String.sub s 0 idx ^ "…"
  in
  let header_list = [h1; h2; h3] in
  let rows_list = List.map (fun (a, b, c) -> [a; b; c]) rows in
  let col_count = List.length header_list in
  let content_widths =
    let all_rows = header_list :: rows_list in
    List.init col_count (fun col ->
        List.fold_left
          (fun acc row ->
            let cell = List.nth_opt row col |> Option.value ~default:"" in
            max acc (visible_chars_count cell))
          0
          all_rows)
  in
  let base_widths =
    List.map (fun w -> min w (inner_w / max 1 col_count)) content_widths
  in
  let total_base = List.fold_left ( + ) 0 base_widths in
  let extra = max 0 (inner_w - total_base) in
  let col_widths =
    List.mapi
      (fun _ w ->
        let add = if col_count = 0 then 0 else extra / col_count in
        w + add + 2)
      base_widths
  in
  let badge color txt = fg color (bold ("● " ^ txt)) in
  let style_status s =
    match String.lowercase_ascii (String.trim s) with
    | "ok" | "ready" -> badge 82 s
    | "warn" | "warning" -> badge 214 s
    | "error" | "fail" -> badge 196 s
    | _ -> badge 75 s
  in
  let header_line =
    let cells =
      List.mapi
        (fun i h ->
          let w = List.nth col_widths i in
          let base = pad_cell (String.uppercase_ascii h) (w - 2) in
          let padded = " " ^ base ^ " " in
          fg 255 (bold padded))
        header_list
    in
    let line = "  " ^ String.concat (fg 60 "│") cells in
    line
  in
  let line_for_row idx row =
    let row_bg =
      match opts.selection_mode with
      | Row when idx = cursor -> bg 24
      | _ -> bg 235
    in
    let cells =
      List.mapi
        (fun i cell ->
          let w = List.nth col_widths i in
          let cell =
            if i = 2 then style_status cell else pad_cell cell (w - 2)
          in
          let padded = " " ^ pad_cell cell (w - 2) ^ " " in
          padded)
        row
    in
    let pointer = if idx = cursor then fg 51 ">" else " " in
    let bar = String.concat (fg 60 "│") cells in
    let with_pointer = pointer ^ " " ^ bar in
    if idx = cursor then row_bg with_pointer else with_pointer
  in
  let body = List.mapi (fun i row -> line_for_row i row) rows_list in
  let header_sep =
    fg 60 (String.make (max 0 (String.length header_line)) '-')
  in
  String.concat "\n" (header_line :: header_sep :: body)

(* Generic table renderer that handles column width calculation, padding,
   borders, and selection highlighting. *)
let render_table_generic_with_opts ?backend ?(wrap = false) ~cols ~header_list
    ~rows_list ~cursor ~sel_col:_ ~opts ?(col_opts = []) () =
  let glyphs = glyphs_for_backend ?backend () in
  let total_w =
    match cols with Some c -> max 20 (min c 240) | None -> col_widths_total
  in
  let inner_w = max 0 (total_w - 4) in
  let col_count = List.length header_list in
  let default_col_opt : column_opts =
    {max_width = None; pad_left = 1; pad_right = 1}
  in
  let col_opts =
    if List.length col_opts = col_count then col_opts
    else List.init col_count (fun _ -> default_col_opt)
  in
  let content_widths =
    let all_rows = header_list :: rows_list in
    List.init col_count (fun col ->
        List.fold_left
          (fun acc row ->
            let cell = List.nth_opt row col |> Option.value ~default:"" in
            max acc (visible_chars_count cell))
          0
          all_rows)
  in
  let col_widths =
    List.mapi
      (fun i w ->
        let copts = List.nth col_opts i in
        let padded = w + copts.pad_left + copts.pad_right in
        match copts.max_width with Some mw -> min padded mw | None -> padded)
      content_widths
  in
  let total_cols_w = List.fold_left ( + ) 0 col_widths in
  let _sep_w = col_count + 1 in
  let extra_space = max 0 (inner_w - total_cols_w) in
  let col_widths =
    if extra_space > 0 then
      let per_col = extra_space / col_count in
      List.map (fun w -> w + per_col) col_widths
    else col_widths
  in
  let rows = rows_list in
  let rows_sorted =
    match opts.sort with
    | None -> rows
    | Some (col, asc) ->
        let key_of row = List.nth_opt row col |> Option.value ~default:"" in
        let cmp a b = String.compare (key_of a) (key_of b) in
        let lst = List.mapi (fun i r -> (i, r)) rows in
        let lst_sorted =
          List.sort (fun (_, a) (_, b) -> if asc then cmp a b else -cmp a b) lst
        in
        List.map snd lst_sorted
  in
  let headers_padded =
    List.mapi
      (fun i h ->
        let w = List.nth col_widths i in
        let copts = List.nth col_opts i in
        let base = pad h (w - copts.pad_left - copts.pad_right) in
        String.make copts.pad_left ' ' ^ base ^ String.make copts.pad_right ' ')
      header_list
  in
  let header_line =
    let line =
      glyphs.vline
      ^ Helpers.concat_with_sep glyphs.vline (headers_padded |> List.map W.bold)
      ^ glyphs.vline
    in
    if opts.highlight_header then Palette.purple_gradient_line Right line
    else line
  in
  let build_border left sep right =
    let buf = Buffer.create total_w in
    Buffer.add_string buf left ;
    List.iteri
      (fun i w ->
        add_repeat buf glyphs.hline w ;
        Buffer.add_string buf (if i = col_count - 1 then right else sep))
      col_widths ;
    Buffer.contents buf
  in
  let top_border =
    build_border glyphs.corner_tl glyphs.top_sep glyphs.corner_tr
  in
  let mid_border =
    build_border glyphs.mid_left glyphs.mid_sep glyphs.mid_right
  in
  let bottom_border =
    build_border glyphs.corner_bl glyphs.bottom_sep glyphs.corner_br
  in
  let blank_for_col =
    List.mapi (fun idx w -> (idx, String.make w ' ')) col_widths
  in
  let assemble_columns cols =
    let buf = Buffer.create inner_w in
    Buffer.add_string buf glyphs.vline ;
    List.iteri
      (fun idx col ->
        if idx > 0 then Buffer.add_string buf glyphs.vline ;
        Buffer.add_string buf col)
      cols ;
    Buffer.add_string buf glyphs.vline ;
    Buffer.contents buf
  in
  let row_to_lines i cols_cells =
    if not wrap then
      let cells =
        List.mapi
          (fun col_idx cell ->
            let w = List.nth col_widths col_idx in
            let copts = List.nth col_opts col_idx in
            let base = pad cell (w - copts.pad_left - copts.pad_right) in
            String.make copts.pad_left ' '
            ^ base
            ^ String.make copts.pad_right ' ')
          cols_cells
      in
      let line_core = assemble_columns cells in
      let line =
        match opts.selection_mode with
        | Row when i = cursor ->
            Palette.selection_bg (Palette.selection_fg line_core)
        | _ -> line_core
      in
      [line ^ "\027[0m"]
    else
      let cell_lines =
        List.mapi
          (fun col_idx cell ->
            let w = List.nth col_widths col_idx in
            let copts = List.nth col_opts col_idx in
            let inner = max 0 (w - copts.pad_left - copts.pad_right) in
            Widgets.wrap_text ~width:inner cell
            |> List.map (fun l ->
                let base = Widgets.pad_visible l inner in
                String.make copts.pad_left ' '
                ^ base
                ^ String.make copts.pad_right ' '))
          cols_cells
      in
      let height =
        List.fold_left (fun acc ls -> max acc (List.length ls)) 1 cell_lines
      in
      let padded_lines =
        List.mapi
          (fun col_idx lines ->
            let fallback =
              match List.assoc_opt col_idx blank_for_col with
              | Some s -> s
              | None -> String.make (List.nth col_widths col_idx) ' '
            in
            let rec fill lst =
              if List.length lst >= height then lst else fill (lst @ [fallback])
            in
            fill lines)
          cell_lines
      in
      let assemble_line idx =
        let cols_for_idx =
          List.mapi
            (fun col_idx lines ->
              match List.nth_opt lines idx with
              | Some l -> l
              | None -> (
                  match List.assoc_opt col_idx blank_for_col with
                  | Some b -> b
                  | None -> ""))
            padded_lines
        in
        let line_core = assemble_columns cols_for_idx in
        let line =
          match opts.selection_mode with
          | Row when i = cursor ->
              Palette.selection_bg (Palette.selection_fg line_core)
          | _ -> line_core
        in
        line ^ "\027[0m"
      in
      List.init height assemble_line
  in
  let buf = Buffer.create (total_w * (List.length rows_sorted + 4)) in
  let add_line line =
    Buffer.add_string buf line ;
    Buffer.add_char buf '\n'
  in
  add_line top_border ;
  add_line header_line ;
  add_line mid_border ;
  List.iter
    (fun lines -> List.iter add_line lines)
    (List.mapi row_to_lines rows_sorted) ;
  Buffer.add_string buf bottom_border ;
  Buffer.contents buf

let render_table_80_with_opts ?backend ?wrap ~cols ~header:(h1, h2, h3) ~rows
    ~cursor ~sel_col ~opts () =
  let header_list = [h1; h2; h3] in
  let rows_list = List.map (fun (a, b, c) -> [a; b; c]) rows in
  render_table_generic_with_opts
    ?backend
    ?wrap
    ~cols
    ~header_list
    ~rows_list
    ~cursor
    ~sel_col
    ~opts
    ()

let render_table_80 ~cols ~header ~rows ~cursor ~sel_col =
  let a, b, c = header in
  let header_list = [a; b; c] in
  let rows_list = List.map (fun (x, y, z) -> [x; y; z]) rows in
  match W.get_backend () with
  | `Sdl ->
      render_table_sdl
        ~cols
        ~header:(a, b, c)
        ~rows
        ~cursor
        ~sel_col
        ~opts:default_opts
  | `Terminal ->
      render_table_generic_with_opts
        ~backend:(W.get_backend ())
        ~cols
        ~header_list
        ~rows_list
        ~cursor
        ~sel_col
        ~opts:default_opts
        ()

module Table = struct
  type 'a column = {header : string; to_string : 'a -> string}

  type column_layout = {
    min_width : int option;
    max_width : int option;
    weight : int option;
    pad_left : int option;
    pad_right : int option;
  }

  type 'a t = {
    cols : int option;
    columns : 'a column list;
    opts : render_opts;
    rows : 'a list;
    cursor : int;
    layout : column_layout list option;
  }

  let create ?cols ?(opts = default_opts) ?layout ~columns ~rows () =
    let layout = layout in
    {cols; columns; opts; rows; cursor = 0; layout}

  let set_rows t rows =
    let cursor =
      if t.cursor >= List.length rows then max 0 (List.length rows - 1)
      else t.cursor
    in
    {t with rows; cursor}

  let move_cursor t delta =
    let len = List.length t.rows in
    if len = 0 then {t with cursor = 0}
    else
      let raw = t.cursor + delta in
      let raw = if raw < 0 then 0 else if raw >= len then len - 1 else raw in
      {t with cursor = raw}

  let cursor t = t.cursor

  let rows t = List.length t.rows

  let set_layout t layout = {t with layout = Some layout}

  let render t =
    let headers = List.map (fun c -> c.header) t.columns in
    let rendered_rows =
      let safe_to_string f x = try f x with _ -> "<err>" in
      List.map
        (fun a ->
          List.map (fun col -> safe_to_string col.to_string a) t.columns)
        t.rows
    in
    let col_opts =
      match t.layout with
      | None -> []
      | Some specs ->
          let count = List.length t.columns in
          let specs =
            if List.length specs = count then specs
            else
              List.init count (fun _ ->
                  {
                    min_width = None;
                    max_width = None;
                    weight = None;
                    pad_left = None;
                    pad_right = None;
                  })
          in
          let content_widths =
            let all_rows = headers :: rendered_rows in
            List.init count (fun col ->
                List.fold_left
                  (fun acc row ->
                    let cell =
                      List.nth_opt row col |> Option.value ~default:""
                    in
                    max acc (visible_chars_count cell))
                  0
                  all_rows)
          in
          let base_widths =
            List.mapi
              (fun i w ->
                let spec = List.nth specs i in
                match spec.min_width with None -> w | Some mw -> max mw w)
              content_widths
          in
          let total_w =
            match t.cols with
            | Some c -> max 20 (min c 240)
            | None -> col_widths_total
          in
          let inner_w = max 0 (total_w - (count + 1)) in
          let paddings =
            List.map
              (fun spec ->
                let pl = Option.value ~default:1 spec.pad_left in
                let pr = Option.value ~default:1 spec.pad_right in
                pl + pr)
              specs
          in
          let occupied =
            List.fold_left2
              (fun acc w pad -> acc + w + pad)
              0
              base_widths
              paddings
          in
          let remaining = max 0 (inner_w - occupied) in
          let weights =
            let raw =
              List.map (fun spec -> Option.value ~default:1 spec.weight) specs
            in
            let sum = List.fold_left ( + ) 0 raw in
            if sum = 0 then List.init count (fun _ -> 1) else raw
          in
          let distributed =
            if remaining = 0 then List.init count (fun _ -> 0)
            else
              let sumw = List.fold_left ( + ) 0 weights in
              let acc = ref 0 in
              List.mapi
                (fun i w ->
                  if i = count - 1 then remaining - !acc
                  else
                    let add = remaining * w / sumw in
                    acc := !acc + add ;
                    add)
                weights
          in
          List.mapi
            (fun i spec ->
              let pl = Option.value ~default:1 spec.pad_left in
              let pr = Option.value ~default:1 spec.pad_right in
              let w = List.nth base_widths i + List.nth distributed i in
              let w =
                match spec.max_width with Some mw -> min mw w | None -> w
              in
              {max_width = Some w; pad_left = pl; pad_right = pr})
            specs
    in
    render_table_generic_with_opts
      ~cols:t.cols
      ~header_list:headers
      ~rows_list:rendered_rows
      ~cursor:t.cursor
      ~sel_col:0
      ~opts:t.opts
      ~col_opts
      ()

  let get_selected t =
    if t.cursor < 0 || t.cursor >= List.length t.rows then None
    else Some (List.nth t.rows t.cursor)
end
