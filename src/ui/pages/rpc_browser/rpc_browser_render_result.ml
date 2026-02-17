(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

module Widgets = Miaou_widgets_display.Widgets
module Pager = Miaou_widgets_display.Pager_widget
module State = Rpc_browser_state
module Style_context = Miaou_style.Style_context

(* Layout constants *)
let min_pager_cols = 80

let min_pager_rows = 24

(** Calculate visible length of a string (excluding ANSI escape codes, handling UTF-8) *)
let visible_length s =
  let len = String.length s in
  let rec skip_escape i =
    if i >= len then len
    else
      match s.[i] with
      | 'A' .. 'Z' | 'a' .. 'z' -> i + 1
      | _ -> skip_escape (i + 1)
  in
  (* Get byte length of UTF-8 character starting at position i *)
  let utf8_char_len i =
    if i >= len then 0
    else
      let c = Char.code s.[i] in
      if c land 0x80 = 0 then 1 (* ASCII: 0xxxxxxx *)
      else if c land 0xE0 = 0xC0 then 2 (* 2-byte: 110xxxxx *)
      else if c land 0xF0 = 0xE0 then 3 (* 3-byte: 1110xxxx *)
      else if c land 0xF8 = 0xF0 then 4 (* 4-byte: 11110xxx *)
      else 1 (* Invalid, treat as 1 *)
  in
  let rec loop i acc =
    if i >= len then acc
    else if s.[i] = '\027' then loop (skip_escape (i + 1)) acc
    else
      let char_len = utf8_char_len i in
      loop (i + char_len) (acc + 1)
  in
  loop 0 0

let render_pager_header ~slot ~is_focused ~is_target =
  (* Get target instance name - use short form for display *)
  let target_name =
    match slot.State.target_instance with
    | Some svc ->
        let name = svc.Octez_manager_lib.Service.instance in
        (* Truncate long names *)
        if String.length name > 20 then String.sub name 0 17 ^ "..." else name
    | None -> "?"
  in
  (* Clean OpenCode-style header: simple markers, no heavy ASCII art *)
  let id_str = Printf.sprintf "[%d]" slot.State.id in
  let request_str =
    if slot.State.request = "" then "(empty)"
    else if String.length slot.State.request > 40 then
      String.sub slot.State.request 0 37 ^ "..."
    else slot.State.request
  in
  let time_str =
    match slot.State.response_time_ms with
    | Some t -> Printf.sprintf " %.0fms" t
    | None -> ""
  in
  let size_str =
    match slot.State.response_size with
    | Some s when s >= 1024 -> Printf.sprintf " %dKB" (s / 1024)
    | Some s -> Printf.sprintf " %dB" s
    | None -> ""
  in
  let base_text =
    Printf.sprintf
      "%s @%s GET %s%s%s"
      id_str
      target_name
      request_str
      time_str
      size_str
  in
  if is_focused then
    (* Focused pager: plain text for external styling *)
    base_text
  else if is_target then
    (* Target pager: accent color for the ID *)
    let id_colored = Widgets.themed_accent id_str in
    Printf.sprintf
      "%s %s GET %s%s%s"
      id_colored
      (Widgets.themed_muted (Printf.sprintf "@%s" target_name))
      (Widgets.themed_muted request_str)
      (Widgets.themed_muted time_str)
      (Widgets.themed_muted size_str)
  else
    (* Unfocused pager: all muted *)
    Widgets.themed_muted base_text

let render_loading () =
  let spinner = Context.render_spinner "" in
  Printf.sprintf "%s %s" spinner (Widgets.themed_muted "Loading...")

let render_error msg = Widgets.themed_error ("Error: " ^ msg)

let render_help ~num_pagers =
  let split_hint = if num_pagers < 10 then "S: split  " else "" in
  let close_hint = if num_pagers > 1 then "x: close  " else "" in
  let pager_hint = if num_pagers > 1 then "C-x N: pager  " else "" in
  Widgets.themed_muted
    (Printf.sprintf
       "?: help  %s%s%s1-5: shortcut  Space: fold  f/F: fold  s: save  Esc: \
        back"
       split_hint
       close_hint
       pager_hint)

(** Render using pager widget when available *)
let render_with_pager ~pager ~cols ~rows ~focus =
  Pager.render ~cols ~win:rows pager ~focus

(** Render a single pager slot with consistent width - OpenCode style with no heavy borders *)
let render_single_pager ~slot ~cols ~rows ~is_focused ~is_target =
  let header = render_pager_header ~slot ~is_focused ~is_target in
  (* Pad header to full width - use dimmed background for visual separation *)
  let header_visible_len = visible_length header in
  let header_padded =
    let padding =
      if header_visible_len >= cols then ""
      else String.make (cols - header_visible_len) ' '
    in
    Style_context.with_child_context
      ~widget_name:"rpc-pager-header"
      ~focused:is_focused
      ~selected:is_target
      (fun () ->
        Widgets.themed_contextual_fill
          (Widgets.themed_contextual (header ^ padding)))
  in
  (* OpenCode style: no heavy borders, just a subtle line or empty space *)
  let chrome_lines = 1 in
  let pager_rows = max 1 (rows - chrome_lines) in
  let body_content =
    match slot.State.pager with
    | Some p ->
        render_with_pager ~pager:p ~cols ~rows:pager_rows ~focus:is_focused
    | None ->
        if slot.State.request = "" then Widgets.themed_muted "(no request yet)"
        else render_loading ()
  in
  Printf.sprintf "%s\n%s" header_padded body_content

(** Render hidden pager indicator *)
let render_hidden_indicator ~hidden_left ~hidden_right =
  let left_str =
    if hidden_left = [] then ""
    else
      Printf.sprintf
        "[%s<-]"
        (String.concat "," (List.map string_of_int hidden_left))
  in
  let right_str =
    if hidden_right = [] then ""
    else
      Printf.sprintf
        "[->%s]"
        (String.concat "," (List.map string_of_int hidden_right))
  in
  if left_str = "" && right_str = "" then ""
  else Widgets.themed_muted (Printf.sprintf " %s%s" left_str right_str)

(** Grid layout: (columns, rows) *)
type grid_layout = {grid_cols : int; grid_rows : int}

(** Calculate optimal grid layout for given number of pagers.
    Tries different grid arrangements and picks the one that maximizes
    space per pager while respecting minimum constraints. *)
let calculate_layout ~cols ~rows ~num_pagers =
  if num_pagers <= 0 then ({grid_cols = 1; grid_rows = 1}, 0)
  else if num_pagers = 1 then ({grid_cols = 1; grid_rows = 1}, 1)
  else
    (* Maximum grid dimensions based on minimum pager sizes *)
    let max_grid_cols = max 1 (cols / min_pager_cols) in
    let max_grid_rows = max 1 (rows / min_pager_rows) in
    (* Generate all (gc, gr) pairs to try *)
    let grid_configs =
      List.concat_map
        (fun gc -> List.init max_grid_rows (fun gr -> (gc, gr + 1)))
        (List.init max_grid_cols (fun gc -> gc + 1))
    in
    (* Initial best: single column with all pagers *)
    let init_visible = min num_pagers max_grid_rows in
    let init = ({grid_cols = 1; grid_rows = num_pagers}, 0, init_visible) in
    (* Find best configuration *)
    let best_layout, _, best_visible =
      List.fold_left
        (fun ((_, best_area, best_vis) as best) (gc, gr) ->
          let capacity = gc * gr in
          if capacity >= num_pagers || capacity >= best_vis then
            let visible = min num_pagers capacity in
            let pager_width = cols / gc in
            let pager_height = rows / gr in
            let area = pager_width * pager_height in
            (* Prefer layouts that show all pagers, then maximize area *)
            if visible > best_vis || (visible = best_vis && area > best_area)
            then ({grid_cols = gc; grid_rows = gr}, area, visible)
            else best
          else best)
        init
        grid_configs
    in
    (best_layout, best_visible)

(** Get visible pager slots based on focus - focused pager is always visible *)
let get_visible_pagers ~pagers ~focused_id ~max_visible =
  let sorted = List.sort (fun a b -> compare a.State.id b.State.id) pagers in
  let n = List.length sorted in
  if n <= max_visible then (sorted, [], [])
  else
    (* Find focused pager position *)
    let focused_idx =
      match
        List.mapi (fun i p -> (i, p)) sorted
        |> List.find_opt (fun (_, p) -> p.State.id = focused_id)
      with
      | Some (i, _) -> i
      | None -> 0
    in
    (* Calculate window around focused pager *)
    let half = max_visible / 2 in
    let start_idx =
      if focused_idx < half then 0
      else if focused_idx >= n - half then max 0 (n - max_visible)
      else focused_idx - half
    in
    let visible =
      List.filteri
        (fun i _ -> i >= start_idx && i < start_idx + max_visible)
        sorted
    in
    let hidden_left =
      List.filteri (fun i _ -> i < start_idx) sorted
      |> List.map (fun p -> p.State.id)
    in
    let hidden_right =
      List.filteri (fun i _ -> i >= start_idx + max_visible) sorted
      |> List.map (fun p -> p.State.id)
    in
    (visible, hidden_left, hidden_right)

(** Truncate a string with ANSI codes to a visible width (handles UTF-8) *)
let truncate_to_width s ~width =
  let len = String.length s in
  let buf = Buffer.create (width * 4) in
  let rec skip_escape i =
    if i >= len then len
    else
      match s.[i] with
      | 'A' .. 'Z' | 'a' .. 'z' -> i + 1
      | _ -> skip_escape (i + 1)
  in
  (* Get byte length of UTF-8 character starting at position i *)
  let utf8_char_len i =
    if i >= len then 0
    else
      let c = Char.code s.[i] in
      if c land 0x80 = 0 then 1
      else if c land 0xE0 = 0xC0 then 2
      else if c land 0xF0 = 0xE0 then 3
      else if c land 0xF8 = 0xF0 then 4
      else 1
  in
  let rec loop i visible_count =
    if i >= len || visible_count >= width then ()
    else if s.[i] = '\027' then begin
      (* Copy the entire escape sequence *)
      let end_idx = skip_escape (i + 1) in
      Buffer.add_substring buf s i (end_idx - i) ;
      loop end_idx visible_count
    end
    else begin
      (* Copy entire UTF-8 character *)
      let char_len = utf8_char_len i in
      let char_len = min char_len (len - i) in
      Buffer.add_substring buf s i char_len ;
      loop (i + char_len) (visible_count + 1)
    end
  in
  loop 0 0 ;
  Buffer.contents buf

(** Split a string into lines, padding/truncating to ensure consistent dimensions *)
let split_lines_padded s ~target_lines ~width =
  let lines = String.split_on_char '\n' s in
  (* Take only target_lines lines *)
  let trimmed =
    if List.length lines <= target_lines then lines
    else List.filteri (fun i _ -> i < target_lines) lines
  in
  let padded =
    if List.length trimmed >= target_lines then trimmed
    else
      let padding =
        List.init (target_lines - List.length trimmed) (fun _ -> "")
      in
      trimmed @ padding
  in
  (* Ensure each line is exactly width chars (truncate or pad) *)
  List.map
    (fun line ->
      let visible_len = visible_length line in
      if visible_len > width then truncate_to_width line ~width
      else if visible_len < width then
        line ^ String.make (width - visible_len) ' '
      else line)
    padded

(** Render a row of pagers horizontally (side-by-side) - OpenCode style *)
let render_pager_row ~pagers ~focused_id ~target_id ~pager_width ~pager_height
    ~focus ~(result_focus : State.result_focus) =
  if pagers = [] then ""
  else
    (* Thicker muted vertical separator (3 chars: space + bar + space) *)
    let separator_col = Widgets.themed_muted " │ " in
    (* Render each pager to its own string - use full pager_width *)
    let pager_renders =
      List.map
        (fun slot ->
          let is_focused =
            focus && slot.State.id = focused_id
            && match result_focus with State.FocusPager _ -> true | _ -> false
          in
          let is_target =
            (not is_focused) && slot.State.id = target_id
            && match result_focus with State.FocusBrowser -> true | _ -> false
          in
          render_single_pager
            ~slot
            ~cols:pager_width
            ~rows:pager_height
            ~is_focused
            ~is_target)
        pagers
    in
    (* Split each render into lines and pad/truncate to exact dimensions *)
    let line_arrays =
      List.map
        (fun s ->
          split_lines_padded s ~target_lines:pager_height ~width:pager_width)
        pager_renders
    in
    (* Join lines horizontally *)
    let combined_lines =
      List.init pager_height (fun i ->
          let row_parts =
            List.map (fun lines -> List.nth lines i) line_arrays
          in
          String.concat separator_col row_parts)
    in
    String.concat "\n" combined_lines

(* Helper to repeat a UTF-8 string n times *)
let repeat_utf8 s n =
  let buf = Buffer.create (n * String.length s) in
  for _ = 1 to n do
    Buffer.add_string buf s
  done ;
  Buffer.contents buf

(** Build a horizontal separator row with proper box-drawing intersections *)
let build_separator_row ~pager_width ~grid_cols ~separator_width:_ =
  let buf = Buffer.create 256 in
  for col = 0 to grid_cols - 1 do
    (* Add horizontal line for this pager width *)
    Buffer.add_string buf (repeat_utf8 "─" pager_width) ;
    (* Add intersection or end *)
    if col < grid_cols - 1 then
      (* Intersection: space + cross + space to match " │ " vertical separator *)
      Buffer.add_string buf "─┼─"
  done ;
  Widgets.themed_muted (Buffer.contents buf)

(** Render pagers in a grid layout - OpenCode style with muted separators *)
let render_grid ~pagers ~focused_id ~target_id ~cols ~rows ~grid_cols ~grid_rows
    ~focus ~(result_focus : State.result_focus) =
  (* Account for 3-char wide separators between columns *)
  let separator_width = 3 in
  let total_separator_width = separator_width * (grid_cols - 1) in
  let pager_width = (cols - total_separator_width) / grid_cols in
  let pager_height = rows / grid_rows in
  (* Build horizontal separator with proper intersections *)
  let separator_row =
    build_separator_row ~pager_width ~grid_cols ~separator_width
  in
  (* Arrange pagers into grid rows *)
  let pager_array = Array.of_list pagers in
  let num_pagers = Array.length pager_array in
  let grid_row_renders =
    List.init grid_rows (fun row_idx ->
        let start = row_idx * grid_cols in
        let row_pagers =
          List.init grid_cols (fun col_idx ->
              let idx = start + col_idx in
              if idx < num_pagers then Some pager_array.(idx) else None)
          |> List.filter_map Fun.id
        in
        if row_pagers = [] then ""
        else
          render_pager_row
            ~pagers:row_pagers
            ~focused_id
            ~target_id
            ~pager_width
            ~pager_height
            ~focus
            ~result_focus)
  in
  (* Join grid rows with separator that has proper intersections *)
  String.concat
    ("\n" ^ separator_row ^ "\n")
    (List.filter (fun s -> s <> "") grid_row_renders)

(** Render pager tabs for single-column mode *)
let render_pager_tabs ~pagers ~focused_id =
  let sorted = List.sort (fun a b -> compare a.State.id b.State.id) pagers in
  let tabs =
    List.map
      (fun slot ->
        let is_focused = slot.State.id = focused_id in
        if is_focused then
          Widgets.themed_accent (Printf.sprintf "[%d*]" slot.State.id)
        else Widgets.themed_muted (Printf.sprintf "[%d]" slot.State.id))
      sorted
  in
  String.concat "" tabs

(** Render result mode with multi-pager support *)
module For_tests = struct
  let visible_length = visible_length

  let truncate_to_width = truncate_to_width

  let split_lines_padded = split_lines_padded
end

let render ~state ~cols ~rows ~focus =
  match state.State.mode with
  | State.List _ -> Widgets.themed_muted "(list mode - use list renderer)"
  | State.Result {pagers; focus = result_focus; last_pager_id} ->
      let focused_id =
        match result_focus with
        | State.FocusPager id -> id
        | State.FocusBrowser -> 0
      in
      (* target_id is the pager that will receive next RPC result *)
      let target_id = last_pager_id in
      let num_pagers = List.length pagers in
      let help = render_help ~num_pagers in
      let error_line =
        match state.State.error with
        | Some msg -> render_error msg ^ "\n"
        | None -> ""
      in

      if num_pagers = 0 then
        Printf.sprintf
          "%s\n%s%s"
          (Widgets.themed_muted "(no pagers)")
          error_line
          help
      else if num_pagers = 1 then
        (* Single pager - simple layout *)
        let slot = List.hd pagers in
        let is_focused =
          focus
          && match result_focus with State.FocusPager _ -> true | _ -> false
        in
        let is_target =
          (not is_focused) && slot.State.id = target_id
          && match result_focus with State.FocusBrowser -> true | _ -> false
        in
        let content =
          render_single_pager
            ~slot
            ~cols
            ~rows:(rows - 2)
            ~is_focused
            ~is_target
        in
        Printf.sprintf "%s\n%s%s" content error_line help
      else
        (* Multi-pager layout - calculate optimal grid *)
        let available_rows = rows - 2 in
        let {grid_cols; grid_rows}, max_visible =
          calculate_layout ~cols ~rows:available_rows ~num_pagers
        in
        let visible, hidden_left, hidden_right =
          get_visible_pagers ~pagers ~focused_id ~max_visible
        in
        let hidden_indicator =
          render_hidden_indicator ~hidden_left ~hidden_right
        in
        let content =
          render_grid
            ~pagers:visible
            ~focused_id
            ~target_id
            ~cols
            ~rows:(available_rows - 1)
            ~grid_cols
            ~grid_rows
            ~focus
            ~result_focus
        in
        Printf.sprintf "%s%s\n%s%s" content hidden_indicator error_line help
