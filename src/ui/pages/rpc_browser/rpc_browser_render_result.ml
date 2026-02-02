(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

module Widgets = Miaou_widgets_display.Widgets
module Pager = Miaou_widgets_display.Pager_widget
module State = Rpc_browser_state

(* Layout constants *)
let min_pager_cols = 80

let min_pager_rows = 24

let render_pager_header ~slot ~is_focused =
  let id_marker =
    if is_focused then Printf.sprintf "[%d*]" slot.State.id
    else Printf.sprintf "[%d]" slot.State.id
  in
  let id_str =
    if is_focused then Widgets.fg 14 id_marker else Widgets.dim id_marker
  in
  let request_str =
    if slot.State.request = "" then Widgets.dim "(empty)"
    else
      let short_req =
        if String.length slot.State.request > 40 then
          String.sub slot.State.request 0 37 ^ "..."
        else slot.State.request
      in
      if is_focused then Widgets.bold short_req else Widgets.dim short_req
  in
  let time_str =
    match slot.State.response_time_ms with
    | Some t -> Widgets.dim (Printf.sprintf " [%.0fms]" t)
    | None -> ""
  in
  let size_str =
    match slot.State.response_size with
    | Some s when s >= 1024 -> Widgets.dim (Printf.sprintf " %dKB" (s / 1024))
    | Some s -> Widgets.dim (Printf.sprintf " %dB" s)
    | None -> ""
  in
  Printf.sprintf "%s GET %s%s%s" id_str request_str time_str size_str

let render_loading () =
  let spinner = Context.render_spinner "" in
  Printf.sprintf "%s %s" spinner (Widgets.dim "Loading...")

let render_error msg = Widgets.red ("Error: " ^ msg)

let render_help ~num_pagers =
  let split_hint = if num_pagers < 10 then "S: split  " else "" in
  let close_hint = if num_pagers > 1 then "x: close  " else "" in
  Widgets.dim
    (Printf.sprintf
       "?: help  %s%s0-9: focus  Space: fold  f/F: fold/unfold  s: save  Esc: \
        back"
       split_hint
       close_hint)

(** Render using pager widget when available *)
let render_with_pager ~pager ~cols ~rows ~focus =
  Pager.render ~cols ~win:rows pager ~focus

(** Render a single pager slot *)
let render_single_pager ~slot ~cols ~rows ~is_focused =
  let header = render_pager_header ~slot ~is_focused in
  let separator =
    if is_focused then Widgets.fg 14 (String.make (min 60 cols) '-')
    else Widgets.dim (String.make (min 60 cols) '-')
  in
  let chrome_lines = 2 in
  let pager_rows = max 1 (rows - chrome_lines) in
  let body_content =
    match slot.State.pager with
    | Some p ->
        render_with_pager ~pager:p ~cols ~rows:pager_rows ~focus:is_focused
    | None ->
        if slot.State.request = "" then Widgets.dim "(no request yet)"
        else render_loading ()
  in
  Printf.sprintf "%s\n%s\n%s" header separator body_content

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
  else Widgets.dim (Printf.sprintf " %s%s" left_str right_str)

(** Layout orientation *)
type layout = Horizontal | Vertical

(** Calculate optimal layout based on available space and number of pagers.
    Choose the orientation that gives each pager the most space. *)
let calculate_layout ~cols ~rows ~num_pagers =
  if num_pagers <= 1 then (Vertical, 1, num_pagers)
  else
    (* Calculate space per pager for each orientation *)
    let h_cols_per_pager = cols / num_pagers in
    let h_rows_per_pager = rows in
    let v_cols_per_pager = cols in
    let v_rows_per_pager = rows / num_pagers in
    (* Calculate "area" (cols * rows) for each orientation *)
    let h_area = h_cols_per_pager * h_rows_per_pager in
    let v_area = v_cols_per_pager * v_rows_per_pager in
    (* Check minimum constraints *)
    let h_viable = h_cols_per_pager >= min_pager_cols in
    let v_viable = v_rows_per_pager >= min_pager_rows in
    match (h_viable, v_viable) with
    | true, true ->
        (* Both viable - pick the one with more area per pager *)
        if h_area >= v_area then (Horizontal, num_pagers, num_pagers)
        else (Vertical, num_pagers, num_pagers)
    | true, false -> (Horizontal, num_pagers, num_pagers)
    | false, true -> (Vertical, num_pagers, num_pagers)
    | false, false ->
        (* Neither viable at full count - reduce visible pagers *)
        let max_h = max 1 (cols / min_pager_cols) in
        let max_v = max 1 (rows / min_pager_rows) in
        if max_h >= max_v then (Horizontal, max_h, max_h)
        else (Vertical, max_v, max_v)

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

(** Calculate visible length of a string (excluding ANSI escape codes) *)
let visible_length s =
  let len = String.length s in
  let rec skip_escape i =
    if i >= len then len
    else
      match s.[i] with
      | 'A' .. 'Z' | 'a' .. 'z' -> i + 1
      | _ -> skip_escape (i + 1)
  in
  let rec loop i acc =
    if i >= len then acc
    else if s.[i] = '\027' then loop (skip_escape (i + 1)) acc
    else loop (i + 1) (acc + 1)
  in
  loop 0 0

(** Split a string into lines, padding to ensure consistent count *)
let split_lines_padded s ~target_lines ~width =
  let lines = String.split_on_char '\n' s in
  let padded =
    if List.length lines >= target_lines then lines
    else
      let padding =
        List.init (target_lines - List.length lines) (fun _ -> "")
      in
      lines @ padding
  in
  (* Ensure each line is exactly width chars (for alignment) *)
  List.map
    (fun line ->
      let visible_len = visible_length line in
      if visible_len >= width then line
      else line ^ String.make (width - visible_len) ' ')
    padded

(** Render multiple pagers horizontally (side-by-side) *)
let render_horizontal ~pagers ~focused_id ~cols ~rows ~focus
    ~(result_focus : State.result_focus) =
  let num_pagers = List.length pagers in
  let pager_width = cols / num_pagers in
  let separator_col = Widgets.dim "│" in
  (* Render each pager to its own string *)
  let pager_renders =
    List.map
      (fun slot ->
        let is_focused =
          focus && slot.State.id = focused_id
          && match result_focus with State.FocusPager _ -> true | _ -> false
        in
        render_single_pager ~slot ~cols:(pager_width - 1) ~rows ~is_focused)
      pagers
  in
  (* Split each render into lines *)
  let max_lines =
    List.fold_left
      (fun acc s -> max acc (List.length (String.split_on_char '\n' s)))
      0
      pager_renders
  in
  let line_arrays =
    List.map
      (fun s ->
        split_lines_padded s ~target_lines:max_lines ~width:(pager_width - 1))
      pager_renders
  in
  (* Join lines horizontally *)
  let combined_lines =
    List.init max_lines (fun i ->
        let row_parts = List.map (fun lines -> List.nth lines i) line_arrays in
        String.concat separator_col row_parts)
  in
  String.concat "\n" combined_lines

(** Render pager tabs for single-column mode *)
let render_pager_tabs ~pagers ~focused_id =
  let sorted = List.sort (fun a b -> compare a.State.id b.State.id) pagers in
  let tabs =
    List.map
      (fun slot ->
        let is_focused = slot.State.id = focused_id in
        if is_focused then Widgets.fg 14 (Printf.sprintf "[%d*]" slot.State.id)
        else Widgets.dim (Printf.sprintf "[%d]" slot.State.id))
      sorted
  in
  String.concat "" tabs

(** Render result mode with multi-pager support *)
let render ~state ~cols ~rows ~focus =
  match state.State.mode with
  | State.List _ -> Widgets.dim "(list mode - use list renderer)"
  | State.Result {pagers; focus = result_focus; _} ->
      let focused_id =
        match result_focus with
        | State.FocusPager id -> id
        | State.FocusBrowser -> 0
      in
      let num_pagers = List.length pagers in
      let help = render_help ~num_pagers in
      let error_line =
        match state.State.error with
        | Some msg -> render_error msg ^ "\n"
        | None -> ""
      in

      if num_pagers = 0 then
        Printf.sprintf "%s\n%s%s" (Widgets.dim "(no pagers)") error_line help
      else if num_pagers = 1 then
        (* Single pager - simple layout *)
        let slot = List.hd pagers in
        let is_focused =
          focus
          && match result_focus with State.FocusPager _ -> true | _ -> false
        in
        let content =
          render_single_pager ~slot ~cols ~rows:(rows - 2) ~is_focused
        in
        Printf.sprintf "%s\n%s%s" content error_line help
      else
        (* Multi-pager layout - choose optimal orientation *)
        let layout, _visible_count, max_visible =
          calculate_layout ~cols ~rows:(rows - 2) ~num_pagers
        in
        let visible, hidden_left, hidden_right =
          get_visible_pagers ~pagers ~focused_id ~max_visible
        in
        let hidden_indicator =
          render_hidden_indicator ~hidden_left ~hidden_right
        in
        let num_visible = List.length visible in

        let content =
          match layout with
          | Vertical ->
              (* Vertical stack *)
              let pager_height = (rows - 3) / num_visible in
              let pager_strs =
                List.map
                  (fun slot ->
                    let is_focused =
                      focus && slot.State.id = focused_id
                      &&
                      match result_focus with
                      | State.FocusPager _ -> true
                      | _ -> false
                    in
                    render_single_pager
                      ~slot
                      ~cols
                      ~rows:pager_height
                      ~is_focused)
                  visible
              in
              String.concat "\n" pager_strs
          | Horizontal ->
              (* Horizontal side-by-side *)
              let pager_height = rows - 3 in
              render_horizontal
                ~pagers:visible
                ~focused_id
                ~cols
                ~rows:pager_height
                ~focus
                ~result_focus
        in
        Printf.sprintf "%s%s\n%s%s" content hidden_indicator error_line help
