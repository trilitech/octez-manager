(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

module Widgets = Miaou_widgets_display.Widgets
module Pager = Miaou_widgets_display.Pager_widget
module State = Rpc_browser_state

let render_header ~request ~response_time_ms ~response_size =
  let base = Printf.sprintf "GET %s" (Widgets.bold request) in
  let time_str =
    match response_time_ms with
    | Some t -> Widgets.dim (Printf.sprintf " [%.0fms]" t)
    | None -> ""
  in
  let size_str =
    match response_size with
    | Some s when s >= 1024 -> Widgets.dim (Printf.sprintf " %dKB" (s / 1024))
    | Some s -> Widgets.dim (Printf.sprintf " %dB" s)
    | None -> ""
  in
  Printf.sprintf "%s%s%s" base time_str size_str

let render_body ~body ~scroll_offset ~visible_height =
  let lines = String.split_on_char '\n' body in
  let total = List.length lines in
  let offset = min scroll_offset (max 0 (total - visible_height)) in
  lines |> List.filteri (fun i _ -> i >= offset && i < offset + visible_height)

let render_scroll_indicator ~current ~total =
  if total <= 1 then ""
  else
    let pct = if total > 1 then current * 100 / (total - 1) else 0 in
    Widgets.dim (Printf.sprintf "[%d/%d %d%%]" (current + 1) total pct)

let render_loading () =
  let spinner = Context.render_spinner "" in
  Printf.sprintf "%s %s" spinner (Widgets.dim "Loading...")

let render_error msg = Widgets.red ("Error: " ^ msg)

let render_help () =
  Widgets.dim "?: help  Tab: fold  f/F: fold/unfold all  s: save  Esc: back"

(** Render using pager widget when available *)
let render_with_pager ~pager ~cols ~rows ~focus =
  Pager.render ~cols ~win:rows pager ~focus

(** Render result mode - returns pager output or fallback *)
let render ~state ~cols ~rows ~focus =
  match state.State.mode with
  | State.List _ ->
      (* List mode is handled by a different renderer *)
      Widgets.dim "(list mode - use list renderer)"
  | State.Result {request; response_time_ms; response_size; pager; _} ->
      let header = render_header ~request ~response_time_ms ~response_size in
      let help = render_help () in
      let separator = Widgets.dim (String.make (min 60 cols) '-') in
      (* Calculate available height for pager (minus header/help chrome) *)
      let chrome_lines = 3 in
      let pager_rows = max 1 (rows - chrome_lines) in
      let body_content =
        match pager with
        | Some p -> render_with_pager ~pager:p ~cols ~rows:pager_rows ~focus
        | None -> Widgets.dim "Loading..."
      in
      let error_line =
        match state.State.error with
        | Some msg -> "\n" ^ render_error msg
        | None -> ""
      in
      Printf.sprintf "%s\n%s\n%s%s\n%s" header separator body_content error_line help
