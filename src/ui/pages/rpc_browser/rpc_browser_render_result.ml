(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

module Widgets = Miaou_widgets_display.Widgets
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
  let keys =
    [
      ("j/k", "scroll");
      ("PgUp/Dn", "page");
      ("g/G", "top/bottom");
      ("Esc", "back");
    ]
  in
  let parts = List.map (fun (k, v) -> Printf.sprintf "%s: %s" k v) keys in
  Widgets.dim (String.concat "  " parts)

let render ~state ~cols ~rows =
  let _ = cols in
  match state.State.mode with
  | State.List _ ->
      (* List mode is handled by a different renderer *)
      [Widgets.dim "(list mode - use list renderer)"]
  | State.Result
      {request; body; scroll_offset; response_time_ms; response_size; _} ->
      let header = render_header ~request ~response_time_ms ~response_size in
      let separator = Widgets.dim (String.make 60 '-') in
      let help = render_help () in
      (* Calculate available height for body *)
      let chrome_lines = 4 in
      (* header + 2 separators + help *)
      let visible_height = max 1 (rows - chrome_lines) in
      let body_lines = render_body ~body ~scroll_offset ~visible_height in
      let total_lines = List.length (String.split_on_char '\n' body) in
      let scroll_ind =
        render_scroll_indicator ~current:scroll_offset ~total:total_lines
      in
      let header_with_scroll =
        if scroll_ind = "" then header
        else Printf.sprintf "%s  %s" header scroll_ind
      in
      let error_line =
        match state.State.error with
        | Some msg -> [render_error msg]
        | None -> []
      in
      [header_with_scroll; separator]
      @ body_lines @ error_line @ [separator; help]
