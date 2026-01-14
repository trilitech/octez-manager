(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

[@@@warning "-32-34-37-69"]

module W = Miaou_widgets_display.Widgets
module Helpers = Miaou_helpers.Helpers

type severity = Info | Success | Warn | Error

type position = [`Top_left | `Top_right | `Bottom_left | `Bottom_right]

type toast = {
  id : int;
  message : string;
  severity : severity;
  created_at : float;
  ttl : float;
}

type t = {queue : toast list; next_id : int; position : position}

let empty ?(position = `Top_right) () = {queue = []; next_id = 0; position}

let enqueue ?(ttl = 5.) ?now (t : t) sev message =
  let created_at =
    match now with Some v -> v | None -> Unix.gettimeofday ()
  in
  let toast = {id = t.next_id; message; severity = sev; created_at; ttl} in
  {t with queue = t.queue @ [toast]; next_id = t.next_id + 1}

let dismiss t ~id = {t with queue = List.filter (fun q -> q.id <> id) t.queue}

let tick ?now t =
  let now = match now with Some v -> v | None -> Unix.gettimeofday () in
  let fresh q = now -. q.created_at <= q.ttl in
  {t with queue = List.filter fresh t.queue}

let with_position t position = {t with position}

let to_list t = t.queue

let color_of_sev = function
  | Info -> W.fg 81
  | Success -> W.fg 40
  | Warn -> W.fg 214
  | Error -> W.fg 196

let render_line ~cols (msg : string) =
  let visible = W.visible_chars_count msg in
  if visible >= cols then msg
  else
    let pad = String.make (max 0 (cols - visible)) ' ' in
    msg ^ pad

let render t ~cols =
  let apply_pos lines =
    match t.position with
    | `Top_left | `Bottom_left -> lines
    | `Top_right | `Bottom_right ->
        List.map
          (fun line ->
            let v = W.visible_chars_count line in
            if v >= cols then line
            else
              let pad = String.make (max 0 (cols - v)) ' ' in
              pad ^ line)
          lines
  in
  let base_lines =
    List.map
      (fun q ->
        let tag =
          match q.severity with
          | Info -> "[info]"
          | Success -> "[ok]"
          | Warn -> "[warn]"
          | Error -> "[err]"
        in
        let colored = color_of_sev q.severity in
        render_line ~cols (colored (Printf.sprintf "%s %s" tag q.message)))
      t.queue
  in
  let ordered =
    match t.position with
    | `Top_left | `Top_right -> List.rev base_lines
    | `Bottom_left | `Bottom_right -> base_lines
  in
  Helpers.concat_lines (apply_pos ordered)
