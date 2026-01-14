(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

[@@@warning "-32-34-37-69"]

module W = Miaou_widgets_display.Widgets

type t = {
  title : string option;
  body : string;
  footer : string option;
  accent : int option;
}

let create ?title ?footer ?accent ~body () = {title; body; footer; accent}

let with_body t body = {t with body}

let render t ~cols =
  let title =
    match t.title with
    | Some s -> ( match t.accent with Some c -> W.fg c (W.bold s) | None -> s)
    | None -> ""
  in
  let footer = match t.footer with Some f -> f | None -> "" in
  W.render_frame ~title ~body:t.body ~footer ~cols ()
