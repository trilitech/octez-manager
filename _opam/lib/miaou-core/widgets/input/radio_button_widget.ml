(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)
open Miaou_widgets_display.Widgets

type t = {
  label : string option;
  selected : bool;
  cancelled : bool;
  disabled : bool;
}

let create ?label ?(selected = false) ?(disabled = false) () =
  {label; selected; cancelled = false; disabled}

let open_centered ?label ?(selected = false) ?disabled () =
  create ?label ~selected ?disabled ()

let render (t : t) ~focus =
  let box = if t.selected then "(X)" else "( )" in
  match t.label with
  | None -> box
  | Some l ->
      let lbl = if focus then bold l else l in
      box ^ " " ^ lbl

let handle_key (t : t) ~key =
  if t.disabled then t
  else
    match key with
    | " " | "Space" | "Enter" -> {t with selected = true}
    | "Esc" | "Escape" -> {t with cancelled = true}
    | _ -> t

let is_selected t = t.selected

let set_selected t v = {t with selected = v}

let is_cancelled t = t.cancelled

let reset_cancelled t = {t with cancelled = false}
