(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)
open Miaou_widgets_display.Widgets

type t = {
  label : string option;
  checked_ : bool;
  cancelled : bool;
  disabled : bool;
}

let create ?label ?(checked_ = false) ?(disabled = false) () =
  {label; checked_; cancelled = false; disabled}

let open_centered ?label ?(checked_ = false) ?disabled () =
  create ?label ~checked_ ?disabled ()

let render (t : t) ~focus =
  let box = if t.checked_ then "[X]" else "[ ]" in
  match t.label with
  | None -> box
  | Some l ->
      let lbl = if focus then bold l else l in
      box ^ " " ^ lbl

let handle_key (t : t) ~key =
  if t.disabled then t
  else
    match key with
    | " " | "Space" | "Enter" -> {t with checked_ = not t.checked_}
    | "Esc" | "Escape" -> {t with cancelled = true}
    | _ -> t

let is_checked t = t.checked_

let set_checked t v = {t with checked_ = v}

let is_cancelled t = t.cancelled

let reset_cancelled t = {t with cancelled = false}
