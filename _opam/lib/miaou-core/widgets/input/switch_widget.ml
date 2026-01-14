(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)
open Miaou_widgets_display.Widgets

type t = {label : string option; on : bool; cancelled : bool; disabled : bool}

let create ?label ?(on = false) ?(disabled = false) () =
  {label; on; cancelled = false; disabled}

let open_centered ?label ?(on = false) ?disabled () =
  create ?label ~on ?disabled ()

let render (t : t) ~focus =
  let core = if t.on then "[ ON ]" else "[ OFF ]" in
  let sw = if t.on then green core else dim core in
  match t.label with
  | None -> sw
  | Some l ->
      let lbl = if focus then bold l else l in
      lbl ^ ": " ^ sw

let handle_key (t : t) ~key =
  if t.disabled then t
  else
    match key with
    | " " | "Space" | "Enter" -> {t with on = not t.on}
    | "Esc" | "Escape" -> {t with cancelled = true}
    | _ -> t

let is_on t = t.on

let set_on t v = {t with on = v}

let is_cancelled t = t.cancelled

let reset_cancelled t = {t with cancelled = false}
