(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)
type t = {label : string; on_click : unit -> unit; disabled : bool}

let create ?(disabled = false) ~label ~on_click () = {label; on_click; disabled}

let render t ~focus =
  let open Miaou_widgets_display.Widgets in
  let base = "[ " ^ t.label ^ " ]" in
  let decorated = if focus then bg 24 (fg 15 (bold base)) else base in
  if t.disabled then dim decorated else decorated

let handle_key t ~key =
  if t.disabled then (t, false)
  else
    match key with
    | "Enter" | " " ->
        t.on_click () ;
        (t, true)
    | _ -> (t, false)
