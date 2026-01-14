(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

[@@@warning "-32-34-37-69"]

module W = Miaou_widgets_display.Widgets
module Helpers = Miaou_helpers.Helpers

type tab = {id : string; label : string}

type t = {tabs : tab list; selected : int}

let clamp idx len = if len = 0 then 0 else max 0 (min (len - 1) idx)

let tab ~id ~label = {id; label}

let id t = t.id

let label t = t.label

let make tabs =
  let tabs =
    List.filter (fun t -> String.length t.label > 0) tabs
    |> List.map (fun t -> {id = t.id; label = t.label})
  in
  {tabs; selected = clamp 0 (List.length tabs)}

let current t =
  match List.nth_opt t.tabs t.selected with Some x -> Some x | None -> None

let move t dir =
  let len = List.length t.tabs in
  if len = 0 then t
  else
    let next =
      match dir with
      | `Left -> (t.selected - 1 + len) mod len
      | `Right -> (t.selected + 1) mod len
      | `First -> 0
      | `Last -> len - 1
    in
    {t with selected = next}

let select t ~id =
  let rec find i = function
    | [] -> t.selected
    | x :: xs -> if String.equal x.id id then i else find (i + 1) xs
  in
  let len = List.length t.tabs in
  let idx = clamp (find 0 t.tabs) len in
  {t with selected = idx}

let handle_event ?(bubble_unhandled = false) t ~key =
  match Miaou_core.Keys.of_string key with
  | Some Miaou_core.Keys.Left -> (move t `Left, `Handled)
  | Some Miaou_core.Keys.Right -> (move t `Right, `Handled)
  | Some Miaou_core.Keys.Home -> (move t `First, `Handled)
  | Some Miaou_core.Keys.End -> (move t `Last, `Handled)
  | _ -> (t, if bubble_unhandled then `Bubble else `Handled)

let handle_key t ~key =
  let t, _ = handle_event t ~key in
  t

let render t ~focus =
  let pad s = if String.length s = 0 then s else Printf.sprintf " %s " s in
  let highlight s = if focus then W.bold s else s in
  let rendered =
    List.mapi
      (fun i tab ->
        if i = t.selected then highlight (pad tab.label)
        else W.dim (pad tab.label))
      t.tabs
  in
  match rendered with
  | [] -> ""
  | _ -> Helpers.concat_with_sep (W.dim "|") rendered
