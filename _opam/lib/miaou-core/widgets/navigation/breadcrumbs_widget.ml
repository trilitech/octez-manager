(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

[@@@warning "-32-34-37-69"]

module W = Miaou_widgets_display.Widgets
module Helpers = Miaou_helpers.Helpers

type crumb = {id : string; label : string; on_enter : (unit -> unit) option}

type t = {crumbs : crumb list; selected : int}

let crumb ~id ~label ?on_enter () = {id; label; on_enter}

let id c = c.id

let label c = c.label

let clamp idx len = if len = 0 then 0 else max 0 (min (len - 1) idx)

let make crumbs =
  let crumbs =
    List.filter (fun c -> String.length c.label > 0) crumbs
    |> List.map (fun c -> {id = c.id; label = c.label; on_enter = c.on_enter})
  in
  {crumbs; selected = clamp 0 (List.length crumbs)}

let current t = List.nth_opt t.crumbs t.selected

let move t dir =
  let len = List.length t.crumbs in
  if len = 0 then t
  else
    let next =
      match dir with
      | `Left -> max 0 (t.selected - 1)
      | `Right -> min (len - 1) (t.selected + 1)
      | `First -> 0
      | `Last -> len - 1
    in
    {t with selected = next}

let select t ~id =
  let rec find i = function
    | [] -> t.selected
    | c :: cs -> if String.equal c.id id then i else find (i + 1) cs
  in
  let len = List.length t.crumbs in
  let idx = clamp (find 0 t.crumbs) len in
  {t with selected = idx}

let handle_event ?(bubble_unhandled = false) t ~key =
  match Miaou_core.Keys.of_string key with
  | Some Miaou_core.Keys.Left -> (move t `Left, `Handled)
  | Some Miaou_core.Keys.Right -> (move t `Right, `Handled)
  | Some (Miaou_core.Keys.Char "Home") -> (move t `First, `Handled)
  | Some (Miaou_core.Keys.Char "End") -> (move t `Last, `Handled)
  | Some Miaou_core.Keys.Enter -> (
      match current t with
      | Some {on_enter = Some f; _} ->
          f () ;
          (t, `Handled)
      | _ -> (t, if bubble_unhandled then `Bubble else `Handled))
  | _ -> (t, if bubble_unhandled then `Bubble else `Handled)

let handle_key t ~key =
  let t, status = handle_event t ~key in
  (t, match status with `Handled -> `Handled | `Bubble -> `Ignored)

let render t ~focus =
  let parts =
    List.mapi
      (fun i c ->
        let base = if i = t.selected then W.bold c.label else W.dim c.label in
        if focus && i = t.selected then W.title_highlight base else base)
      t.crumbs
  in
  match parts with [] -> "" | _ -> Helpers.concat_with_sep (W.dim " > ") parts
