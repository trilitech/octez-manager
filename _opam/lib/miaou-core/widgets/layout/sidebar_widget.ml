(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

[@@@warning "-32-34-37-69"]

type t = {
  sidebar : string;
  main : string;
  sidebar_open : bool;
  sidebar_width : int option;
}

let create ?sidebar_width ~sidebar ~main ~sidebar_open () =
  {sidebar; main; sidebar_open; sidebar_width}

let toggle t = {t with sidebar_open = not t.sidebar_open}

let with_main t main = {t with main}

let with_sidebar t sidebar = {t with sidebar}

let render t ~cols =
  let min_cols_for_split = 40 in
  if (not t.sidebar_open) || cols < min_cols_for_split then t.main
  else
    let left_width =
      match t.sidebar_width with
      | Some w -> max 10 (min w (cols / 2))
      | None -> max 16 (cols / 3)
    in
    Pane.split_vertical_with_left_width
      ~width:cols
      ~left_pad:1
      ~right_pad:1
      ~border:true
      ~wrap:false
      ~sep:""
      ~left:t.sidebar
      ~right:t.main
      ~left_width
