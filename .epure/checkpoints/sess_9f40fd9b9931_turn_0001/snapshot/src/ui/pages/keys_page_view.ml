(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Pure rendering functions for the keys page. No Eio calls. *)

open Octez_manager_lib
module Widgets = Miaou_widgets_display.Widgets

(** A group of keys from one base directory *)
type dir_group = {
  base_dir : string;
  keys : Keys_reader.key_info list;
  error : string option;
}

(** Page state *)
type state = {
  groups : dir_group list;
  selected : int;
  total_keys : int;
}

let default_client_base_dir () =
  Filename.concat (Paths.home_dir ()) ".tezos-client"

(** Render the page header *)
let header s =
  let count_text =
    match s.total_keys with
    | 0 -> "No keys found"
    | 1 -> "1 key"
    | n -> Printf.sprintf "%d keys" n
  in
  let dir_count = List.length s.groups in
  let dir_text =
    match dir_count with
    | 0 -> ""
    | 1 -> " in 1 directory"
    | n -> Printf.sprintf " across %d directories" n
  in
  [
    Widgets.themed_primary (Printf.sprintf " Keys \xc2\xb7 %s%s" count_text dir_text);
    Widgets.themed_muted "k/j: navigate  Esc: back  ?: help";
  ]

(** Render a single key entry *)
let render_key ~is_selected (key : Keys_reader.key_info) =
  let marker = if is_selected then Widgets.themed_emphasis "  > " else "    " in
  let alias = Widgets.themed_emphasis (Printf.sprintf "%-20s" key.name) in
  let hash = Widgets.themed_muted key.value in
  Printf.sprintf "%s%s %s" marker alias hash

(** Render a directory group with its keys *)
let render_group ~selected ~current_key (group : dir_group) =
  let header_line =
    Printf.sprintf "\n%s" (Widgets.themed_primary group.base_dir)
  in
  let content_lines =
    match group.error with
    | Some err ->
        [
          Printf.sprintf
            "  %s"
            (Widgets.themed_error (Printf.sprintf "Error: %s" err));
        ]
    | None ->
        if group.keys = [] then
          [Printf.sprintf "  %s" (Widgets.themed_muted "(no keys)")]
        else
          List.mapi
            (fun _i key ->
              let global_idx = !selected in
              selected := !selected + 1 ;
              render_key ~is_selected:(global_idx = current_key) key)
            group.keys
  in
  header_line :: content_lines

(** Main view function - renders the entire page *)
let view s ~focus:_ ~size =
  let body =
    if s.groups = [] then
      [
        "";
        Widgets.themed_muted "  No keys found in any base directory.";
        "";
        Widgets.themed_muted "  Keys are stored in:";
        Widgets.themed_muted
          (Printf.sprintf
             "    \xe2\x80\xa2 %s (default)"
             (default_client_base_dir ()));
        Widgets.themed_muted
          "    \xe2\x80\xa2 Managed base directories from baker/accuser instances";
      ]
    else
      let selected_counter = ref 0 in
      s.groups
      |> List.map
           (render_group ~selected:selected_counter ~current_key:s.selected)
      |> List.flatten
  in
  Themed_page.render_layout ~size ~header:(header s) ~footer:[] ~child:(fun _ ->
      String.concat "\n" body)
