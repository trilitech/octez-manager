(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** JSON folding operations for the RPC Browser.

    Provides fold/unfold operations on the focused pager's JSON content,
    using {!Foldable_json} for the fold state and {!Rpc_browser_pagers} for
    pager slot access. *)

open Rpc_browser_types
module Pager = Miaou_widgets_display.Pager_widget

(** Refresh the focused pager's display body from its foldable JSON state.
    Preserves cursor position and search query. *)
let update_focused_pager_from_foldable state =
  match Rpc_browser_pagers.get_focused_pager state with
  | Some slot -> (
      match slot.foldable with
      | Some f ->
          let new_body = Foldable_json.render f in
          let pager = Pager.open_text ~title:"Response" new_body in
          (* Preserve cursor mode, position, and search from old pager *)
          let pager =
            match slot.pager with
            | Some old_p ->
                let pager =
                  Pager.set_cursor_mode pager (Pager.cursor_mode old_p)
                in
                let pager =
                  Pager.set_cursor pager (Pager.get_cursor_line old_p)
                in
                (* Preserve search query *)
                Pager.set_search pager old_p.Pager.search
            | None -> Pager.set_cursor_mode pager true
          in
          let pager_id = Rpc_browser_pagers.get_focused_pager_id state in
          Rpc_browser_pagers.update_pager_slot
            pager_id
            (fun s -> {s with body = new_body; pager = Some pager})
            state
      | None -> state)
  | None -> state

(** Toggle fold at a specific line in the JSON view of the focused pager.
    @param line Line number to toggle fold at *)
let toggle_fold ~line state =
  match Rpc_browser_pagers.get_focused_pager state with
  | Some slot -> (
      match slot.foldable with
      | Some f ->
          let f' = Foldable_json.toggle_fold_at_line f ~line in
          let pager_id = Rpc_browser_pagers.get_focused_pager_id state in
          let state' =
            Rpc_browser_pagers.update_pager_slot
              pager_id
              (fun s -> {s with foldable = Some f'})
              state
          in
          update_focused_pager_from_foldable state'
      | None -> state)
  | None -> state

(** Unfold all JSON sections in the focused pager. *)
let unfold_all state =
  match Rpc_browser_pagers.get_focused_pager state with
  | Some slot -> (
      match slot.foldable with
      | Some f ->
          let f' = Foldable_json.unfold_all f in
          let pager_id = Rpc_browser_pagers.get_focused_pager_id state in
          let state' =
            Rpc_browser_pagers.update_pager_slot
              pager_id
              (fun s -> {s with foldable = Some f'})
              state
          in
          update_focused_pager_from_foldable state'
      | None -> state)
  | None -> state

(** Fold all JSON sections in the focused pager. *)
let fold_all state =
  match Rpc_browser_pagers.get_focused_pager state with
  | Some slot -> (
      match slot.foldable with
      | Some f ->
          let f' = Foldable_json.fold_all f in
          let pager_id = Rpc_browser_pagers.get_focused_pager_id state in
          let state' =
            Rpc_browser_pagers.update_pager_slot
              pager_id
              (fun s -> {s with foldable = Some f'})
              state
          in
          update_focused_pager_from_foldable state'
      | None -> state)
  | None -> state
