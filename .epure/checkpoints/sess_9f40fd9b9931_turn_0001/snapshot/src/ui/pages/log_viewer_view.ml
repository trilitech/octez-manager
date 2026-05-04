(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

(** Pure rendering functions for the log viewer page. No Eio calls. *)

module Pager = Miaou_widgets_display.Pager_widget
module Widgets = Miaou_widgets_display.Widgets
open Octez_manager_lib

type log_source = Log_viewer.log_source = Journald | DailyLogs

let view ~instance ~role:_ ~source ~pager ~focus ~size =
  let source_str =
    match source with
    | Log_viewer.Journald -> "journald"
    | Log_viewer.DailyLogs -> "daily logs"
  in
  let privilege =
    if Paths.is_root () then Widgets.themed_error "@ SYSTEM"
    else Widgets.themed_success "@ USER"
  in
  let title =
    Printf.sprintf
      "%s   %s"
      (Widgets.themed_primary
         (Printf.sprintf " Logs: %s " (String.capitalize_ascii instance)))
      privilege
  in
  let help =
    Widgets.themed_muted
      (Printf.sprintf
         "Source: %s . r: refresh . t: toggle . /: search . f: follow . w: \
          wrap . ?: help . Esc: back"
         source_str)
  in
  let header = [title; help] in
  Themed_page.render_layout ~size ~header ~footer:[] ~child:(fun inner_size ->
      Pager.render
        ~cols:inner_size.LTerm_geom.cols
        ~win:inner_size.LTerm_geom.rows
        pager
        ~focus)
