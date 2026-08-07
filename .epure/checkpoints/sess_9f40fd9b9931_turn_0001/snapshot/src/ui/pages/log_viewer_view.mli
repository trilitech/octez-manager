(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

(** Pure rendering functions for the log viewer page. No Eio calls. *)

module Pager = Miaou_widgets_display.Pager_widget

type log_source = Octez_manager_lib.Log_viewer.log_source = Journald | DailyLogs

(** Render the log viewer page. *)
val view :
  instance:string ->
  role:string ->
  source:log_source ->
  pager:Pager.t ->
  focus:bool ->
  size:LTerm_geom.size ->
  string
