(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Clears and re-renders the terminal screen *)
val clear_and_render :
  (module Miaou_core.Tui_page.PAGE_SIG with type state = 'a) ->
  'a Miaou_core.Navigation.t ->
  Miaou_internals.Key_handler_stack.t ->
  (unit -> LTerm_geom.size) ->
  string ref ->
  LTerm_geom.size ref ->
  unit
