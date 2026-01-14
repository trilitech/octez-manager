(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type driver_key = Term_events.driver_key =
  | Quit
  | Refresh
  | Enter
  | NextPage
  | PrevPage
  | Up
  | Down
  | Left
  | Right
  | Other of string

(** Test helper: deterministic runner with injected key source, bypassing TTY setup. *)
val run_with_key_source :
  read_key:(unit -> driver_key) ->
  (module Miaou_core.Tui_page.PAGE_SIG) ->
  [`Quit | `SwitchTo of string]
