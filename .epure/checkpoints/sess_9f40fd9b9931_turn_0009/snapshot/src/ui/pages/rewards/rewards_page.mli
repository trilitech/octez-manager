(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

(** Rewards page: TUI dashboard for reward distribution.

    Provides 4 tabs: Overview, Delegators, History, Configuration.
    Implements [PAGE_SIG] for registration in [manager_app.ml]. *)

val name : string

val register : unit -> unit

(** PAGE_SIG module for use with the headless driver and main_shell. *)
module Page : Miaou.Core.Tui_page.PAGE_SIG
