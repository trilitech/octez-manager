(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

(** Page name for the page registry. *)
val name : string

(** Register this page with the global page registry. *)
val register : unit -> unit

module Page : Miaou.Core.Tui_page.PAGE_SIG
