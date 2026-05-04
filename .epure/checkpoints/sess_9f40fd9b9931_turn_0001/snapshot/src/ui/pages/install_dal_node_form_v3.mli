(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

(** DAL node installation form. *)

(** Name of the form page for navigation. *)
val name : string

(** Register the DAL node installation form page. *)
val register : unit -> unit

(** The DAL node installation form page for TUI. *)
module Page : Miaou.Core.Tui_page.PAGE_SIG
