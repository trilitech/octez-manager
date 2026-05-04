(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

(** RPC browser page for exploring and executing node RPC endpoints. *)

(** Page name for the page registry. *)
val name : string

(** Pre-built page value for registration. *)
val page : Miaou.Core.Registry.page

(** Register this page with the global page registry. *)
val register : unit -> unit

(** Page implementation satisfying the Miaou TUI page signature. *)
module Page : Miaou.Core.Tui_page.PAGE_SIG
