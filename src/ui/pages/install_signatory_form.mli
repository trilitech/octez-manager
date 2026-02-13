(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

(** Signatory remote signer installation form *)

(** Page identifier for registration *)
val name : string

(** The page module implementing signatory installation *)
val page : Miaou.Core.Registry.page

(** Register the signatory installation page in the Miaou registry *)
val register : unit -> unit

(** The page module for direct TUI initialization (used in tests) *)
module Page : Miaou.Core.Tui_page.PAGE_SIG
