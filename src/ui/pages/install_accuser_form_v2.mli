(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Accuser installer using declarative form builder. *)

val name : string
val page : Miaou.Core.Registry.page
val register : unit -> unit
