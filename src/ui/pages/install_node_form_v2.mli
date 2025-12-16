(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Node installation form using declarative form builder *)

val name : string
(** Page identifier for registration *)

val page : Miaou.Core.Registry.page
(** The page module implementing node installation *)

val register : unit -> unit
(** Register the node installation page in the Miaou registry *)
