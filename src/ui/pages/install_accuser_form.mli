(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Accuser installation form using field bundles *)

val name : string
(** Page identifier for registration *)

val page : Miaou.Core.Registry.page
(** The page module implementing accuser installation *)

val register : unit -> unit
(** Register the accuser installation page in the Miaou registry *)
