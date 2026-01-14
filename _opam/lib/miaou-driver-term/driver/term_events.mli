(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type driver_key =
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

val clear : unit -> unit
