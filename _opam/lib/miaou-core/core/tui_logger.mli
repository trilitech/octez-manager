(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

val set_enabled : bool -> unit

val set_logfile : string option -> (unit, string) result

val logf : Miaou_interfaces.Logger_capability.level -> string -> unit
