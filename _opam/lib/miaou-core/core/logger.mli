(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module Tui_logger : sig
  val set_enabled : bool -> unit

  val set_logfile : string option -> (unit, string) result

  val logf : ('a, unit, string, unit) format4 -> 'a
end
