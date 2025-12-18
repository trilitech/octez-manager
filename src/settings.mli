(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

type t = {
  app_bin_dir : string option;
  default_history_mode : History_mode.t option;
  default_logging_mode : Logging_mode.t option;
}

val load : unit -> (t, Rresult.R.msg) result

val save : t -> (unit, Rresult.R.msg) result
