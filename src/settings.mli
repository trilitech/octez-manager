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

module For_tests : sig
  val default : t

  val to_yojson : t -> Yojson.Safe.t

  val of_yojson : Yojson.Safe.t -> (t, Rresult.R.msg) result
end
