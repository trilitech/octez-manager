(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

type t = {
  app_bin_dir : string option;
  default_history_mode : History_mode.t option;
  default_logging_mode : Logging_mode.t option;
}

(** Load settings from the configuration file.
    Returns defaults if the file does not exist. *)
val load : unit -> (t, Rresult.R.msg) result

(** Persist settings to the configuration file. *)
val save : t -> (unit, Rresult.R.msg) result

module For_tests : sig
  (** Default settings (all fields [None]). *)
  val default : t

  (** Serialize settings to JSON. *)
  val to_yojson : t -> Yojson.Safe.t

  (** Deserialize settings from JSON. *)
  val of_yojson : Yojson.Safe.t -> (t, Rresult.R.msg) result
end
