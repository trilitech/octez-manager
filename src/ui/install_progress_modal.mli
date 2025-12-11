(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type state

(** Open the installation progress modal.
    Returns the modal state which can be used to update progress. *)
val open_modal : has_snapshot:bool -> unit -> state

(** Mark a step as complete and advance to the next step. *)
val set_step_complete : state -> string -> unit

(** Update download progress (0-100). *)
val set_download_progress : state -> int -> unit

(** Add a log line to the modal (shown when logs are enabled). *)
val add_log_line : state -> string -> unit
