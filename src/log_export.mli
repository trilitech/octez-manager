(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Export logs and diagnostic information for an instance.

    Creates a tar.gz archive containing:
    - Daily log files for the last 7 days
    - Journald logs for the last 7 days
    - Instance details and configuration
    - Binary version information
    - Related service versions (dependencies and dependents) *)

(** [export_logs ~instance ~svc] exports logs and diagnostic information
    for the given instance. Returns the path to the created archive on success. *)
val export_logs :
  instance:string -> svc:Service.t -> (string, [> `Msg of string]) result
