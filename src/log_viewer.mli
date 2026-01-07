(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type log_source =
  | Journald  (** System journal logs via journalctl *)
  | DailyLogs  (** File-based logs in data_dir/daily_logs *)

(** Get the path to the daily log file for a service instance.
    Different roles store logs in different locations:
    - node: <data_dir>/daily_logs/
    - baker: <base_dir>/logs/octez-baker/
    - accuser: <base_dir>/logs/octez-accuser/
    - dal-node: <data_dir>/daily_logs/
    - signer: <base_dir>/logs/octez-signer/ *)
val get_daily_log_file :
  role:string -> instance:string -> (string, [> `Msg of string]) result

(** Get the shell command to follow logs *)
val get_log_cmd :
  role:string ->
  instance:string ->
  source:log_source ->
  (string, [> `Msg of string]) result
