(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type log_source =
  | Journald  (** System journal logs via journalctl *)
  | DailyLogs  (** File-based logs in data_dir/daily_logs *)

(** Read logs for a given service instance *)
val read_logs :
  role:string ->
  instance:string ->
  source:log_source ->
  lines:int ->
  (string list, [> `Msg of string]) result

(** Get the path to the daily log file for an instance's data directory *)
val get_daily_log_file : data_dir:string -> (string, [> `Msg of string]) result
