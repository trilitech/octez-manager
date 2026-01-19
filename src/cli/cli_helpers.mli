(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** CLI helper utilities for prompting, validation, and common operations *)

open Octez_manager_lib
module Term = Cmdliner.Term

(** Return cmdliner error with message *)
val cmdliner_error : string -> [> `Error of bool * string]

(** Resolve application binary directory from optional path or PATH *)
val resolve_app_bin_dir : string option -> (string, string) result

(** Check if running in interactive terminal *)
val is_interactive : unit -> bool

(** Normalize optional string (trim and convert empty to None) *)
val normalize_opt_string : string option -> string option

(** Prompt user for input with optional default value *)
val prompt_input : ?default:string * string -> string -> string option

(** Prompt for required string, repeating until valid input *)
val prompt_required_string : string -> string

(** Prompt for directory path *)
val prompt_directory : string -> string option

(** Prompt for required directory, repeating until valid *)
val prompt_required_directory : string -> string

(** Prompt with inline completion (internal copy, prefer prompt_with_completion) *)
val prompt_with_completion_inline : string -> string list -> string option

(** Prompt with completion list *)
val prompt_with_completion : string -> string list -> string option

(** Prompt with multi-value completion *)
val prompt_with_multi_completion : string -> string list -> string option

(** Prompt for history mode selection *)
val prompt_history_mode : History_mode.t -> History_mode.t

(** Prompt for yes/no with default *)
val prompt_yes_no : string -> default:bool -> bool

(** Resolve temporary directory for snapshot download *)
val resolve_tmp_dir_for_snapshot :
  snapshot_url:string -> tmp_dir:string option -> (string option, string) result

(** Check if data directory has enough space for snapshot *)
val check_data_dir_space :
  snapshot_url:string -> data_dir:string -> (unit, string) result

(** Validate and resolve port address, checking for conflicts *)
val validate_port_addr :
  label:string ->
  addr:string ->
  default:string ->
  ?exclude_instance:string ->
  unit ->
  (string, string) result

(** Resolve node instance or endpoint specification *)
val resolve_node_instance_or_endpoint :
  node_instance:string option ->
  ([> `Endpoint of string | `Instance of string], [`Msg of string]) result

(** Convert result to cmdliner result type *)
val run_result :
  (unit, [< `Msg of string]) result -> [> `Ok of unit | `Error of bool * string]

(** Cmdliner term for logging mode (always journald) *)
val logging_mode_term : Logging_mode.t Term.t

(** Documentation for history mode option *)
val history_mode_doc : string

(** History mode choices for cmdliner *)
val history_mode_choices : (string * History_mode.t) list

(** Cmdliner term for optional history mode *)
val history_mode_opt_term : History_mode.t option Term.t
