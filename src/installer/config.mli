(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Configuration and validation utilities for service installation *)

(** Ensure node config.json exists, creating it if needed *)
val ensure_node_config :
  ?quiet:bool ->
  app_bin_dir:string ->
  data_dir:string ->
  network:string ->
  history_mode:History_mode.t ->
  unit ->
  (unit, Rresult.R.msg) result

(** Normalize data directory path, using default if not provided *)
val normalize_data_dir : string -> string option -> string

(** Convert RPC address to full endpoint URL *)
val endpoint_of_rpc : string -> string

(** Look up a node service by instance name *)
val lookup_node_service : string -> (Service.t, Rresult.R.msg) result

(** Build command-line arguments for node run *)
val build_run_args :
  network:string ->
  history_mode:History_mode.t ->
  rpc_addr:string ->
  net_addr:string ->
  extra_args:string list ->
  logging_mode:Logging_mode.t ->
  string

(** Prepare logging mode (always returns journald) *)
val prepare_logging :
  instance:string ->
  role:string ->
  logging_mode:Logging_mode.t ->
  Logging_mode.t

(** Ensure logging destination exists (no-op for journald) *)
val ensure_logging_destination :
  service_user:string -> Logging_mode.t -> (unit, Rresult.R.msg) result

(** Ensure logging base directory exists (no-op for journald) *)
val ensure_logging_base_directory :
  owner:string -> group:string -> Logging_mode.t -> (unit, Rresult.R.msg) result

(** Remove logging artifacts (no-op for journald) *)
val remove_logging_artifacts : Logging_mode.t -> (unit, Rresult.R.msg) result

(** Check if service user should be dropped after service removal *)
val should_drop_service_user :
  user:string -> remaining_services:Service.t list -> bool

(** Ensure runtime log directory exists (no-op for journald) *)
val ensure_runtime_log_directory :
  owner:string -> group:string -> Logging_mode.t -> (unit, Rresult.R.msg) result

(** Ensure multiple directories exist with proper ownership *)
val ensure_directories :
  owner:string -> group:string -> string list -> (unit, Rresult.R.msg) result

(** Recursively reown runtime paths *)
val reown_runtime_paths :
  owner:string ->
  group:string ->
  paths:string list ->
  logging_mode:Logging_mode.t ->
  (unit, Rresult.R.msg) result

(** Check if character is valid in instance name *)
val is_valid_instance_char : char -> bool

(** Validate instance name contains only valid characters *)
val validate_instance_name_chars :
  instance:string -> (unit, Rresult.R.msg) result

(** Validate instance name is not already in use *)
val validate_instance_name_unique :
  instance:string -> (unit, Rresult.R.msg) result

(** Validate instance name (chars + uniqueness) *)
val validate_instance_name :
  ?allow_existing:bool ->
  instance:string ->
  unit ->
  (unit, Rresult.R.msg) result

(** Resolve network configuration from existing data directory *)
val resolve_from_data_dir :
  string ->
  ( [`Path of string | `Data_dir of Installer_types.resolved_data_dir_config],
    string )
  result

(** Update endpoint references in dependent services when RPC address changes *)
val update_dependent_endpoints :
  instance:string ->
  role:string ->
  new_rpc_addr:string ->
  unit ->
  (unit, Rresult.R.msg) result
