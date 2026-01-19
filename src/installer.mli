(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

open Installer_types

module For_tests : sig
  type file_backup

  val validate_instance_name_chars :
    instance:string -> (unit, [`Msg of string]) result

  val validate_instance_name_unique :
    instance:string -> (unit, [`Msg of string]) result

  val validate_instance_name :
    ?allow_existing:bool ->
    instance:string ->
    unit ->
    (unit, [`Msg of string]) result

  val ensure_logging_base_directory :
    owner:string ->
    group:string ->
    Logging_mode.t ->
    (unit, [`Msg of string]) result

  val remove_logging_artifacts :
    Logging_mode.t -> (unit, [`Msg of string]) result

  val should_drop_service_user :
    user:string -> remaining_services:Service.t list -> bool

  val backup_file_if_exists :
    path:string -> (file_backup option, [`Msg of string]) result

  val restore_backup :
    owner:string ->
    group:string ->
    file_backup option ->
    (unit, [`Msg of string]) result

  val normalize_data_dir : string -> string option -> string

  val build_run_args :
    network:string ->
    history_mode:History_mode.t ->
    rpc_addr:string ->
    net_addr:string ->
    extra_args:string list ->
    logging_mode:Logging_mode.t ->
    string

  val snapshot_plan_of_request :
    node_request -> (snapshot_plan, [`Msg of string]) result

  val snapshot_metadata_of_plan :
    no_check:bool -> snapshot_plan -> snapshot_metadata

  val strip_file_uri : string -> string option

  val is_http_url : string -> bool

  val is_file_uri : string -> bool

  val resolve_snapshot_download :
    network:string ->
    history_mode:History_mode.t ->
    (snapshot_resolution, [`Msg of string]) result

  val history_mode_matches :
    requested:History_mode.t -> snapshot_mode:string -> bool

  val known_baker_global_options : string list

  val split_baker_extra_args :
    app_bin_dir:string -> string list -> string list * string list
end
