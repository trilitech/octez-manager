(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Installer_types

[@@@warning "-32"]

val invalid_instance_name_chars_msg : string

val install_node : node_request -> (Service.t, [`Msg of string]) result

val install_daemon : daemon_request -> (Service.t, [`Msg of string]) result

val install_baker : baker_request -> (Service.t, [`Msg of string]) result

val install_signer : signer_request -> (Service.t, [`Msg of string]) result

val import_snapshot_for_instance :
  instance:string ->
  ?snapshot_uri:string ->
  ?snapshot_kind:string ->
  ?network:string ->
  ?history_mode:History_mode.t ->
  no_check:bool ->
  unit ->
  (unit, [`Msg of string]) result

val refresh_instance_from_snapshot :
  instance:string ->
  ?snapshot_uri:string ->
  ?snapshot_kind:string ->
  ?network:string ->
  ?history_mode:History_mode.t ->
  no_check:bool ->
  unit ->
  (unit, [`Msg of string]) result

val start_service : instance:string -> (unit, [`Msg of string]) result

val stop_service : instance:string -> (unit, [`Msg of string]) result

val restart_service : instance:string -> (unit, [`Msg of string]) result

val remove_service :
  delete_data_dir:bool -> instance:string -> (unit, [`Msg of string]) result

val purge_service : instance:string -> (unit, [`Msg of string]) result

val list_services : unit -> (Service.t list, [`Msg of string]) result

(** Find directories and files not associated with registered services. *)
val find_orphan_directories :
  unit -> (string list * string list, [`Msg of string]) result

(** Remove orphan directories and log files.
    @param dry_run If true, only report what would be removed without deleting.
    @return (removed_paths, errors) *)
val cleanup_orphans :
  dry_run:bool ->
  (string list * (string * string) list, [`Msg of string]) result

val schedule_refresh :
  instance:string ->
  frequency:string ->
  snapshot_kind:string ->
  no_check:bool ->
  (unit, [`Msg of string]) result

val unschedule_refresh : instance:string -> unit

val generate_secret_key :
  instance:string -> alias:string -> (unit, [`Msg of string]) result

val list_keys : instance:string -> (string, [`Msg of string]) result

val add_authorized_key :
  instance:string ->
  key:string ->
  name:string option ->
  (unit, [`Msg of string]) result

val import_snapshot_for_instance :
  instance:string ->
  ?snapshot_uri:string ->
  ?snapshot_kind:string ->
  ?network:string ->
  ?history_mode:History_mode.t ->
  no_check:bool ->
  unit ->
  (unit, [`Msg of string]) result

val refresh_instance_from_snapshot :
  instance:string ->
  ?snapshot_uri:string ->
  ?snapshot_kind:string ->
  ?network:string ->
  ?history_mode:History_mode.t ->
  ?on_download_progress:(int -> int option -> unit) ->
  no_check:bool ->
  unit ->
  (unit, [`Msg of string]) result

val endpoint_of_rpc : string -> string

module For_tests : sig
  type file_backup

  val validate_instance_name_chars :
    instance:string -> (unit, [`Msg of string]) result

  val validate_instance_name_unique :
    instance:string -> (unit, [`Msg of string]) result

  val validate_instance_name :
    instance:string -> (unit, [`Msg of string]) result

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
    snapshot_kind:string option ->
    (snapshot_resolution, [`Msg of string]) result

  val history_mode_matches :
    requested:History_mode.t -> snapshot_mode:string -> bool
end
