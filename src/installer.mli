(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Installer_types

[@@@warning "-32"]

val invalid_instance_name_chars_msg : string

val resolve_from_data_dir :
  string ->
  ([`Path of string | `Data_dir of resolved_data_dir_config], string) result

val install_node :
  ?quiet:bool ->
  ?on_log:(string -> unit) ->
  node_request ->
  (Service.t, [`Msg of string]) result

val install_daemon :
  ?quiet:bool -> daemon_request -> (Service.t, [`Msg of string]) result

val install_baker :
  ?quiet:bool -> baker_request -> (Service.t, [`Msg of string]) result

val install_accuser :
  ?quiet:bool -> accuser_request -> (Service.t, [`Msg of string]) result

val start_service :
  ?quiet:bool -> instance:string -> unit -> (unit, [`Msg of string]) result

val stop_service :
  ?quiet:bool -> instance:string -> unit -> (unit, [`Msg of string]) result

(** Get stopped parent dependencies for an instance.
    Returns services in order: topmost parent first, so they can be started in sequence. *)
val get_stopped_dependencies :
  instance:string -> unit -> (Service.t list, [`Msg of string]) result

(** Get stopped dependents (children) for an instance. *)
val get_stopped_dependents :
  instance:string -> unit -> (Service.t list, [`Msg of string]) result

val restart_service :
  ?quiet:bool -> instance:string -> unit -> (unit, [`Msg of string]) result

val remove_service :
  ?quiet:bool ->
  delete_data_dir:bool ->
  instance:string ->
  unit ->
  (unit, [`Msg of string]) result

val purge_service :
  ?quiet:bool ->
  prompt_yes_no:(string -> default:bool -> bool) ->
  instance:string ->
  unit ->
  (unit, [`Msg of string]) result

val list_services : unit -> (Service.t list, [`Msg of string]) result

(** Clean up old instance after rename.
    Stops and disables old service, removes dropin and registry entry,
    updates dependent env files to point to new instance name,
    and transfers dependents list to the new service.
    Does NOT delete data directory (data is preserved in the renamed instance). *)
val cleanup_renamed_instance :
  ?quiet:bool ->
  old_instance:string ->
  new_instance:string ->
  unit ->
  (unit, [`Msg of string]) result

(** Clean up stale dependency entries.
    Scans all services and removes dependents that no longer exist in the registry.
    @return Number of stale entries removed *)
val cleanup_dependencies : unit -> (int, [`Msg of string]) result

(** Find directories and files not associated with registered services. *)
val find_orphan_directories :
  unit -> (string list * string list, [`Msg of string]) result

(** Remove orphan directories and log files.
    @param dry_run If true, only report what would be removed without deleting.
    @return (removed_paths, errors) *)
val cleanup_orphans :
  dry_run:bool ->
  (string list * (string * string) list, [`Msg of string]) result

val endpoint_of_rpc : string -> string

(** Resolve snapshot download URL from tzinit for a given network and history mode.
    Used for space checks before downloading. *)
val resolve_snapshot_download :
  network:string ->
  history_mode:History_mode.t ->
  (snapshot_resolution, [`Msg of string]) result

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
end
