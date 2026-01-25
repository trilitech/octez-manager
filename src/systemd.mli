(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

include Service_backend.S

val unit_name : string -> string -> string

(** Detailed unit state from systemd *)
type unit_state = {
  active_state : string;  (** active, inactive, failed, etc. *)
  sub_state : string;  (** running, dead, failed, etc. *)
  result : string option;  (** exit-code, signal, timeout, etc. when failed *)
  exit_status : int option;  (** actual exit code if available *)
}

(** Get detailed unit state including failure information *)
val get_unit_state :
  role:string -> instance:string -> (unit_state, [`Msg of string]) result

val cat_unit :
  role:string -> instance:string -> (string, [`Msg of string]) result

val validate_bin_dir :
  user:string ->
  app_bin_dir:string ->
  role:string ->
  (unit, [`Msg of string]) result

(** [validate_binary_access] validates that a service user can execute a binary.
    This accepts the full binary path directly instead of deriving from role. *)
val validate_binary_access :
  user:string -> binary_path:string -> (unit, [`Msg of string]) result

val install_unit :
  ?quiet:bool ->
  role:string ->
  app_bin_dir:string ->
  user:string ->
  unit ->
  (unit, [`Msg of string]) result

val write_dropin :
  ?quiet:bool ->
  role:string ->
  inst:string ->
  data_dir:string ->
  logging_mode:Logging_mode.t ->
  ?extra_paths:string list ->
  ?depends_on:string * string ->
  unit ->
  (unit, [`Msg of string]) result

val write_dropin_node :
  ?quiet:bool ->
  inst:string ->
  data_dir:string ->
  logging_mode:Logging_mode.t ->
  unit ->
  (unit, [`Msg of string]) result

val remove_dropin : role:string -> instance:string -> unit

val get_service_paths : role:string -> instance:string -> (string * string) list

type logrotate_spec = {role : string; paths : string list}

val sync_logrotate : logrotate_spec list -> (unit, [`Msg of string]) result

module For_tests : sig
  val role_binary : string -> string

  val unit_name : string -> string -> string

  val system_unit_path : string -> string

  val user_unit_path : string -> string

  val unit_path : string -> string

  val dropin_dir : string -> string -> string

  val dropin_path : string -> string -> string

  val systemctl_cmd : unit -> string list

  val env_file_template : bool -> string

  val system_logrotate_config_path : string -> string

  val user_logrotate_root : unit -> string

  val user_logrotate_include_dir : unit -> string

  val user_logrotate_main_config : unit -> string

  val prestart_hooks_dir : unit -> string

  val prestart_script_path : string -> string

  val unit_template :
    role:string ->
    app_bin_dir:string ->
    user:string ->
    ?prestart:string ->
    unit ->
    string

  val render_logging_lines : Logging_mode.t -> string list

  val exec_line : string -> string

  (** Parse systemd show output string into unit_state (for testing) *)
  val parse_unit_state_output : string -> unit_state
end

(** {2 Service Lifecycle} *)

(** Start a systemd unit by its full unit name (e.g., "octez-shadownet-baker.service").
    Timeout: 30 seconds. *)
val start_unit : unit_name:string -> (unit, [> `Msg of string]) result

(** Stop a systemd unit by its full unit name.
    Timeout: 30 seconds. *)
val stop_unit : unit_name:string -> (unit, [> `Msg of string]) result

(** Restart a systemd unit by its full unit name.
    Timeout: 60 seconds. *)
val restart_unit : unit_name:string -> (unit, [> `Msg of string]) result

(** Start a managed service instance. *)
val start :
  ?quiet:bool ->
  role:string ->
  instance:string ->
  unit ->
  (unit, [> `Msg of string]) result

(** Stop a managed service instance. *)
val stop :
  ?quiet:bool ->
  role:string ->
  instance:string ->
  unit ->
  (unit, [> `Msg of string]) result

(** Restart a managed service instance. *)
val restart :
  ?quiet:bool ->
  role:string ->
  instance:string ->
  unit ->
  (unit, [> `Msg of string]) result

(** Enable a systemd unit by its full unit name (e.g., "octez-node.service"). *)
val enable_unit : string -> (unit, [> `Msg of string]) result

(** Disable a systemd unit by its full unit name. *)
val disable_unit : string -> (unit, [> `Msg of string]) result

(** Enable a managed service instance. *)
val enable :
  ?quiet:bool ->
  role:string ->
  instance:string ->
  start_now:bool ->
  unit ->
  (unit, [> `Msg of string]) result

(** Disable a managed service instance. *)
val disable :
  ?quiet:bool ->
  role:string ->
  instance:string ->
  stop_now:bool ->
  unit ->
  (unit, [> `Msg of string]) result
