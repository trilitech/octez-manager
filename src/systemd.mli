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

(** Read the effective systemd unit file content for a service via [systemctl cat]. *)
val cat_unit :
  role:string -> instance:string -> (string, [`Msg of string]) result

(** Validate that [user] can execute the role binary in [app_bin_dir].
    Returns [Error] if the binary is missing or not accessible. *)
val validate_bin_dir :
  user:string ->
  app_bin_dir:string ->
  role:string ->
  (unit, [`Msg of string]) result

(** [validate_binary_access] validates that a service user can execute a binary.
    This accepts the full binary path directly instead of deriving from role. *)
val validate_binary_access :
  user:string -> binary_path:string -> (unit, [`Msg of string]) result

(** Install the base systemd unit file for a role.
    Creates the [.service] file under the system or user unit directory. *)
val install_unit :
  ?quiet:bool ->
  role:string ->
  app_bin_dir:string ->
  user:string ->
  unit ->
  (unit, [`Msg of string]) result

(** Write a systemd drop-in override for an instance, configuring
    data directory, logging mode, extra paths, and dependencies.
    When [~app_bin_dir] is provided, the dropin includes
    [Environment=APP_BIN_DIR=...] to override the shared template value.
    
    The [~depends_on] parameter accepts a list of (role, instance) tuples,
    generating [BindsTo=] and [After=] directives for each dependency. *)
val write_dropin :
  ?quiet:bool ->
  role:string ->
  inst:string ->
  data_dir:string ->
  logging_mode:Logging_mode.t ->
  ?extra_paths:string list ->
  ?app_bin_dir:string ->
  ?depends_on:(string * string) list ->
  unit ->
  (unit, [`Msg of string]) result

(** Write a node-specific systemd drop-in (convenience wrapper around {!write_dropin}). *)
val write_dropin_node :
  ?quiet:bool ->
  inst:string ->
  data_dir:string ->
  logging_mode:Logging_mode.t ->
  ?app_bin_dir:string ->
  unit ->
  (unit, [`Msg of string]) result

(** Remove the drop-in directory for a service instance. *)
val remove_dropin : role:string -> instance:string -> unit

(** Clear the [StartLimitHit] failure state for a service.
    Safe to call on healthy services — silently ignored if not in failed state.
    Must be called before {!restart} when a service hit its restart limit. *)
val reset_failed :
  role:string -> instance:string -> unit -> (unit, [`Msg of string]) result

(** Return the list of filesystem paths (unit file, drop-in, env file)
    associated with a service instance as [(description, path)] pairs. *)
val get_service_paths : role:string -> instance:string -> (string * string) list

(** Build the systemctl command prefix, accounting for root vs user mode.
    Returns [["systemctl"]] when running as root,
    [["systemctl"; "--user"]] otherwise. *)
val systemctl_cmd : unit -> string list

module For_tests : sig
  val role_binary : string -> string

  (** Compute the systemd unit name for a role and instance
    (e.g. ["octez-mynet-node.service"]). *)
  val unit_name : string -> string -> string

  val system_unit_path : string -> string

  val user_unit_path : string -> string

  val unit_path : string -> string

  val dropin_dir : string -> string -> string

  val dropin_path : string -> string -> string

  val systemctl_cmd : unit -> string list

  val env_file_template : bool -> string

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

(** {2 Payout Timer Management} *)

(** Get the User property from a systemd unit. Returns None if not set or on error. *)
val get_service_user : role:string -> instance:string -> string option

(** Write the payout oneshot service unit file for a baker instance. *)
val write_payout_service :
  instance:string ->
  octez_manager_bin:string ->
  service_user:string option ->
  unit ->
  (unit, [`Msg of string]) result

(** Write the payout timer unit file for a baker instance. *)
val write_payout_timer :
  instance:string -> unit -> (unit, [`Msg of string]) result

(** Enable and start the payout timer. *)
val enable_payout_timer : instance:string -> (unit, [`Msg of string]) result

(** Disable and stop the payout timer. *)
val disable_payout_timer : instance:string -> (unit, [`Msg of string]) result

(** Remove payout timer and service unit files. *)
val remove_payout_units : instance:string -> unit

(** Check if payout timer is active. *)
val is_payout_timer_active : instance:string -> bool

(** Get payout timer status info (for display). Returns None if timer doesn't exist. *)
val payout_timer_status : instance:string -> string option
