(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
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

  val unit_path : string -> string

  val dropin_dir : string -> string -> string

  val dropin_path : string -> string -> string

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
