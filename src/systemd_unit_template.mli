(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Systemd unit file template generation and installation.

    This module handles generating systemd service unit files, prestart scripts,
    and binary validation. *)

(** Map a service role name to its Octez binary name.
    For example, ["node"] maps to ["octez-node"]. *)
val role_binary : string -> string

(** Generate the environment file path template for a systemd unit.
    The [%i] placeholder is replaced by the instance name at runtime. *)
val env_file_template : bool -> string

(** Generate the [ExecStart] line for a given service role. *)
val exec_line : string -> string

(** Return the directory used for prestart hook scripts. *)
val prestart_hooks_dir : unit -> string

(** Return the path to the prestart script for a given role. *)
val prestart_script_path : string -> string

(** The shell script body used as a node prestart hook. *)
val node_prestart_script_body : string

(** Write the prestart script for a given role.
    Returns [Ok (Some path)] for roles that have a prestart script (currently
    only ["node"]), or [Ok None] for roles that don't. *)
val write_prestart_script : string -> (string option, [`Msg of string]) result

(** Generate the full systemd unit file content for a service. *)
val unit_template :
  user_mode:bool ->
  role:string ->
  app_bin_dir:string ->
  user:string ->
  ?prestart:string ->
  unit ->
  string

(** Validate that the service user can execute the role's binary. *)
val validate_bin_dir :
  user:string ->
  app_bin_dir:string ->
  role:string ->
  (unit, [`Msg of string]) result

(** Validate that a service user can execute a binary at the given path.
    This is a convenience wrapper that accepts the full binary path directly
    instead of deriving it from a role. *)
val validate_binary_access :
  user:string -> binary_path:string -> (unit, [`Msg of string]) result

(** Install a systemd unit file for a service role.

    The [~unit_path] and [~daemon_reload] callbacks are injected by the
    caller (typically {!Systemd}) to avoid a circular dependency. *)
val install_unit :
  ?quiet:bool ->
  unit_path:(string -> string) ->
  daemon_reload:(quiet:bool -> (unit, [`Msg of string]) result) ->
  role:string ->
  app_bin_dir:string ->
  user:string ->
  unit ->
  (unit, [`Msg of string]) result
