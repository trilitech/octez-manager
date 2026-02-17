(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Systemd drop-in override generation and installation.

    This module handles generating systemd drop-in configuration files for
    per-instance overrides (data directory, logging, read-write paths, etc.). *)

(** Resources needed for a particular logging configuration. *)
type logging_resources = {extra_lines : string list; extra_paths : string list}

(** Return the logging resources for a given role and logging mode.
    Currently always returns journald directives. *)
val logging_resources :
  role:string -> logging_mode:Logging_mode.t -> logging_resources

(** Remove duplicates and empty strings from a path list. *)
val unique_non_empty : string list -> string list

(** Compute the [ReadWritePaths=] entries for a drop-in. *)
val read_write_paths_for :
  data_dir:string ->
  logging_paths:string list ->
  extra_paths:string list ->
  string list

(** Generate the textual content of a systemd drop-in override file.
    When [~app_bin_dir] is provided, an [Environment=APP_BIN_DIR=...] line
    is emitted so the per-instance dropin overrides the shared template.
    
    The [~depends_on] parameter accepts a list of (role, instance) tuples,
    generating [BindsTo=] and [After=] directives for each dependency. *)
val write_dropin_body :
  role:string ->
  data_dir:string ->
  logging_mode:Logging_mode.t ->
  extra_paths:string list ->
  ?app_bin_dir:string ->
  ?depends_on:(string * string) list ->
  unit ->
  string

(** Write a systemd drop-in override file for a service instance.

    The [~dropin_dir], [~dropin_path], and [~daemon_reload] callbacks are
    injected by the caller (typically {!Systemd}) to avoid a circular
    dependency.

    When [~app_bin_dir] is provided, the dropin includes
    [Environment=APP_BIN_DIR=...] to override the shared template's value
    on a per-instance basis.
    
    The [~depends_on] parameter accepts a list of (role, instance) tuples,
    generating [BindsTo=] and [After=] directives for each dependency. *)
val write_dropin :
  ?quiet:bool ->
  dropin_dir:(string -> string -> string) ->
  dropin_path:(string -> string -> string) ->
  daemon_reload:(quiet:bool -> (unit, [`Msg of string]) result) ->
  role:string ->
  inst:string ->
  data_dir:string ->
  logging_mode:Logging_mode.t ->
  ?extra_paths:string list ->
  ?app_bin_dir:string ->
  ?depends_on:(string * string) list ->
  unit ->
  (unit, [`Msg of string]) result

(** Write a systemd drop-in override file specifically for a node instance. *)
val write_dropin_node :
  ?quiet:bool ->
  dropin_dir:(string -> string -> string) ->
  dropin_path:(string -> string -> string) ->
  daemon_reload:(quiet:bool -> (unit, [`Msg of string]) result) ->
  inst:string ->
  data_dir:string ->
  logging_mode:Logging_mode.t ->
  ?app_bin_dir:string ->
  unit ->
  (unit, [`Msg of string]) result

(** Return the systemd log output directives for a given logging mode. *)
val render_logging_lines : Logging_mode.t -> string list
