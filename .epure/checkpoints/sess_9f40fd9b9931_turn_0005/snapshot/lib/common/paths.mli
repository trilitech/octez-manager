(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Filesystem paths, XDG directories, and user identity. *)

(** [true] when the effective UID is 0. *)
val is_root : unit -> bool

(** Current user's home directory from [$HOME] or [getpwuid]. *)
val home_dir : unit -> string

(** [$XDG_CONFIG_HOME] or [~/.config]. *)
val xdg_config_home : unit -> string

(** [$XDG_DATA_HOME] or [~/.local/share]. *)
val xdg_data_home : unit -> string

(** [$XDG_STATE_HOME] or [~/.local/state]. *)
val xdg_state_home : unit -> string

(** Return [(username, groupname)] for the effective user. *)
val current_user_group_names : unit -> string * string

(** Base directory for per-instance env files. *)
val env_instances_base_dir : unit -> string

(** Root directory for octez-manager registries (services, directories).
    Returns ["/etc/octez_manager"] when running as root,
    otherwise ["$XDG_CONFIG_HOME/octez-manager"]. *)
val registry_root : unit -> string

(** Default data directory for an instance. *)
val default_data_dir : string -> string

(** Default data directory for a role/instance pair, with sanitization.
    If the instance name already starts with the role prefix, it is not
    duplicated. *)
val default_role_dir : string -> string -> string

(** Default log directory. Currently ignores role and instance. *)
val default_log_dir : role:string -> instance:string -> string

(** Search [$PATH] (plus common fallback directories) for an executable. *)
val which : string -> string option

(** Make a path absolute, resolving relative paths against [cwd]. *)
val make_absolute_path : string -> (string, string) result
