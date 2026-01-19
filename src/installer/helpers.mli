(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Utility functions for the installer *)

open Installer_types

(** Result bind operator for convenient error handling *)
val ( let* ) : ('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result

(** Error message for invalid instance name characters *)
val invalid_instance_name_chars_msg : string

(** Backup a file if it exists, returning backup info or None *)
val backup_file_if_exists : string -> (file_backup option, Rresult.R.msg) result

(** Ensure parent directory exists for backup restoration *)
val ensure_backup_parent :
  owner:string -> group:string -> string -> (unit, Rresult.R.msg) result

(** Restore a file from backup *)
val restore_backup :
  owner:string ->
  group:string ->
  file_backup option ->
  (unit, Rresult.R.msg) result

(** Normalize optional string by trimming and converting empty to None *)
val normalize_optional_string : string option -> string option

(** Generate logrotate specs for services (currently returns empty list) *)
val logrotate_specs_of : Service.t list -> 'a list

(** Well-known global options for octez-baker/octez-client *)
val known_baker_global_options : string list

(** Split baker extra args into global args (before subcommand) and command args (after).
    Discovers global options from octez-baker --help output. *)
val split_baker_extra_args :
  app_bin_dir:string -> string list -> string list * string list

(** Check if string is an HTTP/HTTPS URL *)
val is_http_url : string -> bool

(** Check if string is a file:// URI *)
val is_file_uri : string -> bool

(** Strip file:// prefix from URI, returning the path *)
val strip_file_uri : string -> string option
