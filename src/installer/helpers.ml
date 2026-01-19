(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Rresult
open Installer_types

let ( let* ) = Result.bind

let invalid_instance_name_chars_msg =
  "Only alphanumeric characters (a-z, A-Z, 0-9), hyphens (-), underscores (_), \
   and dots (.) are allowed."

let backup_file_if_exists path =
  let normalized = String.trim path in
  if normalized = "" then Ok None
  else if Sys.file_exists normalized then (
    let tmp = Filename.temp_file "octez-manager.backup" ".tmp" in
    match Common.copy_file normalized tmp with
    | Ok () -> Ok (Some {tmp_path = tmp; original_path = normalized})
    | Error _ as e ->
        Common.remove_path tmp ;
        e)
  else Ok None

let ensure_backup_parent ~owner ~group path =
  let dir = Filename.dirname path in
  if dir = "" || dir = "." then Ok ()
  else if Sys.file_exists dir then Ok ()
  else Common.ensure_dir_path ~owner ~group ~mode:0o755 dir

let restore_backup ~owner ~group backup_opt =
  match backup_opt with
  | None -> Ok ()
  | Some backup -> (
      let* () = ensure_backup_parent ~owner ~group backup.original_path in
      match Common.copy_file backup.tmp_path backup.original_path with
      | Ok () ->
          Common.remove_path backup.tmp_path ;
          Ok ()
      | Error _ as e -> e)

let normalize_optional_string = function
  | Some s ->
      let trimmed = String.trim s in
      if trimmed = "" then None else Some trimmed
  | None -> None

(* Logging is via journald - no logrotate needed *)
let logrotate_specs_of (_services : Service.t list) = []

(** Well-known global options for octez-baker/octez-client.
    These must appear before the subcommand. Used as fallback when dynamic
    discovery from --help fails. *)
let known_baker_global_options =
  [
    "-d";
    "--base-dir";
    "-c";
    "--config-file";
    "-f";
    "--password-filename";
    "-E";
    "--endpoint";
    "-S";
    "--sources";
    "-l";
    "--log-requests";
    "-M";
    "--media-type";
  ]

(** Discover global options from octez-baker --help and split extra args.
    Returns (global_args, command_args) where global_args must come before
    the subcommand and command_args come after.
    Falls back to known_baker_global_options if dynamic discovery fails. *)
let split_baker_extra_args ~app_bin_dir extra_args =
  if extra_args = [] then ([], [])
  else
    let global_options =
      let binary = Filename.concat app_bin_dir "octez-baker" in
      if not (Sys.file_exists binary) then known_baker_global_options
      else
        match Common.run_out [binary; "--help"] with
        | Error _ -> known_baker_global_options
        | Ok output ->
            let discovered =
              Help_parser.extract_baker_global_option_names output
            in
            if discovered = [] then known_baker_global_options else discovered
    in
    Help_parser.split_extra_args ~global_options extra_args

let is_http_url s =
  let trimmed = String.trim s |> String.lowercase_ascii in
  String.starts_with ~prefix:"http://" trimmed
  || String.starts_with ~prefix:"https://" trimmed

let is_file_uri s =
  let trimmed = String.trim s |> String.lowercase_ascii in
  String.starts_with ~prefix:"file://" trimmed

let strip_file_uri s =
  if is_file_uri s then
    let prefix_len = String.length "file://" in
    Some (String.sub s prefix_len (String.length s - prefix_len))
  else None
