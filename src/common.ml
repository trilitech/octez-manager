(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

(** Thin re-export wrapper.

    All logic has moved to focused submodules in [lib/common/]:
    {ul
      {- {!Paths} -- filesystem paths, XDG dirs, user identity}
      {- {!Cmd_runner} -- shell command execution}
      {- {!File_ops} -- file and directory manipulation}
      {- {!Download} -- HTTP downloads and checksums}
      {- {!String_utils} -- formatting, sleep, editor, string helpers}}

    Callers should migrate to the submodules directly.  This wrapper
    will be removed once all call-sites are updated. *)

(* -- Paths ---------------------------------------------------------------- *)

let is_root = Paths.is_root

let home_dir = Paths.home_dir

let xdg_config_home = Paths.xdg_config_home

let xdg_data_home = Paths.xdg_data_home

let xdg_state_home = Paths.xdg_state_home

let current_user_group_names = Paths.current_user_group_names

let env_instances_base_dir = Paths.env_instances_base_dir

let registry_root = Paths.registry_root

let default_data_dir = Paths.default_data_dir

let default_role_dir = Paths.default_role_dir

let default_log_dir = Paths.default_log_dir

let which = Paths.which

let make_absolute_path = Paths.make_absolute_path

(* -- Cmd_runner ----------------------------------------------------------- *)

let append_debug_log = Cmd_runner.append_debug_log

let sh_quote = Cmd_runner.sh_quote

let cmd_to_string = Cmd_runner.cmd_to_string

let run = Cmd_runner.run

let run_silent = Cmd_runner.run_silent

let run_streaming = Cmd_runner.run_streaming

let run_verbose = Cmd_runner.run_verbose

let run_out = Cmd_runner.run_out

let run_out_silent = Cmd_runner.run_out_silent

let run_as = Cmd_runner.run_as

(* -- File_ops ------------------------------------------------------------- *)

let ensure_dir_path = File_ops.ensure_dir_path

let write_file = File_ops.write_file

let with_file_lock = File_ops.with_file_lock

let ensure_tree_owner = File_ops.ensure_tree_owner

let remove_path = File_ops.remove_path

let remove_tree = File_ops.remove_tree

let copy_file = File_ops.copy_file

let get_available_space = File_ops.get_available_space

let get_filesystem_id = File_ops.get_filesystem_id

let same_filesystem = File_ops.same_filesystem

let get_dir_size = File_ops.get_dir_size

(* -- Download ------------------------------------------------------------- *)

let download_file = Download.download_file

let download_file_with_progress = Download.download_file_with_progress

let kill_active_download = Download.kill_active_download

let get_remote_file_size = Download.get_remote_file_size

let compute_sha256 = Download.compute_sha256

(* -- String_utils --------------------------------------------------------- *)

let interruptible_sleep = String_utils.interruptible_sleep

let octez_exit_code_description = String_utils.octez_exit_code_description

let get_editor = String_utils.get_editor

let open_in_editor = String_utils.open_in_editor

let string_contains = String_utils.string_contains

let now = String_utils.now

let format_size = String_utils.format_size

let format_bytes = String_utils.format_bytes

let unquote = String_utils.unquote

let format_size_float = String_utils.format_size_float
