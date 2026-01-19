(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

open Installer_types
open Helpers
open Snapshot
open Config

let backup_file_if_exists_for_tests = backup_file_if_exists

let restore_backup_for_tests = restore_backup

module For_tests = struct
  type nonrec file_backup = file_backup

  let validate_instance_name_chars = validate_instance_name_chars

  let validate_instance_name_unique = validate_instance_name_unique

  let validate_instance_name = validate_instance_name

  let ensure_logging_base_directory = ensure_logging_base_directory

  let remove_logging_artifacts = remove_logging_artifacts

  let should_drop_service_user = should_drop_service_user

  let backup_file_if_exists ~path = backup_file_if_exists_for_tests path

  let restore_backup ~owner ~group backup =
    restore_backup_for_tests ~owner ~group backup

  let normalize_data_dir = normalize_data_dir

  let build_run_args = build_run_args

  let snapshot_plan_of_request = snapshot_plan_of_request

  let snapshot_metadata_of_plan = snapshot_metadata_of_plan

  let strip_file_uri = strip_file_uri

  let is_http_url = is_http_url

  let is_file_uri = is_file_uri

  let resolve_snapshot_download = resolve_snapshot_download

  let history_mode_matches = history_mode_matches

  let known_baker_global_options = known_baker_global_options

  let split_baker_extra_args = split_baker_extra_args
end
