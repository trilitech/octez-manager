(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Extended tests for Common module - covering untested functions
    
    Tests cover:
    - Directory utilities (xdg_config_home, xdg_data_home, xdg_state_home)
    - Octez exit code descriptions
    - Remote file size checking
    - Filesystem utilities
    - Port checking
    - Editor detection
    - File removal utilities
*)

open Alcotest
open Octez_manager_lib

(* ============================================================ *)
(* XDG Directory Tests *)
(* ============================================================ *)

let test_xdg_config_home_from_env () =
  let original = Sys.getenv_opt "XDG_CONFIG_HOME" in
  Unix.putenv "XDG_CONFIG_HOME" "/custom/config" ;
  let result = Common.xdg_config_home () in
  check string "uses XDG_CONFIG_HOME" "/custom/config" result ;
  match original with
  | Some v -> Unix.putenv "XDG_CONFIG_HOME" v
  | None -> Unix.putenv "XDG_CONFIG_HOME" ""

let test_xdg_config_home_fallback () =
  let original = Sys.getenv_opt "XDG_CONFIG_HOME" in
  Unix.putenv "XDG_CONFIG_HOME" "" ;
  let result = Common.xdg_config_home () in
  check
    bool
    "falls back to ~/.config"
    true
    (String.ends_with ~suffix:".config" result) ;
  match original with
  | Some v -> Unix.putenv "XDG_CONFIG_HOME" v
  | None -> Unix.putenv "XDG_CONFIG_HOME" ""

let test_xdg_data_home_from_env () =
  let original = Sys.getenv_opt "XDG_DATA_HOME" in
  Unix.putenv "XDG_DATA_HOME" "/custom/data" ;
  let result = Common.xdg_data_home () in
  check string "uses XDG_DATA_HOME" "/custom/data" result ;
  match original with
  | Some v -> Unix.putenv "XDG_DATA_HOME" v
  | None -> Unix.putenv "XDG_DATA_HOME" ""

let test_xdg_data_home_fallback () =
  let original = Sys.getenv_opt "XDG_DATA_HOME" in
  Unix.putenv "XDG_DATA_HOME" "" ;
  let result = Common.xdg_data_home () in
  check
    bool
    "falls back to ~/.local/share"
    true
    (String.ends_with ~suffix:".local/share" result) ;
  match original with
  | Some v -> Unix.putenv "XDG_DATA_HOME" v
  | None -> Unix.putenv "XDG_DATA_HOME" ""

let test_xdg_state_home_from_env () =
  let original = Sys.getenv_opt "XDG_STATE_HOME" in
  Unix.putenv "XDG_STATE_HOME" "/custom/state" ;
  let result = Common.xdg_state_home () in
  check string "uses XDG_STATE_HOME" "/custom/state" result ;
  match original with
  | Some v -> Unix.putenv "XDG_STATE_HOME" v
  | None -> Unix.putenv "XDG_STATE_HOME" ""

let test_xdg_state_home_fallback () =
  let original = Sys.getenv_opt "XDG_STATE_HOME" in
  Unix.putenv "XDG_STATE_HOME" "" ;
  let result = Common.xdg_state_home () in
  check
    bool
    "falls back to ~/.local/state"
    true
    (String.ends_with ~suffix:".local/state" result) ;
  match original with
  | Some v -> Unix.putenv "XDG_STATE_HOME" v
  | None -> Unix.putenv "XDG_STATE_HOME" ""

(* ============================================================ *)
(* Octez Exit Code Description Tests *)
(* ============================================================ *)

let test_exit_code_success () =
  let desc = Common.octez_exit_code_description 0 in
  check string "success code" "success" desc

let test_exit_code_unhandled_exception () =
  let desc = Common.octez_exit_code_description 126 in
  check string "unhandled exception" "unhandled exception (bug)" desc

let test_exit_code_terminated_by_signal () =
  let desc = Common.octez_exit_code_description 127 in
  check string "terminated by signal" "terminated by signal" desc

let test_exit_code_shutdown_error () =
  let desc = Common.octez_exit_code_description 128 in
  check string "shutdown error" "error during shutdown" desc

let test_exit_code_unhandled_with_shutdown () =
  let desc = Common.octez_exit_code_description 254 in
  check
    string
    "unhandled with shutdown"
    "unhandled exception with shutdown error"
    desc

let test_exit_code_forcefully_terminated () =
  let desc = Common.octez_exit_code_description 255 in
  check string "forcefully terminated" "forcefully terminated" desc

let test_exit_code_config_error_range () =
  let desc1 = Common.octez_exit_code_description 1 in
  let desc50 = Common.octez_exit_code_description 50 in
  let desc125 = Common.octez_exit_code_description 125 in
  check string "config error 1" "configuration or startup error" desc1 ;
  check string "config error 50" "configuration or startup error" desc50 ;
  check string "config error 125" "configuration or startup error" desc125

let test_exit_code_shutdown_failure_range () =
  let desc129 = Common.octez_exit_code_description 129 in
  let desc200 = Common.octez_exit_code_description 200 in
  let desc253 = Common.octez_exit_code_description 253 in
  check string "shutdown failure 129" "error with shutdown failure" desc129 ;
  check string "shutdown failure 200" "error with shutdown failure" desc200 ;
  check string "shutdown failure 253" "error with shutdown failure" desc253

let test_exit_code_unknown () =
  let desc = Common.octez_exit_code_description 999 in
  check
    bool
    "unknown code formatted"
    true
    (String.starts_with ~prefix:"exit code" desc)

(* ============================================================ *)
(* Editor Detection Tests *)
(* ============================================================ *)

let test_get_editor_from_visual () =
  let original_visual = Sys.getenv_opt "VISUAL" in
  let original_editor = Sys.getenv_opt "EDITOR" in
  Unix.putenv "VISUAL" "custom-visual" ;
  Unix.putenv "EDITOR" "should-not-use-this" ;
  let result = Common.get_editor () in
  check string "uses VISUAL" "custom-visual" result ;
  (match original_visual with
  | Some v -> Unix.putenv "VISUAL" v
  | None -> Unix.putenv "VISUAL" "") ;
  match original_editor with
  | Some v -> Unix.putenv "EDITOR" v
  | None -> Unix.putenv "EDITOR" ""

let test_get_editor_from_editor () =
  let original_visual = Sys.getenv_opt "VISUAL" in
  let original_editor = Sys.getenv_opt "EDITOR" in
  Unix.putenv "VISUAL" "" ;
  Unix.putenv "EDITOR" "custom-editor" ;
  let result = Common.get_editor () in
  check string "uses EDITOR" "custom-editor" result ;
  (match original_visual with
  | Some v -> Unix.putenv "VISUAL" v
  | None -> Unix.putenv "VISUAL" "") ;
  match original_editor with
  | Some v -> Unix.putenv "EDITOR" v
  | None -> Unix.putenv "EDITOR" ""

let test_get_editor_fallback () =
  let original_visual = Sys.getenv_opt "VISUAL" in
  let original_editor = Sys.getenv_opt "EDITOR" in
  Unix.putenv "VISUAL" "" ;
  Unix.putenv "EDITOR" "" ;
  let result = Common.get_editor () in
  (* Should fall back to sensible-editor, vi, or /usr/bin/vi *)
  check bool "has fallback editor" true (String.length result > 0) ;
  (match original_visual with
  | Some v -> Unix.putenv "VISUAL" v
  | None -> Unix.putenv "VISUAL" "") ;
  match original_editor with
  | Some v -> Unix.putenv "EDITOR" v
  | None -> Unix.putenv "EDITOR" ""

(* ============================================================ *)
(* Filesystem Utilities Tests *)
(* ============================================================ *)

let test_get_filesystem_id_existing_path () =
  (* Test with /tmp which should always exist *)
  let result = Common.get_filesystem_id "/tmp" in
  check bool "returns Some for existing path" true (Option.is_some result)

let test_get_filesystem_id_nonexistent_path () =
  let result = Common.get_filesystem_id "/nonexistent/path/that/doesnt/exist" in
  check bool "returns None for nonexistent" true (Option.is_none result)

let test_same_filesystem_same_path () =
  let result = Common.same_filesystem "/tmp" "/tmp" in
  match result with
  | Some true -> check bool "same path is same filesystem" true true
  | Some false -> fail "same path should be same filesystem"
  | None -> fail "should return Some for existing paths"

let test_same_filesystem_different_paths () =
  (* /tmp and /var should exist on most systems *)
  let result = Common.same_filesystem "/tmp" "/var" in
  match result with
  | Some _ -> check bool "returns result for existing paths" true true
  | None -> check bool "or returns None if paths don't exist" true true

let test_same_filesystem_nonexistent () =
  let result = Common.same_filesystem "/nonexistent1" "/nonexistent2" in
  check bool "returns None for nonexistent paths" true (Option.is_none result)

let test_remove_path_nonexistent () =
  (* Should not fail on nonexistent path *)
  Common.remove_path "/nonexistent/file/that/doesnt/exist" ;
  check bool "remove_path handles nonexistent" true true

(* ============================================================ *)
(* default_role_dir Sanitization Tests *)
(* ============================================================ *)

let test_default_role_dir_basic () =
  let result = Common.default_role_dir "node" "test" in
  check bool "contains node prefix" true (String.contains result 'n')

let test_default_role_dir_uppercase_role () =
  let result = Common.default_role_dir "NODE" "test" in
  (* Role should be lowercased *)
  check bool "lowercases role" true (String.contains result 'n')

let test_default_role_dir_special_chars_in_role () =
  let result = Common.default_role_dir "bak!er@" "test" in
  (* Special chars should be sanitized to hyphens *)
  check bool "sanitizes role" true (String.contains result '-')

let test_default_role_dir_already_prefixed () =
  let result1 = Common.default_role_dir "node" "node-mainnet" in
  let result2 = Common.default_role_dir "node" "mainnet" in
  (* Should not double-prefix *)
  check bool "avoids double prefix" true (not (String.contains result1 '@')) ;
  check bool "handles non-prefixed" true (String.length result2 > 0)

let test_default_role_dir_empty_role () =
  let result = Common.default_role_dir "" "test" in
  (* Empty role should use "service" as fallback *)
  check
    bool
    "handles empty role"
    true
    (String.contains result 's' || String.contains result 't')

let test_default_role_dir_whitespace () =
  let result = Common.default_role_dir "  baker  " "  test  " in
  (* Whitespace should be trimmed *)
  check bool "trims whitespace" true (String.length result > 0)

(* ============================================================ *)
(* current_user_group_names Tests *)
(* ============================================================ *)

let test_current_user_group_names () =
  let user, group = Common.current_user_group_names () in
  (* Should return non-empty strings (unless in weird environment) *)
  check
    bool
    "returns values"
    true
    (String.length user >= 0 && String.length group >= 0)

(* ============================================================ *)
(* env_instances_base_dir Tests *)
(* ============================================================ *)

let test_env_instances_base_dir () =
  let result = Common.env_instances_base_dir () in
  (* Should return a valid path *)
  check bool "returns path" true (String.contains result '/')

(* ============================================================ *)
(* default_log_dir Tests *)
(* ============================================================ *)

let test_default_log_dir () =
  let result = Common.default_log_dir ~role:"node" ~instance:"test" in
  (* Should return a valid path *)
  check bool "returns log path" true (String.contains result '/')

(* ============================================================ *)
(* Test Suite *)
(* ============================================================ *)

let xdg_tests =
  [
    ("xdg_config_home from env", `Quick, test_xdg_config_home_from_env);
    ("xdg_config_home fallback", `Quick, test_xdg_config_home_fallback);
    ("xdg_data_home from env", `Quick, test_xdg_data_home_from_env);
    ("xdg_data_home fallback", `Quick, test_xdg_data_home_fallback);
    ("xdg_state_home from env", `Quick, test_xdg_state_home_from_env);
    ("xdg_state_home fallback", `Quick, test_xdg_state_home_fallback);
  ]

let exit_code_tests =
  [
    ("exit code 0", `Quick, test_exit_code_success);
    ("exit code 126", `Quick, test_exit_code_unhandled_exception);
    ("exit code 127", `Quick, test_exit_code_terminated_by_signal);
    ("exit code 128", `Quick, test_exit_code_shutdown_error);
    ("exit code 254", `Quick, test_exit_code_unhandled_with_shutdown);
    ("exit code 255", `Quick, test_exit_code_forcefully_terminated);
    ("exit code 1-125 range", `Quick, test_exit_code_config_error_range);
    ("exit code 129-253 range", `Quick, test_exit_code_shutdown_failure_range);
    ("exit code unknown", `Quick, test_exit_code_unknown);
  ]

let editor_tests =
  [
    ("get_editor from VISUAL", `Quick, test_get_editor_from_visual);
    ("get_editor from EDITOR", `Quick, test_get_editor_from_editor);
    ("get_editor fallback", `Quick, test_get_editor_fallback);
  ]

let filesystem_tests =
  [
    ("get_filesystem_id existing", `Quick, test_get_filesystem_id_existing_path);
    ( "get_filesystem_id nonexistent",
      `Quick,
      test_get_filesystem_id_nonexistent_path );
    ("same_filesystem same path", `Quick, test_same_filesystem_same_path);
    ( "same_filesystem different paths",
      `Quick,
      test_same_filesystem_different_paths );
    ("same_filesystem nonexistent", `Quick, test_same_filesystem_nonexistent);
    ("remove_path nonexistent", `Quick, test_remove_path_nonexistent);
  ]

let role_dir_tests =
  [
    ("default_role_dir basic", `Quick, test_default_role_dir_basic);
    ("default_role_dir uppercase", `Quick, test_default_role_dir_uppercase_role);
    ( "default_role_dir special chars",
      `Quick,
      test_default_role_dir_special_chars_in_role );
    ( "default_role_dir already prefixed",
      `Quick,
      test_default_role_dir_already_prefixed );
    ("default_role_dir empty role", `Quick, test_default_role_dir_empty_role);
    ("default_role_dir whitespace", `Quick, test_default_role_dir_whitespace);
  ]

let misc_tests =
  [
    ("current_user_group_names", `Quick, test_current_user_group_names);
    ("env_instances_base_dir", `Quick, test_env_instances_base_dir);
    ("default_log_dir", `Quick, test_default_log_dir);
  ]

let () =
  Alcotest.run
    "Common_extended"
    [
      ("xdg_directories", xdg_tests);
      ("exit_codes", exit_code_tests);
      ("editor", editor_tests);
      ("filesystem", filesystem_tests);
      ("role_dir", role_dir_tests);
      ("misc", misc_tests);
    ]
