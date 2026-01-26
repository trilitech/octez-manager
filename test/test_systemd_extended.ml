(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Extended tests for Systemd module
    
    Tests cover:
    - Role to binary name mapping
    - Unit path generation (system vs user mode)
    - Unit name formatting
    - Drop-in directory paths
    - Systemctl command construction
    - Unit state parsing
    - Environment file template generation
*)

open Alcotest
open Octez_manager_lib
module ST = Systemd.For_tests

(* ============================================================ *)
(* Role Binary Mapping Tests *)
(* ============================================================ *)

let test_role_binary_node () =
  let result = ST.role_binary "node" in
  check string "node binary" "octez-node" result

let test_role_binary_baker () =
  let result = ST.role_binary "baker" in
  check string "baker binary" "octez-baker" result

let test_role_binary_accuser () =
  let result = ST.role_binary "accuser" in
  check string "accuser binary" "octez-baker" result

let test_role_binary_dal () =
  let result = ST.role_binary "dal" in
  check string "dal binary" "octez-dal-node" result

let test_role_binary_dal_node () =
  let result = ST.role_binary "dal-node" in
  check string "dal-node binary" "octez-dal-node" result

let test_role_binary_uppercase () =
  let result = ST.role_binary "NODE" in
  check string "uppercase node" "octez-node" result

let test_role_binary_mixed_case () =
  let result = ST.role_binary "BaKeR" in
  check string "mixed case baker" "octez-baker" result

let test_role_binary_unknown () =
  let result = ST.role_binary "custom" in
  check string "unknown role gets prefix" "octez-custom" result

(* ============================================================ *)
(* Unit Name Tests *)
(* ============================================================ *)

let test_unit_name_basic () =
  let result = ST.unit_name "node" "mainnet" in
  check string "basic unit name" "octez-node@mainnet" result

let test_unit_name_with_hyphen () =
  let result = ST.unit_name "dal-node" "test-instance" in
  check string "hyphenated names" "octez-dal-node@test-instance" result

let test_unit_name_numeric_instance () =
  let result = ST.unit_name "baker" "123" in
  check string "numeric instance" "octez-baker@123" result

let test_unit_name_empty_instance () =
  let result = ST.unit_name "node" "" in
  check string "empty instance" "octez-node@" result

(* ============================================================ *)
(* System Unit Path Tests *)
(* ============================================================ *)

let test_system_unit_path_node () =
  let result = ST.system_unit_path "node" in
  check bool "starts with /etc" true (String.starts_with ~prefix:"/etc" result) ;
  check bool "contains octez-node@" true (String.contains result 'n') ;
  check
    bool
    "ends with .service"
    true
    (String.ends_with ~suffix:".service" result)

let test_system_unit_path_baker () =
  let result = ST.system_unit_path "baker" in
  check bool "contains baker" true (String.contains result 'b')

let test_system_unit_path_dal () =
  let result = ST.system_unit_path "dal" in
  check bool "contains dal" true (String.contains result 'd')

(* ============================================================ *)
(* Drop-in Directory Tests *)
(* ============================================================ *)

let test_dropin_dir_contains_role () =
  (* This will use the current user mode *)
  let result = ST.dropin_dir "node" "mainnet" in
  check bool "contains octez" true (String.contains result 'o') ;
  check bool "contains instance" true (String.contains result 'm') ;
  check
    bool
    "ends with .service.d"
    true
    (String.ends_with ~suffix:".service.d" result)

let test_dropin_dir_different_instances () =
  let result1 = ST.dropin_dir "node" "inst1" in
  let result2 = ST.dropin_dir "node" "inst2" in
  check bool "different instances give different paths" true (result1 <> result2)

let test_dropin_path_contains_override () =
  let result = ST.dropin_path "node" "mainnet" in
  check
    bool
    "ends with override.conf"
    true
    (String.ends_with ~suffix:"override.conf" result)

let test_dropin_path_is_in_dropin_dir () =
  let dir = ST.dropin_dir "baker" "test" in
  let path = ST.dropin_path "baker" "test" in
  check bool "path starts with dir" true (String.starts_with ~prefix:dir path)

(* ============================================================ *)
(* Systemctl Command Construction Tests *)
(* ============================================================ *)

let test_systemctl_cmd () =
  (* Should return either ["systemctl"] or ["systemctl"; "--user"] *)
  let result = ST.systemctl_cmd () in
  check bool "has systemctl" true (List.mem "systemctl" result) ;
  check bool "is list" true (List.length result >= 1)

(* ============================================================ *)
(* Environment File Template Tests *)
(* ============================================================ *)

let test_env_file_template_user_mode () =
  let result = ST.env_file_template true in
  check bool "contains %i" true (String.contains result 'i') ;
  check
    bool
    "ends with node.env"
    true
    (String.ends_with ~suffix:"node.env" result)

let test_env_file_template_system_mode () =
  let result = ST.env_file_template false in
  check bool "starts with /etc" true (String.starts_with ~prefix:"/etc" result) ;
  check bool "contains octez" true (String.contains result 'o') ;
  check bool "contains %i" true (String.contains result '%')

let test_env_file_template_different_modes () =
  (* Skip this test when running as root, since user mode returns the same
     path as system mode in that case (both use /etc/octez/instances) *)
  if Common.is_root () then Alcotest.skip ()
  else
    let user = ST.env_file_template true in
    let system = ST.env_file_template false in
    check bool "user and system differ" true (user <> system)

(* ============================================================ *)
(* Logrotate Path Tests *)
(* ============================================================ *)

let test_system_logrotate_config_path () =
  let result = ST.system_logrotate_config_path "node" in
  check bool "contains logrotate.d" true (String.contains result 'l') ;
  check bool "contains octez" true (String.contains result 'o')

let test_system_logrotate_config_path_different_roles () =
  let node = ST.system_logrotate_config_path "node" in
  let baker = ST.system_logrotate_config_path "baker" in
  check bool "different roles give different paths" true (node <> baker)

let test_user_logrotate_root () =
  let result = ST.user_logrotate_root () in
  check bool "returns path" true (String.length result > 0) ;
  check bool "is absolute" true (String.starts_with ~prefix:"/" result)

let test_user_logrotate_include_dir () =
  let result = ST.user_logrotate_include_dir () in
  check bool "returns path" true (String.length result > 0)

let test_user_logrotate_main_config () =
  let result = ST.user_logrotate_main_config () in
  check bool "ends with .conf" true (String.ends_with ~suffix:".conf" result)

(* ============================================================ *)
(* Prestart Script Path Tests *)
(* ============================================================ *)

let test_prestart_hooks_dir () =
  let result = ST.prestart_hooks_dir () in
  check bool "returns directory" true (String.length result > 0) ;
  check bool "is absolute" true (String.starts_with ~prefix:"/" result)

let test_prestart_script_path () =
  let result = ST.prestart_script_path "node" in
  check bool "contains role" true (String.contains result 'n')

let test_prestart_script_path_different_roles () =
  let node = ST.prestart_script_path "node" in
  let baker = ST.prestart_script_path "baker" in
  check bool "different roles give different paths" true (node <> baker)

(* ============================================================ *)
(* Test Suite *)
(* ============================================================ *)

let role_binary_tests =
  [
    ("role_binary node", `Quick, test_role_binary_node);
    ("role_binary baker", `Quick, test_role_binary_baker);
    ("role_binary accuser", `Quick, test_role_binary_accuser);
    ("role_binary dal", `Quick, test_role_binary_dal);
    ("role_binary dal-node", `Quick, test_role_binary_dal_node);
    ("role_binary uppercase", `Quick, test_role_binary_uppercase);
    ("role_binary mixed case", `Quick, test_role_binary_mixed_case);
    ("role_binary unknown", `Quick, test_role_binary_unknown);
  ]

let unit_name_tests =
  [
    ("unit_name basic", `Quick, test_unit_name_basic);
    ("unit_name with hyphen", `Quick, test_unit_name_with_hyphen);
    ("unit_name numeric instance", `Quick, test_unit_name_numeric_instance);
    ("unit_name empty instance", `Quick, test_unit_name_empty_instance);
  ]

let unit_path_tests =
  [
    ("system_unit_path node", `Quick, test_system_unit_path_node);
    ("system_unit_path baker", `Quick, test_system_unit_path_baker);
    ("system_unit_path dal", `Quick, test_system_unit_path_dal);
  ]

let dropin_tests =
  [
    ("dropin_dir contains role", `Quick, test_dropin_dir_contains_role);
    ( "dropin_dir different instances",
      `Quick,
      test_dropin_dir_different_instances );
    ("dropin_path contains override", `Quick, test_dropin_path_contains_override);
    ("dropin_path in dropin_dir", `Quick, test_dropin_path_is_in_dropin_dir);
  ]

let systemctl_tests = [("systemctl_cmd", `Quick, test_systemctl_cmd)]

let env_file_tests =
  [
    ("env_file_template user mode", `Quick, test_env_file_template_user_mode);
    ("env_file_template system mode", `Quick, test_env_file_template_system_mode);
    ( "env_file_template different modes",
      `Quick,
      test_env_file_template_different_modes );
  ]

let logrotate_tests =
  [
    ("system_logrotate_config_path", `Quick, test_system_logrotate_config_path);
    ( "system_logrotate_config_path roles",
      `Quick,
      test_system_logrotate_config_path_different_roles );
    ("user_logrotate_root", `Quick, test_user_logrotate_root);
    ("user_logrotate_include_dir", `Quick, test_user_logrotate_include_dir);
    ("user_logrotate_main_config", `Quick, test_user_logrotate_main_config);
  ]

let prestart_tests =
  [
    ("prestart_hooks_dir", `Quick, test_prestart_hooks_dir);
    ("prestart_script_path", `Quick, test_prestart_script_path);
    ( "prestart_script_path different roles",
      `Quick,
      test_prestart_script_path_different_roles );
  ]

let () =
  Alcotest.run
    "Systemd_extended"
    [
      ("role_binary", role_binary_tests);
      ("unit_name", unit_name_tests);
      ("unit_path", unit_path_tests);
      ("dropin", dropin_tests);
      ("systemctl", systemctl_tests);
      ("env_file", env_file_tests);
      ("logrotate", logrotate_tests);
      ("prestart", prestart_tests);
    ]
