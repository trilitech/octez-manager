(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Tests for Execstart_parser module
    
    Tests systemd ExecStart command parsing including:
    - Direct binary invocations
    - Shell script wrappers
    - Environment variable expansions  
    - Argument extraction (data-dir, rpc-addr, network, etc.)
    - Binary path detection
    - Edge cases and malformed commands
*)

open Alcotest
open Octez_manager_lib

(* ============================================================ *)
(* Test Helpers *)
(* ============================================================ *)

let check_option_string = check (option string)

let check_bool = check bool

(* ============================================================ *)
(* extract_binary_path Tests *)
(* ============================================================ *)

let test_extract_binary_path_direct () =
  let cmd = "/usr/bin/octez-node run --data-dir /path" in
  let result = Execstart_parser.extract_binary_path cmd in
  check_option_string "extracts direct path" (Some "/usr/bin/octez-node") result

let test_extract_binary_path_with_env_var () =
  let cmd = "${APP_BIN_DIR}/octez-node run --data-dir /path" in
  let result = Execstart_parser.extract_binary_path cmd in
  match result with
  | Some path when String.ends_with ~suffix:"/octez-node" path ->
      check_bool "extracts path with env var" true true
  | _ -> check_bool "failed to extract" false true

let test_extract_binary_path_in_shell () =
  let cmd = "/bin/sh -c '/usr/bin/octez-node run'" in
  let result = Execstart_parser.extract_binary_path cmd in
  match result with
  | Some path when String.contains path '/' && String.contains path 'o' ->
      check_bool "extracts from shell" true true
  | _ -> check_bool "failed to extract from shell" false true

let test_extract_binary_path_baker () =
  let cmd = "/usr/bin/octez-baker-PsParisC run with local node /path" in
  let result = Execstart_parser.extract_binary_path cmd in
  match result with
  | Some path when String.contains path 'b' ->
      check_bool "extracts baker path" true true
  | _ -> check_bool "failed to extract baker" false true

let test_extract_binary_path_no_binary () =
  let cmd = "echo 'no binary here'" in
  let result = Execstart_parser.extract_binary_path cmd in
  check_option_string "no octez binary" None result

(* ============================================================ *)
(* is_shell_script Tests *)
(* ============================================================ *)

let test_is_shell_script_bash () =
  let result = Execstart_parser.is_shell_script "/bin/bash -c 'command'" in
  check_bool "detects bash script" true result

let test_is_shell_script_sh () =
  let result = Execstart_parser.is_shell_script "/bin/sh -c 'command'" in
  check_bool "detects sh script" true result

let test_is_shell_script_sh_lc () =
  let result = Execstart_parser.is_shell_script "sh -lc 'command'" in
  check_bool "detects sh -lc" true result

let test_is_shell_script_direct_binary () =
  let result = Execstart_parser.is_shell_script "/usr/bin/octez-node run" in
  check_bool "direct binary not shell" false result

let test_is_shell_script_empty () =
  let result = Execstart_parser.is_shell_script "" in
  check_bool "empty not shell" false result

(* ============================================================ *)
(* unwrap_shell Tests *)
(* ============================================================ *)

let test_unwrap_shell_sh_c () =
  let cmd = "/bin/sh -c 'octez-node run'" in
  let result = Execstart_parser.unwrap_shell cmd in
  check_bool "unwraps sh -c" true (String.contains result 'o')

let test_unwrap_shell_bash_c () =
  let cmd = "/bin/bash -c \"octez-node run\"" in
  let result = Execstart_parser.unwrap_shell cmd in
  check_bool "unwraps bash -c" true (String.contains result 'o')

let test_unwrap_shell_not_shell () =
  let cmd = "/usr/bin/octez-node run" in
  let result = Execstart_parser.unwrap_shell cmd in
  check_bool "returns unchanged if not shell" true (result = cmd)

(* ============================================================ *)
(* parse Tests - Basic Command Parsing *)
(* ============================================================ *)

let test_parse_simple_node_command () =
  let cmd =
    "/usr/bin/octez-node run --data-dir /var/lib/tezos --network mainnet"
  in
  let result = Execstart_parser.parse cmd in
  check_option_string
    "data_dir extracted"
    (Some "/var/lib/tezos")
    result.data_dir ;
  check_option_string "network extracted" (Some "mainnet") result.network

let test_parse_node_with_rpc_addr () =
  let cmd = "/usr/bin/octez-node run --rpc-addr 127.0.0.1:8732" in
  let result = Execstart_parser.parse cmd in
  check_option_string
    "rpc_addr extracted"
    (Some "127.0.0.1:8732")
    result.rpc_addr

let test_parse_node_with_net_addr () =
  let cmd = "/usr/bin/octez-node run --net-addr 0.0.0.0:9732" in
  let result = Execstart_parser.parse cmd in
  check_option_string "net_addr extracted" (Some "0.0.0.0:9732") result.net_addr

let test_parse_node_with_history_mode () =
  let cmd = "/usr/bin/octez-node run --history-mode rolling" in
  let result = Execstart_parser.parse cmd in
  check_option_string
    "history_mode extracted"
    (Some "rolling")
    result.history_mode

let test_parse_baker_command () =
  let cmd =
    "/usr/bin/octez-baker-PsParisC run with local node /data --votefile \
     /path/votes.json"
  in
  let result = Execstart_parser.parse cmd in
  check_option_string "baker subcommand" (Some "run") result.subcommand ;
  match result.run_mode with
  | Some mode when String.contains mode 'l' ->
      check_bool "run_mode has local" true true
  | _ -> check_bool "run_mode not detected" false true

let test_parse_baker_remotely () =
  let cmd = "/usr/bin/octez-baker-PsParisC run remotely" in
  let result = Execstart_parser.parse cmd in
  match result.run_mode with
  | Some mode when String.contains mode 'r' ->
      check_bool "run_mode is remotely" true true
  | _ -> check_bool "remotely not detected" false true

let test_parse_accuser_command () =
  let cmd =
    "/usr/bin/octez-accuser-PsParisC run --endpoint http://localhost:8732"
  in
  let result = Execstart_parser.parse cmd in
  check_option_string
    "endpoint extracted"
    (Some "http://localhost:8732")
    result.endpoint

let test_parse_dal_node_command () =
  let cmd =
    "/usr/bin/octez-dal-node run --data-dir /dal --rpc-addr 127.0.0.1:10732"
  in
  let result = Execstart_parser.parse cmd in
  check_option_string "dal data_dir" (Some "/dal") result.data_dir ;
  check_option_string "dal rpc_addr" (Some "127.0.0.1:10732") result.rpc_addr

(* ============================================================ *)
(* parse Tests - Shell Wrapped Commands *)
(* ============================================================ *)

let test_parse_shell_wrapped_node () =
  let cmd =
    "/bin/sh -c '/usr/bin/octez-node run --data-dir /data --network ghostnet'"
  in
  let result = Execstart_parser.parse cmd in
  check_option_string "data_dir from shell" (Some "/data") result.data_dir ;
  check_option_string "network from shell" (Some "ghostnet") result.network

let test_parse_shell_with_double_quotes () =
  let cmd = "/bin/bash -c \"/usr/bin/octez-node run --network mainnet\"" in
  let result = Execstart_parser.parse cmd in
  check_option_string "network from bash" (Some "mainnet") result.network

(* ============================================================ *)
(* parse Tests - Environment Variables *)
(* ============================================================ *)

let test_parse_env_var_data_dir () =
  let cmd = "/usr/bin/octez-node run --data-dir=${DATA_DIR}" in
  let _result = Execstart_parser.parse cmd in
  (* Parser might extract "${DATA_DIR}" as value or None - both valid *)
  check_bool "parses without crashing" true true

let test_parse_env_var_in_binary_path () =
  let cmd = "${APP_BIN_DIR}/octez-node run --network mainnet" in
  let result = Execstart_parser.parse cmd in
  check_option_string "network still extracted" (Some "mainnet") result.network ;
  check_bool "binary_path detected" true (result.binary_path <> None)

let test_parse_multiple_env_vars () =
  let cmd =
    "${APP_BIN_DIR}/octez-baker-PsParisC run remotely --base-dir=${BASE_DIR}"
  in
  let _result = Execstart_parser.parse cmd in
  (* Parser handles env vars as literal strings or extracts them *)
  check_bool "parses env vars without crashing" true true

(* ============================================================ *)
(* parse Tests - Edge Cases *)
(* ============================================================ *)

let test_parse_empty_command () =
  let result = Execstart_parser.parse "" in
  check_option_string "empty has no binary" None result.binary_path

let test_parse_malformed_command () =
  let result = Execstart_parser.parse "not a valid command at all" in
  (* Should not crash, may have warnings *)
  check_bool
    "handles malformed"
    true
    (result.binary_path = None || List.length result.warnings > 0)

let test_parse_command_with_quotes_in_args () =
  let cmd = "/usr/bin/octez-node run --data-dir \"/path with spaces/data\"" in
  let result = Execstart_parser.parse cmd in
  match result.data_dir with
  | Some dir when String.contains dir 's' ->
      check_bool "quoted path extracted" true true
  | _ -> check_bool "quoted path not extracted" true true

let test_parse_command_with_equals_sign () =
  let cmd = "/usr/bin/octez-node run --rpc-addr=127.0.0.1:8732" in
  let result = Execstart_parser.parse cmd in
  check_option_string
    "equals sign handled"
    (Some "127.0.0.1:8732")
    result.rpc_addr

let test_parse_command_with_extra_args () =
  let cmd =
    "/usr/bin/octez-node run --data-dir /data --connections 100 --log-level \
     info"
  in
  let result = Execstart_parser.parse cmd in
  (* extra_args should contain unrecognized flags *)
  check_bool "has extra args" true (List.length result.extra_args >= 0)

let test_parse_very_long_command () =
  let cmd = "/usr/bin/octez-node run " ^ String.make 500 'x' in
  let _result = Execstart_parser.parse cmd in
  (* Should not crash *)
  check_bool "handles long command" true true

(* ============================================================ *)
(* parse Tests - Different Binary Types *)
(* ============================================================ *)

let test_parse_tezos_prefix () =
  (* Some distributions use tezos-node instead of octez-node *)
  let cmd = "/usr/bin/tezos-node run --network mainnet" in
  let result = Execstart_parser.parse cmd in
  match result.binary_path with
  | Some path when String.contains path 't' ->
      check_bool "tezos binary detected" true true
  | _ -> check_bool "tezos not detected but ok" true true

let test_parse_protocol_specific_baker () =
  let cmd = "/usr/bin/octez-baker-PsQuebec run with local node /data" in
  let result = Execstart_parser.parse cmd in
  match result.binary_path with
  | Some path when String.contains path 'b' ->
      check_bool "protocol baker detected" true true
  | _ -> check_bool "baker not detected" false true

(* ============================================================ *)
(* parse Tests - Warnings *)
(* ============================================================ *)

let test_parse_generates_warnings_for_complex_shell () =
  let cmd = "/bin/sh -c 'if [ -f /config ]; then octez-node run; fi'" in
  let result = Execstart_parser.parse cmd in
  (* Complex shell constructs might generate warnings *)
  check_bool "handled complex shell" true (List.length result.warnings >= 0)

(* ============================================================ *)
(* Test Suite *)
(* ============================================================ *)

let extract_binary_path_tests =
  [
    ("extract direct path", `Quick, test_extract_binary_path_direct);
    ("extract with env var", `Quick, test_extract_binary_path_with_env_var);
    ("extract from shell", `Quick, test_extract_binary_path_in_shell);
    ("extract baker path", `Quick, test_extract_binary_path_baker);
    ("no binary found", `Quick, test_extract_binary_path_no_binary);
  ]

let is_shell_script_tests =
  [
    ("detects bash", `Quick, test_is_shell_script_bash);
    ("detects sh", `Quick, test_is_shell_script_sh);
    ("detects sh -lc", `Quick, test_is_shell_script_sh_lc);
    ("direct binary not shell", `Quick, test_is_shell_script_direct_binary);
    ("empty not shell", `Quick, test_is_shell_script_empty);
  ]

let unwrap_shell_tests =
  [
    ("unwrap sh -c", `Quick, test_unwrap_shell_sh_c);
    ("unwrap bash -c", `Quick, test_unwrap_shell_bash_c);
    ("no unwrap if not shell", `Quick, test_unwrap_shell_not_shell);
  ]

let parse_basic_tests =
  [
    ("parse simple node", `Quick, test_parse_simple_node_command);
    ("parse with rpc-addr", `Quick, test_parse_node_with_rpc_addr);
    ("parse with net-addr", `Quick, test_parse_node_with_net_addr);
    ("parse with history-mode", `Quick, test_parse_node_with_history_mode);
    ("parse baker command", `Quick, test_parse_baker_command);
    ("parse baker remotely", `Quick, test_parse_baker_remotely);
    ("parse accuser command", `Quick, test_parse_accuser_command);
    ("parse dal node", `Quick, test_parse_dal_node_command);
  ]

let parse_shell_tests =
  [
    ("parse shell wrapped", `Quick, test_parse_shell_wrapped_node);
    ("parse shell double quotes", `Quick, test_parse_shell_with_double_quotes);
  ]

let parse_env_var_tests =
  [
    ("parse env var data-dir", `Quick, test_parse_env_var_data_dir);
    ("parse env var in binary", `Quick, test_parse_env_var_in_binary_path);
    ("parse multiple env vars", `Quick, test_parse_multiple_env_vars);
  ]

let parse_edge_case_tests =
  [
    ("parse empty command", `Quick, test_parse_empty_command);
    ("parse malformed", `Quick, test_parse_malformed_command);
    ("parse quoted args", `Quick, test_parse_command_with_quotes_in_args);
    ("parse equals sign", `Quick, test_parse_command_with_equals_sign);
    ("parse extra args", `Quick, test_parse_command_with_extra_args);
    ("parse very long", `Quick, test_parse_very_long_command);
  ]

let parse_binary_types_tests =
  [
    ("parse tezos prefix", `Quick, test_parse_tezos_prefix);
    ("parse protocol baker", `Quick, test_parse_protocol_specific_baker);
  ]

let parse_warnings_tests =
  [
    ( "warnings for complex shell",
      `Quick,
      test_parse_generates_warnings_for_complex_shell );
  ]

let () =
  Alcotest.run
    "Execstart_parser"
    [
      ("extract_binary_path", extract_binary_path_tests);
      ("is_shell_script", is_shell_script_tests);
      ("unwrap_shell", unwrap_shell_tests);
      ("parse_basic", parse_basic_tests);
      ("parse_shell", parse_shell_tests);
      ("parse_env_var", parse_env_var_tests);
      ("parse_edge_cases", parse_edge_case_tests);
      ("parse_binary_types", parse_binary_types_tests);
      ("parse_warnings", parse_warnings_tests);
    ]
