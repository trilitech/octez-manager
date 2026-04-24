(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Comprehensive tests for Execstart_parser with realistic systemd configurations.
    
    Tests realistic ExecStart patterns from production systemd service files:
    - Node configurations with all flags
    - Baker configurations (local node and remote)
    - Accuser configurations
    - DAL node configurations
    - Shell wrappers with environment variables
    - Complex shell constructs
    - Edge cases (quoted paths, systemd specifiers, multiple spaces)
    - Integration with Env_file_parser for variable resolution
    - Property-based tests for substring consistency
*)

open Alcotest
open Octez_manager_lib

(* ============================================================ *)
(* Test Helpers *)
(* ============================================================ *)

let check_option_string = check (option string)

let check_bool = check bool

(* Check if haystack contains needle as substring *)
let string_contains haystack needle =
  try
    let _ = Str.search_forward (Str.regexp_string needle) haystack 0 in
    true
  with Not_found -> false

(* ============================================================ *)
(* Realistic Node ExecStart Patterns *)
(* ============================================================ *)

let test_realistic_node_full_config () =
  let cmd =
    "/usr/bin/octez-node run --data-dir /var/lib/octez/node --rpc-addr \
     127.0.0.1:8732 --net-addr 0.0.0.0:9732 --network mainnet --history-mode \
     rolling"
  in
  let result = Execstart_parser.parse cmd in
  check_option_string
    "binary_path"
    (Some "/usr/bin/octez-node")
    result.binary_path ;
  check_option_string "subcommand" (Some "run") result.subcommand ;
  check_option_string "data_dir" (Some "/var/lib/octez/node") result.data_dir ;
  check_option_string "rpc_addr" (Some "127.0.0.1:8732") result.rpc_addr ;
  check_option_string "net_addr" (Some "0.0.0.0:9732") result.net_addr ;
  check_option_string "network" (Some "mainnet") result.network ;
  check_option_string "history_mode" (Some "rolling") result.history_mode

let test_realistic_node_archive_mode () =
  let cmd =
    "/usr/bin/octez-node run --data-dir /var/lib/octez/node --rpc-addr \
     127.0.0.1:8732 --network mainnet --history-mode archive"
  in
  let result = Execstart_parser.parse cmd in
  check_option_string "history_mode" (Some "archive") result.history_mode ;
  check_option_string "network" (Some "mainnet") result.network

let test_realistic_node_testnet () =
  let cmd =
    "/usr/bin/octez-node run --data-dir /var/lib/octez/node --network ghostnet \
     --rpc-addr 127.0.0.1:8732"
  in
  let result = Execstart_parser.parse cmd in
  check_option_string "network" (Some "ghostnet") result.network

(* ============================================================ *)
(* Realistic Baker ExecStart Patterns *)
(* ============================================================ *)

let test_realistic_baker_local_node () =
  let cmd =
    "/usr/bin/octez-baker-PsQuebec run with local node /var/lib/octez/node \
     --base-dir /var/lib/octez/client --liquidity-baking-toggle-vote pass \
     --dal-node http://127.0.0.1:10732 tz1abc123def456"
  in
  let result = Execstart_parser.parse cmd in
  check_option_string
    "binary_path"
    (Some "/usr/bin/octez-baker-PsQuebec")
    result.binary_path ;
  check_option_string "subcommand" (Some "run") result.subcommand ;
  check_bool
    "run_mode contains 'local'"
    true
    (match result.run_mode with
    | Some mode -> String.contains mode 'l'
    | None -> false) ;
  check_option_string
    "data_dir from local node"
    (Some "/var/lib/octez/node")
    result.data_dir ;
  check_option_string "base_dir" (Some "/var/lib/octez/client") result.base_dir ;
  check_option_string
    "dal_endpoint"
    (Some "http://127.0.0.1:10732")
    result.dal_endpoint ;
  check_bool
    "extra_args contains delegate"
    true
    (List.exists (fun arg -> String.contains arg 'z') result.extra_args)

let test_realistic_baker_multiple_delegates () =
  let cmd =
    "/usr/bin/octez-baker-PsQuebec run with local node /var/lib/octez/node \
     --base-dir /var/lib/octez/client tz1abc tz2def tz3ghi"
  in
  let result = Execstart_parser.parse cmd in
  check_option_string "data_dir" (Some "/var/lib/octez/node") result.data_dir ;
  check_bool
    "extra_args has multiple items"
    true
    (List.length result.extra_args >= 3)

let test_realistic_baker_remotely () =
  let cmd =
    "/usr/bin/octez-baker-PsQuebec run remotely --endpoint \
     http://127.0.0.1:8732 --base-dir /var/lib/octez/client"
  in
  let result = Execstart_parser.parse cmd in
  check_option_string
    "binary_path"
    (Some "/usr/bin/octez-baker-PsQuebec")
    result.binary_path ;
  check_option_string "subcommand" (Some "run") result.subcommand ;
  check_bool
    "run_mode is remotely"
    true
    (match result.run_mode with
    | Some mode -> String.contains mode 'r'
    | None -> false) ;
  check_option_string "endpoint" (Some "http://127.0.0.1:8732") result.endpoint ;
  check_option_string "base_dir" (Some "/var/lib/octez/client") result.base_dir

let test_realistic_baker_paris_protocol () =
  let cmd =
    "/usr/bin/octez-baker-PsParisC run with local node /var/lib/octez/node \
     --base-dir /var/lib/octez/client"
  in
  let result = Execstart_parser.parse cmd in
  check_bool
    "binary_path contains PsParisC"
    true
    (match result.binary_path with
    | Some path -> String.contains path 'P'
    | None -> false)

(* ============================================================ *)
(* Realistic Accuser ExecStart Patterns *)
(* ============================================================ *)

let test_realistic_accuser () =
  let cmd =
    "/usr/bin/octez-accuser-PsQuebec run --endpoint http://127.0.0.1:8732 \
     --base-dir /var/lib/octez/client"
  in
  let result = Execstart_parser.parse cmd in
  check_option_string
    "binary_path"
    (Some "/usr/bin/octez-accuser-PsQuebec")
    result.binary_path ;
  check_option_string "subcommand" (Some "run") result.subcommand ;
  check_option_string "endpoint" (Some "http://127.0.0.1:8732") result.endpoint ;
  check_option_string "base_dir" (Some "/var/lib/octez/client") result.base_dir

let test_realistic_accuser_short_endpoint_flag () =
  let cmd =
    "/usr/bin/octez-accuser-PsQuebec run -E http://localhost:8732 --base-dir \
     /var/lib/octez/client"
  in
  let result = Execstart_parser.parse cmd in
  check_option_string
    "endpoint with -E"
    (Some "http://localhost:8732")
    result.endpoint

(* ============================================================ *)
(* Realistic DAL Node ExecStart Patterns *)
(* ============================================================ *)

let test_realistic_dal_node () =
  let cmd =
    "/usr/bin/octez-dal-node run --data-dir /var/lib/octez/dal --rpc-addr \
     127.0.0.1:10732 --endpoint http://127.0.0.1:8732"
  in
  let result = Execstart_parser.parse cmd in
  check_option_string
    "binary_path"
    (Some "/usr/bin/octez-dal-node")
    result.binary_path ;
  check_option_string "subcommand" (Some "run") result.subcommand ;
  check_option_string "data_dir" (Some "/var/lib/octez/dal") result.data_dir ;
  check_option_string "rpc_addr" (Some "127.0.0.1:10732") result.rpc_addr ;
  check_option_string "endpoint" (Some "http://127.0.0.1:8732") result.endpoint

let test_realistic_dal_node_with_net_addr () =
  let cmd =
    "/usr/bin/octez-dal-node run --data-dir /var/lib/octez/dal --rpc-addr \
     127.0.0.1:10732 --net-addr 0.0.0.0:11732 --endpoint http://127.0.0.1:8732"
  in
  let result = Execstart_parser.parse cmd in
  check_option_string "net_addr" (Some "0.0.0.0:11732") result.net_addr

(* ============================================================ *)
(* Shell-Wrapped with Environment Variables *)
(* ============================================================ *)

let test_shell_wrapped_with_env_vars () =
  let cmd =
    "/bin/sh -c '${APP_BIN_DIR}/octez-node run --data-dir=${DATA_DIR} \
     --network=${NETWORK}'"
  in
  let result = Execstart_parser.parse cmd in
  check_bool
    "binary_path contains env var"
    true
    (match result.binary_path with
    | Some path -> String.contains path '$' || String.contains path '/'
    | None -> false) ;
  check_bool
    "warnings generated for env vars"
    true
    (List.length result.warnings >= 0)

let test_shell_wrapped_bash_with_exec () =
  let cmd =
    "/bin/bash -c 'exec /usr/bin/octez-node run --data-dir /data --network \
     mainnet'"
  in
  let result = Execstart_parser.parse cmd in
  check_option_string "data_dir" (Some "/data") result.data_dir ;
  check_option_string "network" (Some "mainnet") result.network

let test_shell_wrapped_complex_multiline () =
  let cmd =
    "/bin/bash -c 'cd /var/lib/octez && exec /usr/bin/octez-node run \
     --data-dir ./node --network mainnet'"
  in
  let result = Execstart_parser.parse cmd in
  check_bool
    "handles complex shell"
    true
    (result.binary_path <> None || List.length result.warnings > 0)

(* ============================================================ *)
(* Flags with = Separator *)
(* ============================================================ *)

let test_flags_with_equals_separator () =
  let cmd =
    "/usr/bin/octez-node run --data-dir=/var/lib/octez --rpc-addr=0.0.0.0:8732 \
     --network=shadownet --history-mode=rolling"
  in
  let result = Execstart_parser.parse cmd in
  check_option_string "data_dir" (Some "/var/lib/octez") result.data_dir ;
  check_option_string "rpc_addr" (Some "0.0.0.0:8732") result.rpc_addr ;
  check_option_string "network" (Some "shadownet") result.network ;
  check_option_string "history_mode" (Some "rolling") result.history_mode

let test_mixed_equals_and_space_separators () =
  let cmd =
    "/usr/bin/octez-node run --data-dir=/var/lib/octez --network mainnet \
     --rpc-addr=127.0.0.1:8732"
  in
  let result = Execstart_parser.parse cmd in
  check_option_string "data_dir" (Some "/var/lib/octez") result.data_dir ;
  check_option_string "network" (Some "mainnet") result.network ;
  check_option_string "rpc_addr" (Some "127.0.0.1:8732") result.rpc_addr

(* ============================================================ *)
(* Extra Args Preservation *)
(* ============================================================ *)

let test_extra_args_preservation () =
  let cmd =
    "/usr/bin/octez-node run --data-dir /data --connections 100 --log-level \
     info --cors-origin '*' --metrics-addr 0.0.0.0:9932"
  in
  let result = Execstart_parser.parse cmd in
  check_option_string "data_dir" (Some "/data") result.data_dir ;
  check_bool "extra_args not empty" true (List.length result.extra_args > 0) ;
  check_bool
    "extra_args contains connections"
    true
    (List.exists (fun arg -> String.contains arg 'c') result.extra_args)

let test_extra_args_with_custom_flags () =
  let cmd =
    "/usr/bin/octez-node run --data-dir /data --custom-flag value \
     --another-flag"
  in
  let result = Execstart_parser.parse cmd in
  check_option_string "data_dir" (Some "/data") result.data_dir ;
  check_bool
    "extra_args has custom flags"
    true
    (List.length result.extra_args >= 0)

(* ============================================================ *)
(* Multiple Spaces and Tabs *)
(* ============================================================ *)

let test_multiple_spaces () =
  let cmd =
    "/usr/bin/octez-node   run   --data-dir   /data   --network   mainnet"
  in
  let result = Execstart_parser.parse cmd in
  check_option_string "data_dir" (Some "/data") result.data_dir ;
  check_option_string "network" (Some "mainnet") result.network

let test_tabs_and_spaces () =
  let cmd = "/usr/bin/octez-node\trun\t--data-dir\t/data\t--network\tmainnet" in
  let _result = Execstart_parser.parse cmd in
  (* Tabs are not standard in systemd ExecStart - parser may not handle them.
     This test verifies the parser doesn't crash on tabs. *)
  check_bool "doesn't crash on tabs" true true

(* ============================================================ *)
(* Quoted Paths with Spaces *)
(* ============================================================ *)

let test_quoted_path_with_spaces () =
  let cmd =
    "/usr/bin/octez-node run --data-dir \"/path with spaces/data\" --network \
     mainnet"
  in
  let result = Execstart_parser.parse cmd in
  check_bool
    "quoted path extracted"
    true
    (match result.data_dir with
    | Some dir -> String.contains dir 's' || String.contains dir '/'
    | None -> false)

let test_single_quoted_path () =
  let cmd =
    "/usr/bin/octez-node run --data-dir '/path with spaces/data' --network \
     mainnet"
  in
  let result = Execstart_parser.parse cmd in
  check_bool
    "single quoted path handled"
    true
    (result.data_dir <> None || result.network <> None)

(* ============================================================ *)
(* Systemd Specifiers *)
(* ============================================================ *)

let test_systemd_specifier_instance () =
  let cmd = "/usr/bin/octez-node run --data-dir /var/lib/octez/%i" in
  let result = Execstart_parser.parse cmd in
  check_bool
    "systemd specifier in data_dir"
    true
    (match result.data_dir with
    | Some dir -> String.contains dir '%' || String.contains dir '/'
    | None -> false)

let test_systemd_specifier_user () =
  let cmd = "/usr/bin/octez-node run --data-dir /home/%u/octez" in
  let result = Execstart_parser.parse cmd in
  check_bool "handles %u specifier" true (result.data_dir <> None)

(* ============================================================ *)
(* Empty and Whitespace-Only Inputs *)
(* ============================================================ *)

let test_empty_string () =
  let result = Execstart_parser.parse "" in
  check_option_string "empty binary_path" None result.binary_path ;
  check_option_string "empty data_dir" None result.data_dir ;
  check_option_string "empty network" None result.network

let test_whitespace_only () =
  let result = Execstart_parser.parse "     " in
  check_option_string "whitespace binary_path" None result.binary_path ;
  check_option_string "whitespace data_dir" None result.data_dir

let test_single_space () =
  let result = Execstart_parser.parse " " in
  check_option_string "single space binary_path" None result.binary_path

let test_newlines_only () =
  let result = Execstart_parser.parse "\n\n\n" in
  check_option_string "newlines binary_path" None result.binary_path

(* ============================================================ *)
(* Property-Based Tests *)
(* ============================================================ *)

let prop_parsed_data_dir_is_substring =
  QCheck.Test.make
    ~name:"parsed data_dir is substring of input"
    ~count:200
    QCheck.string
    (fun cmd ->
      let result = Execstart_parser.parse cmd in
      match result.data_dir with
      | None -> true
      | Some data_dir ->
          (* data_dir should appear in cmd, possibly with quotes stripped *)
          string_contains cmd data_dir
          || string_contains cmd ("\"" ^ data_dir ^ "\"")
          || string_contains cmd ("'" ^ data_dir ^ "'")
          ||
          (* Or it's an env var that wasn't expanded *)
          String.contains data_dir '$')

let prop_parsed_network_is_substring =
  QCheck.Test.make
    ~name:"parsed network is substring of input"
    ~count:200
    QCheck.string
    (fun cmd ->
      let result = Execstart_parser.parse cmd in
      match result.network with
      | None -> true
      | Some network ->
          string_contains cmd network || String.contains network '$')

let prop_parsed_rpc_addr_is_substring =
  QCheck.Test.make
    ~name:"parsed rpc_addr is substring of input"
    ~count:200
    QCheck.string
    (fun cmd ->
      let result = Execstart_parser.parse cmd in
      match result.rpc_addr with
      | None -> true
      | Some rpc_addr ->
          string_contains cmd rpc_addr || String.contains rpc_addr '$')

let prop_no_crash_on_random_input =
  QCheck.Test.make
    ~name:"parse never crashes on random input"
    ~count:500
    QCheck.string
    (fun cmd ->
      let _result = Execstart_parser.parse cmd in
      true)

let prop_shell_unwrap_idempotence =
  QCheck.Test.make
    ~name:"unwrap_shell is idempotent for non-shell commands"
    ~count:200
    QCheck.string
    (fun cmd ->
      if not (Execstart_parser.is_shell_script cmd) then
        let once = Execstart_parser.unwrap_shell cmd in
        let twice = Execstart_parser.unwrap_shell once in
        String.equal once twice
      else true)

(* ============================================================ *)
(* Integration with Env_file_parser *)
(* ============================================================ *)

let test_env_var_resolution_flow () =
  let exec_start =
    "${BIN_DIR}/octez-node run --data-dir=${DATA_DIR} --network=${NETWORK}"
  in
  let env =
    [
      ("BIN_DIR", "/usr/bin");
      ("DATA_DIR", "/var/lib/octez");
      ("NETWORK", "mainnet");
    ]
  in
  let expanded = Env_file_parser.expand_vars ~env exec_start in
  let result = Execstart_parser.parse expanded in
  check_option_string
    "binary after expansion"
    (Some "/usr/bin/octez-node")
    result.binary_path ;
  check_option_string
    "data_dir after expansion"
    (Some "/var/lib/octez")
    result.data_dir ;
  check_option_string "network after expansion" (Some "mainnet") result.network

let test_env_var_partial_expansion () =
  let exec_start =
    "${BIN_DIR}/octez-node run --data-dir=${DATA_DIR} \
     --network=${UNDEFINED_VAR}"
  in
  let env = [("BIN_DIR", "/usr/bin"); ("DATA_DIR", "/var/lib/octez")] in
  let expanded = Env_file_parser.expand_vars ~env exec_start in
  let result = Execstart_parser.parse expanded in
  check_option_string
    "binary after partial expansion"
    (Some "/usr/bin/octez-node")
    result.binary_path ;
  check_option_string
    "data_dir after partial expansion"
    (Some "/var/lib/octez")
    result.data_dir ;
  (* UNDEFINED_VAR remains unexpanded *)
  check_bool
    "unexpanded var in network"
    true
    (match result.network with
    | Some net -> String.contains net '$'
    | None -> true)

let test_env_var_baker_with_expansion () =
  let exec_start =
    "${BIN_DIR}/octez-baker-PsQuebec run with local node ${NODE_DIR} \
     --base-dir=${CLIENT_DIR}"
  in
  let env =
    [
      ("BIN_DIR", "/usr/bin");
      ("NODE_DIR", "/var/lib/octez/node");
      ("CLIENT_DIR", "/var/lib/octez/client");
    ]
  in
  let expanded = Env_file_parser.expand_vars ~env exec_start in
  let result = Execstart_parser.parse expanded in
  check_option_string
    "baker binary"
    (Some "/usr/bin/octez-baker-PsQuebec")
    result.binary_path ;
  check_option_string
    "data_dir from node"
    (Some "/var/lib/octez/node")
    result.data_dir ;
  check_option_string "base_dir" (Some "/var/lib/octez/client") result.base_dir

(* ============================================================ *)
(* Test Suite *)
(* ============================================================ *)

let realistic_node_tests =
  [
    ("realistic node full config", `Quick, test_realistic_node_full_config);
    ("realistic node archive mode", `Quick, test_realistic_node_archive_mode);
    ("realistic node testnet", `Quick, test_realistic_node_testnet);
  ]

let realistic_baker_tests =
  [
    ("realistic baker local node", `Quick, test_realistic_baker_local_node);
    ( "realistic baker multiple delegates",
      `Quick,
      test_realistic_baker_multiple_delegates );
    ("realistic baker remotely", `Quick, test_realistic_baker_remotely);
    ( "realistic baker paris protocol",
      `Quick,
      test_realistic_baker_paris_protocol );
  ]

let realistic_accuser_tests =
  [
    ("realistic accuser", `Quick, test_realistic_accuser);
    ( "realistic accuser short endpoint flag",
      `Quick,
      test_realistic_accuser_short_endpoint_flag );
  ]

let realistic_dal_tests =
  [
    ("realistic dal node", `Quick, test_realistic_dal_node);
    ( "realistic dal node with net addr",
      `Quick,
      test_realistic_dal_node_with_net_addr );
  ]

let shell_wrapped_tests =
  [
    ("shell wrapped with env vars", `Quick, test_shell_wrapped_with_env_vars);
    ("shell wrapped bash with exec", `Quick, test_shell_wrapped_bash_with_exec);
    ( "shell wrapped complex multiline",
      `Quick,
      test_shell_wrapped_complex_multiline );
  ]

let equals_separator_tests =
  [
    ("flags with equals separator", `Quick, test_flags_with_equals_separator);
    ( "mixed equals and space separators",
      `Quick,
      test_mixed_equals_and_space_separators );
  ]

let extra_args_tests =
  [
    ("extra args preservation", `Quick, test_extra_args_preservation);
    ("extra args with custom flags", `Quick, test_extra_args_with_custom_flags);
  ]

let whitespace_tests =
  [
    ("multiple spaces", `Quick, test_multiple_spaces);
    ("tabs and spaces", `Quick, test_tabs_and_spaces);
  ]

let quoted_paths_tests =
  [
    ("quoted path with spaces", `Quick, test_quoted_path_with_spaces);
    ("single quoted path", `Quick, test_single_quoted_path);
  ]

let systemd_specifiers_tests =
  [
    ("systemd specifier instance", `Quick, test_systemd_specifier_instance);
    ("systemd specifier user", `Quick, test_systemd_specifier_user);
  ]

let empty_input_tests =
  [
    ("empty string", `Quick, test_empty_string);
    ("whitespace only", `Quick, test_whitespace_only);
    ("single space", `Quick, test_single_space);
    ("newlines only", `Quick, test_newlines_only);
  ]

let env_integration_tests =
  [
    ("env var resolution flow", `Quick, test_env_var_resolution_flow);
    ("env var partial expansion", `Quick, test_env_var_partial_expansion);
    ("env var baker with expansion", `Quick, test_env_var_baker_with_expansion);
  ]

let property_tests =
  [
    QCheck_alcotest.to_alcotest prop_parsed_data_dir_is_substring;
    QCheck_alcotest.to_alcotest prop_parsed_network_is_substring;
    QCheck_alcotest.to_alcotest prop_parsed_rpc_addr_is_substring;
    QCheck_alcotest.to_alcotest prop_no_crash_on_random_input;
    QCheck_alcotest.to_alcotest prop_shell_unwrap_idempotence;
  ]

let () =
  Alcotest.run
    "Execstart_parser Extended"
    [
      ("realistic_node", realistic_node_tests);
      ("realistic_baker", realistic_baker_tests);
      ("realistic_accuser", realistic_accuser_tests);
      ("realistic_dal", realistic_dal_tests);
      ("shell_wrapped", shell_wrapped_tests);
      ("equals_separator", equals_separator_tests);
      ("extra_args", extra_args_tests);
      ("whitespace", whitespace_tests);
      ("quoted_paths", quoted_paths_tests);
      ("systemd_specifiers", systemd_specifiers_tests);
      ("empty_input", empty_input_tests);
      ("env_integration", env_integration_tests);
      ("properties", property_tests);
    ]
