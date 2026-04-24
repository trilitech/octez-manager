(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Pure function tests for systemd service import configuration preservation.
    
    Tests cover:
    - Env_file_parser: parse_string and expand_vars
    - External_service: suggest_instance_name, endpoint_matches_rpc,
      role_of_binary_name, status_of_unit_state, unknown_field_count,
      unknown_field_names, get_dependencies, get_dependents
    - Import.For_tests: missing_required_fields *)

open Alcotest
open Octez_manager_lib

(* ============================================================ *)
(* Env_file_parser.parse_string Tests *)
(* ============================================================ *)

let test_parse_simple_key_value () =
  let result = Env_file_parser.parse_string "KEY=value" in
  check (list (pair string string)) "simple key=value" [("KEY", "value")] result

let test_parse_double_quoted () =
  let result = Env_file_parser.parse_string "KEY=\"hello world\"" in
  check
    (list (pair string string))
    "double-quoted value"
    [("KEY", "hello world")]
    result

let test_parse_single_quoted () =
  let result = Env_file_parser.parse_string "KEY='hello world'" in
  check
    (list (pair string string))
    "single-quoted value"
    [("KEY", "hello world")]
    result

let test_parse_value_with_equals () =
  let result = Env_file_parser.parse_string "KEY=a=b=c" in
  check
    (list (pair string string))
    "value with equals signs"
    [("KEY", "a=b=c")]
    result

let test_parse_empty_value () =
  let result = Env_file_parser.parse_string "KEY=" in
  check (list (pair string string)) "empty value" [("KEY", "")] result

let test_parse_multiple_entries () =
  let input = "KEY1=value1\nKEY2=value2\nKEY3=value3" in
  let result = Env_file_parser.parse_string input in
  check
    (list (pair string string))
    "multiple entries"
    [("KEY1", "value1"); ("KEY2", "value2"); ("KEY3", "value3")]
    result

let test_parse_comment_lines () =
  let input = "# This is a comment\nKEY=value\n# Another comment" in
  let result = Env_file_parser.parse_string input in
  check
    (list (pair string string))
    "comment lines skipped"
    [("KEY", "value")]
    result

let test_parse_blank_lines () =
  let input = "KEY1=value1\n\nKEY2=value2\n\n" in
  let result = Env_file_parser.parse_string input in
  check
    (list (pair string string))
    "blank lines skipped"
    [("KEY1", "value1"); ("KEY2", "value2")]
    result

let test_parse_mixed_content () =
  let input = "# Comment\n\nKEY1=value1\n# Another comment\n\nKEY2=value2\n" in
  let result = Env_file_parser.parse_string input in
  check
    (list (pair string string))
    "mixed content"
    [("KEY1", "value1"); ("KEY2", "value2")]
    result

let test_parse_key_without_equals () =
  let result = Env_file_parser.parse_string "KEY" in
  check (list (pair string string)) "key without equals" [("KEY", "")] result

let test_parse_whitespace_trimmed () =
  let input = "KEY=  value with spaces  " in
  let result = Env_file_parser.parse_string input in
  (* Note: The actual behavior depends on implementation - adjust if needed *)
  check
    (list (pair string string))
    "whitespace handling"
    [("KEY", "value with spaces")]
    result

(* ============================================================ *)
(* Env_file_parser.expand_vars Tests *)
(* ============================================================ *)

let test_expand_vars_braced () =
  let env = [("VAR", "value")] in
  let result = Env_file_parser.expand_vars ~env "${VAR}" in
  check string "expand ${VAR}" "value" result

let test_expand_vars_unbraced () =
  let env = [("VAR", "value")] in
  let result = Env_file_parser.expand_vars ~env "$VAR" in
  check string "expand $VAR" "value" result

let test_expand_vars_unknown () =
  let env = [("VAR", "value")] in
  let result = Env_file_parser.expand_vars ~env "${UNKNOWN}" in
  check string "unknown var preserved" "${UNKNOWN}" result

let test_expand_vars_multiple () =
  let env = [("VAR1", "hello"); ("VAR2", "world")] in
  let result = Env_file_parser.expand_vars ~env "${VAR1} ${VAR2}" in
  check string "multiple vars" "hello world" result

let test_expand_vars_empty_env () =
  let env = [] in
  let result = Env_file_parser.expand_vars ~env "${VAR}" in
  check string "empty env" "${VAR}" result

let test_expand_vars_no_vars () =
  let env = [("VAR", "value")] in
  let result = Env_file_parser.expand_vars ~env "plain text" in
  check string "no vars in string" "plain text" result

let test_expand_vars_mixed () =
  let env = [("HOME", "/home/user"); ("USER", "alice")] in
  let result = Env_file_parser.expand_vars ~env "User $USER home is ${HOME}" in
  check string "mixed expansion" "User alice home is /home/user" result

(* ============================================================ *)
(* External_service.suggest_instance_name Tests *)
(* ============================================================ *)

let test_suggest_instance_name_simple () =
  let result =
    External_service.suggest_instance_name ~unit_name:"my-node.service"
  in
  check string "simple service name" "my-node" result

let test_suggest_instance_name_octez_node_template () =
  let result =
    External_service.suggest_instance_name
      ~unit_name:"octez-node@mainnet.service"
  in
  check string "octez-node@mainnet" "mainnet" result

let test_suggest_instance_name_tezos_baker () =
  let result =
    External_service.suggest_instance_name
      ~unit_name:"tezos-baker-PsParisC.service"
  in
  check string "tezos-baker-PsParisC" "baker-PsParisC" result

let test_suggest_instance_name_octez_dal_node () =
  let result =
    External_service.suggest_instance_name
      ~unit_name:"octez-dal-node@my-dal.service"
  in
  check string "octez-dal-node@my-dal" "my-dal" result

let test_suggest_instance_name_no_suffix () =
  let result =
    External_service.suggest_instance_name ~unit_name:"custom-service"
  in
  check string "no .service suffix" "custom-service" result

let test_suggest_instance_name_template_unit () =
  let result =
    External_service.suggest_instance_name ~unit_name:"octez-baker@.service"
  in
  check string "template unit" "" result

let test_suggest_instance_name_signatory () =
  let result =
    External_service.suggest_instance_name ~unit_name:"signatory@prod.service"
  in
  check string "signatory@prod" "prod" result

(* ============================================================ *)
(* External_service.endpoint_matches_rpc Tests *)
(* ============================================================ *)

let test_endpoint_matches_localhost_127 () =
  let result =
    External_service.endpoint_matches_rpc
      ~endpoint:"http://localhost:8732"
      ~rpc_addr:"127.0.0.1:8732"
  in
  check bool "localhost matches 127.0.0.1" true result

let test_endpoint_matches_127_to_0000 () =
  let result =
    External_service.endpoint_matches_rpc
      ~endpoint:"http://127.0.0.1:8732"
      ~rpc_addr:"0.0.0.0:8732"
  in
  check bool "127.0.0.1 matches 0.0.0.0" true result

let test_endpoint_matches_localhost_exact () =
  let result =
    External_service.endpoint_matches_rpc
      ~endpoint:"http://localhost:8732"
      ~rpc_addr:"localhost:8732"
  in
  check bool "localhost matches localhost" true result

let test_endpoint_matches_different_host () =
  let result =
    External_service.endpoint_matches_rpc
      ~endpoint:"http://10.0.0.1:8732"
      ~rpc_addr:"127.0.0.1:8732"
  in
  check bool "different hosts don't match" false result

let test_endpoint_matches_different_port () =
  let result =
    External_service.endpoint_matches_rpc
      ~endpoint:"http://localhost:8733"
      ~rpc_addr:"127.0.0.1:8732"
  in
  check bool "different ports don't match" false result

let test_endpoint_matches_https () =
  let result =
    External_service.endpoint_matches_rpc
      ~endpoint:"https://localhost:443"
      ~rpc_addr:"localhost:443"
  in
  check bool "https endpoint matches" true result

(* ============================================================ *)
(* External_service.role_of_binary_name Tests *)
(* ============================================================ *)

let role_testable =
  testable
    (fun fmt role ->
      Format.fprintf fmt "%s" (External_service.role_to_string role))
    (fun a b ->
      String.equal
        (External_service.role_to_string a)
        (External_service.role_to_string b))

let test_role_of_binary_octez_node () =
  let result = External_service.role_of_binary_name "octez-node" in
  check role_testable "octez-node" External_service.Node result

let test_role_of_binary_octez_baker () =
  let result = External_service.role_of_binary_name "octez-baker-PsParisC" in
  check role_testable "octez-baker-PsParisC" External_service.Baker result

let test_role_of_binary_baker_with_dal_subcommand () =
  let result =
    External_service.role_of_binary_name
      ~subcommand:"dal"
      "octez-baker-PsParisC"
  in
  check
    role_testable
    "baker with dal subcommand"
    External_service.Dal_node
    result

let test_role_of_binary_baker_with_accuser_subcommand () =
  let result =
    External_service.role_of_binary_name
      ~subcommand:"accuser"
      "octez-baker-PsParisC"
  in
  check
    role_testable
    "baker with accuser subcommand"
    External_service.Accuser
    result

let test_role_of_binary_octez_accuser () =
  let result = External_service.role_of_binary_name "octez-accuser-PsParisC" in
  check role_testable "octez-accuser-PsParisC" External_service.Accuser result

let test_role_of_binary_octez_dal_node () =
  let result = External_service.role_of_binary_name "octez-dal-node" in
  check role_testable "octez-dal-node" External_service.Dal_node result

let test_role_of_binary_tezos_baker () =
  let result = External_service.role_of_binary_name "tezos-baker-PsParisC" in
  check role_testable "tezos-baker-PsParisC" External_service.Baker result

let test_role_of_binary_with_path () =
  let result = External_service.role_of_binary_name "/usr/bin/octez-node" in
  check role_testable "octez-node with path" External_service.Node result

let test_role_of_binary_signatory () =
  let result = External_service.role_of_binary_name "signatory" in
  check role_testable "signatory" External_service.Signatory result

let test_role_of_binary_unknown () =
  let result = External_service.role_of_binary_name "unknown-binary" in
  check
    role_testable
    "unknown binary"
    (External_service.Unknown "unknown-binary")
    result

(* ============================================================ *)
(* External_service.status_of_unit_state Tests *)
(* ============================================================ *)

let status_testable =
  testable
    (fun fmt status ->
      Format.fprintf fmt "%s" (External_service.status_label status))
    (fun a b ->
      String.equal
        (External_service.status_label a)
        (External_service.status_label b))

let test_status_running () =
  let unit_state =
    External_service.
      {active_state = "active"; sub_state = "running"; enabled = Some true}
  in
  let result = External_service.status_of_unit_state unit_state in
  check status_testable "running status" External_service.Running result

let test_status_disabled () =
  let unit_state =
    External_service.
      {active_state = "inactive"; sub_state = "dead"; enabled = Some false}
  in
  let result = External_service.status_of_unit_state unit_state in
  check status_testable "disabled status" External_service.Disabled result

let test_status_stopped () =
  let unit_state =
    External_service.
      {active_state = "inactive"; sub_state = "dead"; enabled = Some true}
  in
  let result = External_service.status_of_unit_state unit_state in
  check status_testable "stopped status" External_service.Stopped result

let test_status_failed () =
  let unit_state =
    External_service.
      {active_state = "failed"; sub_state = "exit-code"; enabled = Some true}
  in
  let result = External_service.status_of_unit_state unit_state in
  check
    status_testable
    "failed status"
    (External_service.Failed "exit-code")
    result

let test_status_unknown () =
  let unit_state =
    External_service.
      {active_state = "activating"; sub_state = "start"; enabled = None}
  in
  let result = External_service.status_of_unit_state unit_state in
  check
    status_testable
    "unknown status"
    (External_service.Unknown "activating/start")
    result

(* ============================================================ *)
(* External_service.unknown_field_count and unknown_field_names Tests *)
(* ============================================================ *)

let test_unknown_field_count_all_unknown () =
  let unit_state =
    External_service.
      {active_state = "active"; sub_state = "running"; enabled = Some true}
  in
  let config =
    External_service.empty_config
      ~unit_name:"test.service"
      ~exec_start:"/bin/test"
      ~unit_state
  in
  let count = External_service.unknown_field_count config in
  (* All key fields (role, binary_path, data_dir, rpc_addr, network) are unknown *)
  check int "all unknown" 5 count

let test_unknown_field_count_some_detected () =
  let unit_state =
    External_service.
      {active_state = "active"; sub_state = "running"; enabled = Some true}
  in
  let config =
    External_service.empty_config
      ~unit_name:"test.service"
      ~exec_start:"/bin/test"
      ~unit_state
  in
  let config =
    {
      config with
      role = External_service.detected ~source:"test" External_service.Node;
      binary_path = External_service.detected ~source:"test" "/bin/octez-node";
    }
  in
  let count = External_service.unknown_field_count config in
  (* role and binary_path detected, 3 remain unknown *)
  check int "some detected" 3 count

let test_unknown_field_count_all_detected () =
  let unit_state =
    External_service.
      {active_state = "active"; sub_state = "running"; enabled = Some true}
  in
  let config =
    External_service.empty_config
      ~unit_name:"test.service"
      ~exec_start:"/bin/test"
      ~unit_state
  in
  let config =
    {
      config with
      role = External_service.detected ~source:"test" External_service.Node;
      binary_path = External_service.detected ~source:"test" "/bin/octez-node";
      data_dir = External_service.detected ~source:"test" "/data";
      rpc_addr = External_service.detected ~source:"test" "127.0.0.1:8732";
      network = External_service.detected ~source:"test" "mainnet";
    }
  in
  let count = External_service.unknown_field_count config in
  check int "all detected" 0 count

let test_unknown_field_names_all_unknown () =
  let unit_state =
    External_service.
      {active_state = "active"; sub_state = "running"; enabled = Some true}
  in
  let config =
    External_service.empty_config
      ~unit_name:"test.service"
      ~exec_start:"/bin/test"
      ~unit_state
  in
  let names = External_service.unknown_field_names config in
  check
    (list string)
    "all unknown field names"
    ["role"; "binary_path"; "data_dir"; "rpc_addr"; "network"]
    names

let test_unknown_field_names_some_detected () =
  let unit_state =
    External_service.
      {active_state = "active"; sub_state = "running"; enabled = Some true}
  in
  let config =
    External_service.empty_config
      ~unit_name:"test.service"
      ~exec_start:"/bin/test"
      ~unit_state
  in
  let config =
    {
      config with
      role = External_service.detected ~source:"test" External_service.Node;
      binary_path = External_service.detected ~source:"test" "/bin/octez-node";
    }
  in
  let names = External_service.unknown_field_names config in
  check
    (list string)
    "some detected field names"
    ["data_dir"; "rpc_addr"; "network"]
    names

(* ============================================================ *)
(* External_service.get_dependencies and get_dependents Tests *)
(* ============================================================ *)

let make_test_service ~unit_name ~role ~rpc_addr ~node_endpoint =
  let unit_state =
    External_service.
      {active_state = "active"; sub_state = "running"; enabled = Some true}
  in
  let config =
    External_service.empty_config ~unit_name ~exec_start:"/bin/test" ~unit_state
  in
  let config =
    {
      config with
      role = External_service.detected ~source:"test" role;
      rpc_addr =
        (match rpc_addr with
        | Some addr -> External_service.detected ~source:"test" addr
        | None -> External_service.unknown ());
      node_endpoint =
        (match node_endpoint with
        | Some endpoint -> External_service.detected ~source:"test" endpoint
        | None -> External_service.unknown ());
    }
  in
  External_service.
    {
      config;
      suggested_instance_name =
        External_service.suggest_instance_name ~unit_name;
    }

let test_get_dependencies_baker_to_node () =
  let node =
    make_test_service
      ~unit_name:"octez-node@mainnet.service"
      ~role:External_service.Node
      ~rpc_addr:(Some "127.0.0.1:8732")
      ~node_endpoint:None
  in
  let baker =
    make_test_service
      ~unit_name:"octez-baker@mainnet.service"
      ~role:External_service.Baker
      ~rpc_addr:None
      ~node_endpoint:(Some "http://localhost:8732")
  in
  let deps = External_service.get_dependencies baker [node; baker] in
  check
    (list (pair string string))
    "baker depends on node"
    [("octez-node@mainnet.service", "node")]
    deps

let test_get_dependencies_baker_no_match () =
  let node =
    make_test_service
      ~unit_name:"octez-node@mainnet.service"
      ~role:External_service.Node
      ~rpc_addr:(Some "127.0.0.1:8732")
      ~node_endpoint:None
  in
  let baker =
    make_test_service
      ~unit_name:"octez-baker@mainnet.service"
      ~role:External_service.Baker
      ~rpc_addr:None
      ~node_endpoint:(Some "http://10.0.0.1:8732")
  in
  let deps = External_service.get_dependencies baker [node; baker] in
  check (list (pair string string)) "baker no matching node" [] deps

let test_get_dependencies_node_has_none () =
  let node =
    make_test_service
      ~unit_name:"octez-node@mainnet.service"
      ~role:External_service.Node
      ~rpc_addr:(Some "127.0.0.1:8732")
      ~node_endpoint:None
  in
  let deps = External_service.get_dependencies node [node] in
  check (list (pair string string)) "node has no dependencies" [] deps

let test_get_dependents_node_has_baker () =
  let node =
    make_test_service
      ~unit_name:"octez-node@mainnet.service"
      ~role:External_service.Node
      ~rpc_addr:(Some "127.0.0.1:8732")
      ~node_endpoint:None
  in
  let baker =
    make_test_service
      ~unit_name:"octez-baker@mainnet.service"
      ~role:External_service.Baker
      ~rpc_addr:None
      ~node_endpoint:(Some "http://localhost:8732")
  in
  let dependents = External_service.get_dependents node [node; baker] in
  check
    (list (pair string string))
    "node has baker dependent"
    [("octez-baker@mainnet.service", "baker")]
    dependents

let test_get_dependents_node_has_accuser () =
  let node =
    make_test_service
      ~unit_name:"octez-node@mainnet.service"
      ~role:External_service.Node
      ~rpc_addr:(Some "127.0.0.1:8732")
      ~node_endpoint:None
  in
  let accuser =
    make_test_service
      ~unit_name:"octez-accuser@mainnet.service"
      ~role:External_service.Accuser
      ~rpc_addr:None
      ~node_endpoint:(Some "http://localhost:8732")
  in
  let dependents = External_service.get_dependents node [node; accuser] in
  check
    (list (pair string string))
    "node has accuser dependent"
    [("octez-accuser@mainnet.service", "accuser")]
    dependents

(* ============================================================ *)
(* Import.For_tests.missing_required_fields Tests *)
(* ============================================================ *)

let test_missing_required_fields_node_all_present () =
  let unit_state =
    External_service.
      {active_state = "active"; sub_state = "running"; enabled = Some true}
  in
  let config =
    External_service.empty_config
      ~unit_name:"octez-node@mainnet.service"
      ~exec_start:"/bin/octez-node"
      ~unit_state
  in
  let config =
    {
      config with
      role = External_service.detected ~source:"test" External_service.Node;
      network = External_service.detected ~source:"test" "mainnet";
      data_dir = External_service.detected ~source:"test" "/data";
    }
  in
  let external_svc =
    External_service.
      {
        config;
        suggested_instance_name =
          External_service.suggest_instance_name
            ~unit_name:"octez-node@mainnet.service";
      }
  in
  let missing = Import.For_tests.missing_required_fields external_svc in
  check (list string) "node all fields present" [] missing

let test_missing_required_fields_node_missing_network () =
  let unit_state =
    External_service.
      {active_state = "active"; sub_state = "running"; enabled = Some true}
  in
  let config =
    External_service.empty_config
      ~unit_name:"octez-node@mainnet.service"
      ~exec_start:"/bin/octez-node"
      ~unit_state
  in
  let config =
    {
      config with
      role = External_service.detected ~source:"test" External_service.Node;
      data_dir = External_service.detected ~source:"test" "/data";
    }
  in
  let external_svc =
    External_service.
      {
        config;
        suggested_instance_name =
          External_service.suggest_instance_name
            ~unit_name:"octez-node@mainnet.service";
      }
  in
  let missing = Import.For_tests.missing_required_fields external_svc in
  check (list string) "node missing network" ["network"] missing

let test_missing_required_fields_node_with_network_override () =
  let unit_state =
    External_service.
      {active_state = "active"; sub_state = "running"; enabled = Some true}
  in
  let config =
    External_service.empty_config
      ~unit_name:"octez-node@mainnet.service"
      ~exec_start:"/bin/octez-node"
      ~unit_state
  in
  let config =
    {
      config with
      role = External_service.detected ~source:"test" External_service.Node;
      data_dir = External_service.detected ~source:"test" "/data";
    }
  in
  let external_svc =
    External_service.
      {
        config;
        suggested_instance_name =
          External_service.suggest_instance_name
            ~unit_name:"octez-node@mainnet.service";
      }
  in
  let missing =
    Import.For_tests.missing_required_fields
      ~network_override:"mainnet"
      external_svc
  in
  check (list string) "node with network override" [] missing

let test_missing_required_fields_baker_missing_multiple () =
  let unit_state =
    External_service.
      {active_state = "active"; sub_state = "running"; enabled = Some true}
  in
  let config =
    External_service.empty_config
      ~unit_name:"octez-baker@mainnet.service"
      ~exec_start:"/bin/octez-baker"
      ~unit_state
  in
  let config =
    {
      config with
      role = External_service.detected ~source:"test" External_service.Baker;
      network = External_service.detected ~source:"test" "mainnet";
    }
  in
  let external_svc =
    External_service.
      {
        config;
        suggested_instance_name =
          External_service.suggest_instance_name
            ~unit_name:"octez-baker@mainnet.service";
      }
  in
  let missing = Import.For_tests.missing_required_fields external_svc in
  (* Baker requires: network, base_dir, node_endpoint *)
  check
    (list string)
    "baker missing base_dir and node_endpoint"
    ["base_dir"; "node_endpoint"]
    missing

let test_missing_required_fields_baker_all_present () =
  let unit_state =
    External_service.
      {active_state = "active"; sub_state = "running"; enabled = Some true}
  in
  let config =
    External_service.empty_config
      ~unit_name:"octez-baker@mainnet.service"
      ~exec_start:"/bin/octez-baker"
      ~unit_state
  in
  let config =
    {
      config with
      role = External_service.detected ~source:"test" External_service.Baker;
      network = External_service.detected ~source:"test" "mainnet";
      base_dir = External_service.detected ~source:"test" "/base";
      node_endpoint =
        External_service.detected ~source:"test" "http://localhost:8732";
    }
  in
  let external_svc =
    External_service.
      {
        config;
        suggested_instance_name =
          External_service.suggest_instance_name
            ~unit_name:"octez-baker@mainnet.service";
      }
  in
  let missing = Import.For_tests.missing_required_fields external_svc in
  check (list string) "baker all fields present" [] missing

(* ============================================================ *)
(* Test Suite *)
(* ============================================================ *)

let () =
  run
    "Import Config Preservation"
    [
      ( "Env_file_parser.parse_string",
        [
          test_case "simple key=value" `Quick test_parse_simple_key_value;
          test_case "double-quoted value" `Quick test_parse_double_quoted;
          test_case "single-quoted value" `Quick test_parse_single_quoted;
          test_case "value with equals" `Quick test_parse_value_with_equals;
          test_case "empty value" `Quick test_parse_empty_value;
          test_case "multiple entries" `Quick test_parse_multiple_entries;
          test_case "comment lines" `Quick test_parse_comment_lines;
          test_case "blank lines" `Quick test_parse_blank_lines;
          test_case "mixed content" `Quick test_parse_mixed_content;
          test_case "key without equals" `Quick test_parse_key_without_equals;
          test_case "whitespace trimmed" `Quick test_parse_whitespace_trimmed;
        ] );
      ( "Env_file_parser.expand_vars",
        [
          test_case "expand ${VAR}" `Quick test_expand_vars_braced;
          test_case "expand $VAR" `Quick test_expand_vars_unbraced;
          test_case "unknown var" `Quick test_expand_vars_unknown;
          test_case "multiple vars" `Quick test_expand_vars_multiple;
          test_case "empty env" `Quick test_expand_vars_empty_env;
          test_case "no vars in string" `Quick test_expand_vars_no_vars;
          test_case "mixed expansion" `Quick test_expand_vars_mixed;
        ] );
      ( "External_service.suggest_instance_name",
        [
          test_case
            "simple service name"
            `Quick
            test_suggest_instance_name_simple;
          test_case
            "octez-node@mainnet"
            `Quick
            test_suggest_instance_name_octez_node_template;
          test_case
            "tezos-baker-PsParisC"
            `Quick
            test_suggest_instance_name_tezos_baker;
          test_case
            "octez-dal-node@my-dal"
            `Quick
            test_suggest_instance_name_octez_dal_node;
          test_case
            "no .service suffix"
            `Quick
            test_suggest_instance_name_no_suffix;
          test_case
            "template unit"
            `Quick
            test_suggest_instance_name_template_unit;
          test_case "signatory@prod" `Quick test_suggest_instance_name_signatory;
        ] );
      ( "External_service.endpoint_matches_rpc",
        [
          test_case
            "localhost matches 127.0.0.1"
            `Quick
            test_endpoint_matches_localhost_127;
          test_case
            "127.0.0.1 matches 0.0.0.0"
            `Quick
            test_endpoint_matches_127_to_0000;
          test_case
            "localhost matches localhost"
            `Quick
            test_endpoint_matches_localhost_exact;
          test_case
            "different hosts"
            `Quick
            test_endpoint_matches_different_host;
          test_case
            "different ports"
            `Quick
            test_endpoint_matches_different_port;
          test_case "https endpoint" `Quick test_endpoint_matches_https;
        ] );
      ( "External_service.role_of_binary_name",
        [
          test_case "octez-node" `Quick test_role_of_binary_octez_node;
          test_case
            "octez-baker-PsParisC"
            `Quick
            test_role_of_binary_octez_baker;
          test_case
            "baker with dal subcommand"
            `Quick
            test_role_of_binary_baker_with_dal_subcommand;
          test_case
            "baker with accuser subcommand"
            `Quick
            test_role_of_binary_baker_with_accuser_subcommand;
          test_case
            "octez-accuser-PsParisC"
            `Quick
            test_role_of_binary_octez_accuser;
          test_case "octez-dal-node" `Quick test_role_of_binary_octez_dal_node;
          test_case
            "tezos-baker-PsParisC"
            `Quick
            test_role_of_binary_tezos_baker;
          test_case "octez-node with path" `Quick test_role_of_binary_with_path;
          test_case "signatory" `Quick test_role_of_binary_signatory;
          test_case "unknown binary" `Quick test_role_of_binary_unknown;
        ] );
      ( "External_service.status_of_unit_state",
        [
          test_case "running status" `Quick test_status_running;
          test_case "disabled status" `Quick test_status_disabled;
          test_case "stopped status" `Quick test_status_stopped;
          test_case "failed status" `Quick test_status_failed;
          test_case "unknown status" `Quick test_status_unknown;
        ] );
      ( "External_service.unknown_field_count",
        [
          test_case "all unknown" `Quick test_unknown_field_count_all_unknown;
          test_case
            "some detected"
            `Quick
            test_unknown_field_count_some_detected;
          test_case "all detected" `Quick test_unknown_field_count_all_detected;
        ] );
      ( "External_service.unknown_field_names",
        [
          test_case "all unknown" `Quick test_unknown_field_names_all_unknown;
          test_case
            "some detected"
            `Quick
            test_unknown_field_names_some_detected;
        ] );
      ( "External_service.get_dependencies",
        [
          test_case "baker to node" `Quick test_get_dependencies_baker_to_node;
          test_case "baker no match" `Quick test_get_dependencies_baker_no_match;
          test_case "node has none" `Quick test_get_dependencies_node_has_none;
        ] );
      ( "External_service.get_dependents",
        [
          test_case "node has baker" `Quick test_get_dependents_node_has_baker;
          test_case
            "node has accuser"
            `Quick
            test_get_dependents_node_has_accuser;
        ] );
      ( "Import.For_tests.missing_required_fields",
        [
          test_case
            "node all fields present"
            `Quick
            test_missing_required_fields_node_all_present;
          test_case
            "node missing network"
            `Quick
            test_missing_required_fields_node_missing_network;
          test_case
            "node with network override"
            `Quick
            test_missing_required_fields_node_with_network_override;
          test_case
            "baker missing multiple"
            `Quick
            test_missing_required_fields_baker_missing_multiple;
          test_case
            "baker all fields present"
            `Quick
            test_missing_required_fields_baker_all_present;
        ] );
    ]
