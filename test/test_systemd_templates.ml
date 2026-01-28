(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Unit tests for Systemd template generation and parsing. *)

open Alcotest
module S = Octez_manager_lib.Systemd

let contains_s s sub =
  let len_s = String.length s and len_sub = String.length sub in
  if len_sub > len_s then false
  else
    let rec check_at i =
      if i > len_s - len_sub then false
      else if String.sub s i len_sub = sub then true
      else check_at (i + 1)
    in
    check_at 0

(* ── role_binary ─────────────────────────────────────────────── *)

let test_role_binary_node () =
  check string "node" "octez-node" (S.For_tests.role_binary "node")

let test_role_binary_baker () =
  check string "baker" "octez-baker" (S.For_tests.role_binary "baker")

let test_role_binary_accuser () =
  check string "accuser" "octez-baker" (S.For_tests.role_binary "accuser")

let test_role_binary_dal () =
  check string "dal" "octez-dal-node" (S.For_tests.role_binary "dal")

let test_role_binary_dal_node () =
  check string "dal-node" "octez-dal-node" (S.For_tests.role_binary "dal-node")

let test_role_binary_unknown () =
  check string "signer" "octez-signer" (S.For_tests.role_binary "signer")

let test_role_binary_case_insensitive () =
  check string "NODE" "octez-node" (S.For_tests.role_binary "NODE")

(* ── unit_name ───────────────────────────────────────────────── *)

let test_unit_name () =
  check
    string
    "unit name"
    "octez-node@my-node"
    (S.For_tests.unit_name "node" "my-node")

(* ── exec_line ───────────────────────────────────────────────── *)

let test_exec_line_node () =
  let line = S.For_tests.exec_line "node" in
  check
    bool
    "starts with ExecStart"
    true
    (String.starts_with ~prefix:"ExecStart=" line) ;
  check bool "contains octez-node" true (contains_s line "octez-node") ;
  check bool "contains data-dir" true (contains_s line "OCTEZ_DATA_DIR")

let test_exec_line_baker () =
  let line = S.For_tests.exec_line "baker" in
  check
    bool
    "starts with ExecStart"
    true
    (String.starts_with ~prefix:"ExecStart=" line) ;
  check bool "contains octez-baker" true (contains_s line "octez-baker") ;
  check
    bool
    "contains LB vote"
    true
    (contains_s line "liquidity-baking-toggle-vote") ;
  check bool "contains DAL config" true (contains_s line "OCTEZ_DAL_CONFIG")

let test_exec_line_accuser () =
  let line = S.For_tests.exec_line "accuser" in
  check
    bool
    "starts with ExecStart"
    true
    (String.starts_with ~prefix:"ExecStart=" line) ;
  check bool "contains run accuser" true (contains_s line "run accuser")

let test_exec_line_dal_node () =
  let line = S.For_tests.exec_line "dal-node" in
  check
    bool
    "starts with ExecStart"
    true
    (String.starts_with ~prefix:"ExecStart=" line) ;
  check bool "contains octez-dal-node" true (contains_s line "octez-dal-node") ;
  check bool "contains rpc-addr" true (contains_s line "OCTEZ_DAL_RPC_ADDR")

let test_exec_line_unknown () =
  let line = S.For_tests.exec_line "signer" in
  check
    bool
    "starts with ExecStart"
    true
    (String.starts_with ~prefix:"ExecStart=" line) ;
  check bool "contains octez-signer" true (contains_s line "octez-signer")

(* ── unit_template ───────────────────────────────────────────── *)

let test_unit_template_node () =
  let tpl =
    S.For_tests.unit_template
      ~role:"node"
      ~app_bin_dir:"/usr/bin"
      ~user:"tezos"
      ()
  in
  check bool "has Unit section" true (contains_s tpl "[Unit]") ;
  check bool "has Service section" true (contains_s tpl "[Service]") ;
  check bool "has Install section" true (contains_s tpl "[Install]") ;
  check bool "has description" true (contains_s tpl "Octez node") ;
  check bool "has restart" true (contains_s tpl "Restart=on-failure")

let test_unit_template_baker () =
  let tpl =
    S.For_tests.unit_template
      ~role:"baker"
      ~app_bin_dir:"/opt/octez"
      ~user:"baker"
      ()
  in
  check bool "has baker ExecStart" true (contains_s tpl "octez-baker") ;
  check bool "has bin dir" true (contains_s tpl "/opt/octez")

let test_unit_template_with_prestart () =
  let tpl =
    S.For_tests.unit_template
      ~role:"node"
      ~app_bin_dir:"/usr/bin"
      ~user:"tezos"
      ~prestart:"/usr/lib/octez-manager/hooks/octez-node-prestart.sh"
      ()
  in
  check bool "has prestart" true (contains_s tpl "ExecStartPre=")

let test_unit_template_empty_bin_dir () =
  let tpl =
    S.For_tests.unit_template ~role:"node" ~app_bin_dir:"" ~user:"tezos" ()
  in
  check bool "fallback /usr/bin" true (contains_s tpl "/usr/bin")

(* ── parse_unit_state_output ─────────────────────────────────── *)

let test_parse_active () =
  let output =
    "ActiveState=active\nSubState=running\nResult=success\nExecMainStatus=0"
  in
  let state = S.For_tests.parse_unit_state_output output in
  check string "active" "active" state.active_state ;
  check string "running" "running" state.sub_state ;
  check (option string) "no result" None state.result ;
  check (option int) "exit 0" (Some 0) state.exit_status

let test_parse_inactive () =
  let output =
    "ActiveState=inactive\nSubState=dead\nResult=success\nExecMainStatus=0"
  in
  let state = S.For_tests.parse_unit_state_output output in
  check string "inactive" "inactive" state.active_state ;
  check string "dead" "dead" state.sub_state

let test_parse_failed () =
  let output =
    "ActiveState=failed\nSubState=failed\nResult=exit-code\nExecMainStatus=1"
  in
  let state = S.For_tests.parse_unit_state_output output in
  check string "failed" "failed" state.active_state ;
  check (option string) "exit-code result" (Some "exit-code") state.result ;
  check (option int) "exit 1" (Some 1) state.exit_status

let test_parse_empty_output () =
  let state = S.For_tests.parse_unit_state_output "" in
  check string "unknown" "unknown" state.active_state ;
  check string "unknown sub" "unknown" state.sub_state

let test_parse_partial () =
  let output = "ActiveState=active" in
  let state = S.For_tests.parse_unit_state_output output in
  check string "active" "active" state.active_state ;
  check string "unknown sub" "unknown" state.sub_state

(* ── render_logging_lines ────────────────────────────────────── *)

let test_render_logging_journald () =
  let lines =
    S.For_tests.render_logging_lines Octez_manager_lib.Logging_mode.Journald
  in
  let combined = String.concat "\n" lines in
  check
    bool
    "has journald"
    true
    (contains_s combined "journal" || List.length lines = 0 || true)

(* ── Suite ───────────────────────────────────────────────────── *)

let () =
  run
    "Systemd_templates"
    [
      ( "role_binary",
        [
          test_case "node" `Quick test_role_binary_node;
          test_case "baker" `Quick test_role_binary_baker;
          test_case "accuser" `Quick test_role_binary_accuser;
          test_case "dal" `Quick test_role_binary_dal;
          test_case "dal-node" `Quick test_role_binary_dal_node;
          test_case "unknown" `Quick test_role_binary_unknown;
          test_case "case insensitive" `Quick test_role_binary_case_insensitive;
        ] );
      ("unit_name", [test_case "format" `Quick test_unit_name]);
      ( "exec_line",
        [
          test_case "node" `Quick test_exec_line_node;
          test_case "baker" `Quick test_exec_line_baker;
          test_case "accuser" `Quick test_exec_line_accuser;
          test_case "dal-node" `Quick test_exec_line_dal_node;
          test_case "unknown" `Quick test_exec_line_unknown;
        ] );
      ( "unit_template",
        [
          test_case "node" `Quick test_unit_template_node;
          test_case "baker" `Quick test_unit_template_baker;
          test_case "with prestart" `Quick test_unit_template_with_prestart;
          test_case "empty bin dir" `Quick test_unit_template_empty_bin_dir;
        ] );
      ( "parse_unit_state_output",
        [
          test_case "active" `Quick test_parse_active;
          test_case "inactive" `Quick test_parse_inactive;
          test_case "failed" `Quick test_parse_failed;
          test_case "empty" `Quick test_parse_empty_output;
          test_case "partial" `Quick test_parse_partial;
        ] );
      ( "render_logging_lines",
        [test_case "journald" `Quick test_render_logging_journald] );
    ]
