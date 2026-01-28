(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Unit tests for Data module (service state, summaries, formatters). *)

open Alcotest
module Data = Octez_manager_ui.Data
module Service = Octez_manager_lib.Service
module Systemd = Octez_manager_lib.Systemd

let make_service ?(instance = "my-node") ?(role = "node") ?(network = "mainnet")
    () : Service.t =
  {
    instance;
    role;
    network;
    history_mode = Octez_manager_lib.History_mode.Rolling;
    data_dir = "/var/lib/octez/node";
    rpc_addr = "127.0.0.1:8732";
    net_addr = "0.0.0.0:9732";
    service_user = "tezos";
    app_bin_dir = "/usr/bin";
    bin_source = None;
    created_at = "2026-01-01 00:00:00";
    logging_mode = Octez_manager_lib.Logging_mode.Journald;
    snapshot_auto = false;
    snapshot_uri = None;
    snapshot_network_slug = None;
    snapshot_no_check = false;
    extra_args = [];
    depends_on = None;
    dependents = [];
  }

let make_state ?(instance = "my-node") ?(role = "node") ?(enabled = Some true)
    ?(active = Some true) ?(status = Data.Service_state.Running)
    ?(status_text = None) () : Data.Service_state.t =
  {
    service = make_service ~instance ~role ();
    enabled;
    active;
    status;
    status_text;
  }

(* ── parse_enabled_response ──────────────────────────────────── *)

let test_parse_enabled () =
  check
    (option bool)
    "enabled"
    (Some true)
    (Data.For_tests.parse_enabled_response "enabled")

let test_parse_disabled () =
  check
    (option bool)
    "disabled"
    (Some false)
    (Data.For_tests.parse_enabled_response "disabled")

let test_parse_static () =
  check
    (option bool)
    "static"
    (Some true)
    (Data.For_tests.parse_enabled_response "static")

let test_parse_enabled_whitespace () =
  check
    (option bool)
    "whitespace"
    (Some true)
    (Data.For_tests.parse_enabled_response "  enabled  ")

let test_parse_enabled_case () =
  check
    (option bool)
    "ENABLED"
    (Some true)
    (Data.For_tests.parse_enabled_response "ENABLED")

let test_parse_enabled_unknown () =
  check
    (option bool)
    "masked"
    None
    (Data.For_tests.parse_enabled_response "masked")

let test_parse_enabled_empty () =
  check (option bool) "empty" None (Data.For_tests.parse_enabled_response "")

(* ── classify_unit_state ─────────────────────────────────────── *)

let test_classify_active () =
  let result =
    Ok
      Systemd.
        {
          active_state = "active";
          sub_state = "running";
          result = None;
          exit_status = Some 0;
        }
  in
  let active, status = Data.For_tests.classify_unit_state result in
  check (option bool) "active" (Some true) active ;
  check
    bool
    "running"
    true
    (match status with Data.Service_state.Running -> true | _ -> false)

let test_classify_inactive () =
  let result =
    Ok
      Systemd.
        {
          active_state = "inactive";
          sub_state = "dead";
          result = None;
          exit_status = None;
        }
  in
  let active, status = Data.For_tests.classify_unit_state result in
  check (option bool) "inactive" (Some false) active ;
  check
    bool
    "stopped"
    true
    (match status with Data.Service_state.Stopped -> true | _ -> false)

let test_classify_failed_signal () =
  let result =
    Ok
      Systemd.
        {
          active_state = "failed";
          sub_state = "failed";
          result = Some "signal";
          exit_status = Some 127;
        }
  in
  let _active, status = Data.For_tests.classify_unit_state result in
  check
    bool
    "stopped (signal)"
    true
    (match status with Data.Service_state.Stopped -> true | _ -> false)

let test_classify_failed_exit_code () =
  let result =
    Ok
      Systemd.
        {
          active_state = "failed";
          sub_state = "failed";
          result = Some "exit-code";
          exit_status = Some 1;
        }
  in
  let _active, status = Data.For_tests.classify_unit_state result in
  check
    bool
    "unknown (exit code)"
    true
    (match status with Data.Service_state.Unknown _ -> true | _ -> false)

let test_classify_error () =
  let result = Error (`Msg "connection refused") in
  let active, status = Data.For_tests.classify_unit_state result in
  check (option bool) "unknown active" None active ;
  check
    bool
    "unknown status"
    true
    (match status with Data.Service_state.Unknown _ -> true | _ -> false)

(* ── status_label ────────────────────────────────────────────── *)

let test_status_label_running () =
  let state = make_state ~status:Data.Service_state.Running () in
  check string "running" "running" (Data.Service_state.status_label state)

let test_status_label_stopped () =
  let state = make_state ~status:Data.Service_state.Stopped () in
  check string "stopped" "stopped" (Data.Service_state.status_label state)

let test_status_label_unknown () =
  let state = make_state ~status:(Data.Service_state.Unknown "oops") () in
  check
    string
    "unknown"
    "unknown (oops)"
    (Data.Service_state.status_label state)

(* ── summarize ───────────────────────────────────────────────── *)

let test_summarize_empty () =
  let s = Data.summarize [] in
  check int "total" 0 s.total ;
  check int "running" 0 s.running

let test_summarize_mixed () =
  let states =
    [
      make_state ~instance:"a" ~status:Data.Service_state.Running ();
      make_state ~instance:"b" ~status:Data.Service_state.Stopped ();
      make_state ~instance:"c" ~status:(Data.Service_state.Unknown "err") ();
    ]
  in
  let s = Data.summarize states in
  check int "total" 3 s.total ;
  check int "running" 1 s.running ;
  check int "stopped" 1 s.stopped ;
  check int "unknown" 1 s.unknown

let test_summarize_all_running () =
  let states = [make_state ~instance:"a" (); make_state ~instance:"b" ()] in
  let s = Data.summarize states in
  check int "total" 2 s.total ;
  check int "running" 2 s.running ;
  check int "stopped" 0 s.stopped

(* ── diagnostics_lines ───────────────────────────────────────── *)

let test_diagnostics_empty () =
  let lines = Data.diagnostics_lines [] in
  check int "header only" 2 (List.length lines)

let test_diagnostics_with_service () =
  let states = [make_state ~instance:"my-node" ~enabled:(Some true) ()] in
  let lines = Data.diagnostics_lines states in
  check
    bool
    "has instance"
    true
    (List.exists (fun l -> String.contains l 'm') lines) ;
  check
    bool
    "has enabled"
    true
    (List.exists
       (fun l ->
         let len = String.length l in
         len > 8
         &&
           try
             ignore (Str.search_forward (Str.regexp_string "[enabled]") l 0) ;
             true
           with Not_found -> false)
       lines)

(* ── activity_lines ──────────────────────────────────────────── *)

let test_activity_empty () =
  let lines = Data.activity_lines [] in
  check int "header only" 2 (List.length lines)

let test_activity_with_service () =
  let states = [make_state ~instance:"my-node" ()] in
  let lines = Data.activity_lines states in
  check int "header + 1" 3 (List.length lines)

(* ── spotlight_lines ─────────────────────────────────────────── *)

let test_spotlight_empty () =
  let lines = Data.spotlight_lines [] ~limit:5 in
  check int "empty" 0 (List.length lines)

let test_spotlight_running () =
  let states = [make_state ~instance:"my-node" ()] in
  let lines = Data.spotlight_lines states ~limit:5 in
  check int "one line" 1 (List.length lines) ;
  check bool "has bullet" true (String.length (List.hd lines) > 0)

let test_spotlight_limit () =
  let states =
    [
      make_state ~instance:"a" ();
      make_state ~instance:"b" ();
      make_state ~instance:"c" ();
    ]
  in
  let lines = Data.spotlight_lines states ~limit:2 in
  check int "limited" 2 (List.length lines)

(* ── formatted_timestamp ─────────────────────────────────────── *)

let test_formatted_timestamp () =
  let ts = Data.formatted_timestamp 0.0 in
  check bool "has dashes" true (String.contains ts '-') ;
  check bool "has colons" true (String.contains ts ':') ;
  check int "length" 19 (String.length ts)

(* ── Suite ───────────────────────────────────────────────────── *)

let () =
  run
    "Data"
    [
      ( "parse_enabled_response",
        [
          test_case "enabled" `Quick test_parse_enabled;
          test_case "disabled" `Quick test_parse_disabled;
          test_case "static" `Quick test_parse_static;
          test_case "whitespace" `Quick test_parse_enabled_whitespace;
          test_case "case insensitive" `Quick test_parse_enabled_case;
          test_case "unknown" `Quick test_parse_enabled_unknown;
          test_case "empty" `Quick test_parse_enabled_empty;
        ] );
      ( "classify_unit_state",
        [
          test_case "active" `Quick test_classify_active;
          test_case "inactive" `Quick test_classify_inactive;
          test_case "failed signal" `Quick test_classify_failed_signal;
          test_case "failed exit code" `Quick test_classify_failed_exit_code;
          test_case "error" `Quick test_classify_error;
        ] );
      ( "status_label",
        [
          test_case "running" `Quick test_status_label_running;
          test_case "stopped" `Quick test_status_label_stopped;
          test_case "unknown" `Quick test_status_label_unknown;
        ] );
      ( "summarize",
        [
          test_case "empty" `Quick test_summarize_empty;
          test_case "mixed" `Quick test_summarize_mixed;
          test_case "all running" `Quick test_summarize_all_running;
        ] );
      ( "diagnostics_lines",
        [
          test_case "empty" `Quick test_diagnostics_empty;
          test_case "with service" `Quick test_diagnostics_with_service;
        ] );
      ( "activity_lines",
        [
          test_case "empty" `Quick test_activity_empty;
          test_case "with service" `Quick test_activity_with_service;
        ] );
      ( "spotlight_lines",
        [
          test_case "empty" `Quick test_spotlight_empty;
          test_case "running" `Quick test_spotlight_running;
          test_case "limit" `Quick test_spotlight_limit;
        ] );
      ( "formatted_timestamp",
        [test_case "format" `Quick test_formatted_timestamp] );
    ]
