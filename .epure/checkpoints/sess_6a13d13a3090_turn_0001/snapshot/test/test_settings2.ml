(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Unit tests for Settings JSON serialization. *)

open Alcotest

(* ── Roundtrip ───────────────────────────────────────────────── *)

let test_default_roundtrip () =
  let d = Octez_manager_lib.Settings.For_tests.default in
  let json = Octez_manager_lib.Settings.For_tests.to_yojson d in
  match Octez_manager_lib.Settings.For_tests.of_yojson json with
  | Ok t ->
      check (option string) "app_bin_dir" d.app_bin_dir t.app_bin_dir ;
      check
        (option string)
        "history_mode"
        (Option.map
           Octez_manager_lib.History_mode.to_string
           d.default_history_mode)
        (Option.map
           Octez_manager_lib.History_mode.to_string
           t.default_history_mode) ;
      check
        (option string)
        "logging_mode"
        (Option.map
           Octez_manager_lib.Logging_mode.to_string
           d.default_logging_mode)
        (Option.map
           Octez_manager_lib.Logging_mode.to_string
           t.default_logging_mode)
  | Error (`Msg m) -> fail m

let test_with_bin_dir () =
  let t : Octez_manager_lib.Settings.t =
    {
      Octez_manager_lib.Settings.For_tests.default with
      app_bin_dir = Some "/opt/octez/bin";
    }
  in
  let json = Octez_manager_lib.Settings.For_tests.to_yojson t in
  match Octez_manager_lib.Settings.For_tests.of_yojson json with
  | Ok t2 ->
      check (option string) "bin dir" (Some "/opt/octez/bin") t2.app_bin_dir
  | Error (`Msg m) -> fail m

let test_with_history_mode () =
  let t : Octez_manager_lib.Settings.t =
    {
      Octez_manager_lib.Settings.For_tests.default with
      default_history_mode = Some Octez_manager_lib.History_mode.Archive;
    }
  in
  let json = Octez_manager_lib.Settings.For_tests.to_yojson t in
  match Octez_manager_lib.Settings.For_tests.of_yojson json with
  | Ok t2 ->
      check
        (option string)
        "history mode"
        (Some "archive")
        (Option.map
           Octez_manager_lib.History_mode.to_string
           t2.default_history_mode)
  | Error (`Msg m) -> fail m

let test_with_logging_mode () =
  let t : Octez_manager_lib.Settings.t =
    {
      Octez_manager_lib.Settings.For_tests.default with
      default_logging_mode = Some Octez_manager_lib.Logging_mode.Journald;
    }
  in
  let json = Octez_manager_lib.Settings.For_tests.to_yojson t in
  match Octez_manager_lib.Settings.For_tests.of_yojson json with
  | Ok t2 ->
      check
        (option string)
        "logging mode"
        (Some "journald")
        (Option.map
           Octez_manager_lib.Logging_mode.to_string
           t2.default_logging_mode)
  | Error (`Msg m) -> fail m

(* ── Malformed input ─────────────────────────────────────────── *)

let test_empty_json_object () =
  let json = `Assoc [] in
  match Octez_manager_lib.Settings.For_tests.of_yojson json with
  | Ok t ->
      check (option string) "defaults" None t.app_bin_dir ;
      check
        (option string)
        "no history"
        None
        (Option.map
           Octez_manager_lib.History_mode.to_string
           t.default_history_mode)
  | Error (`Msg m) -> fail m

let test_null_fields () =
  let json =
    `Assoc
      [
        ("app_bin_dir", `Null);
        ("default_history_mode", `Null);
        ("default_logging_mode", `Null);
      ]
  in
  match Octez_manager_lib.Settings.For_tests.of_yojson json with
  | Ok t -> check (option string) "all None" None t.app_bin_dir
  | Error (`Msg m) -> fail m

let test_extra_fields_ignored () =
  let json =
    `Assoc [("app_bin_dir", `String "/bin"); ("unknown_field", `Int 42)]
  in
  match Octez_manager_lib.Settings.For_tests.of_yojson json with
  | Ok t -> check (option string) "bin dir" (Some "/bin") t.app_bin_dir
  | Error (`Msg m) -> fail m

let test_invalid_history_mode () =
  let json = `Assoc [("default_history_mode", `String "invalid_mode")] in
  match Octez_manager_lib.Settings.For_tests.of_yojson json with
  | Ok t ->
      (* Invalid history mode should result in None *)
      check
        (option string)
        "invalid -> None"
        None
        (Option.map
           Octez_manager_lib.History_mode.to_string
           t.default_history_mode)
  | Error (`Msg m) -> fail m

let test_to_yojson_structure () =
  let t : Octez_manager_lib.Settings.t =
    {
      app_bin_dir = Some "/usr/bin";
      default_history_mode = None;
      default_logging_mode = None;
    }
  in
  let json = Octez_manager_lib.Settings.For_tests.to_yojson t in
  match json with
  | `Assoc pairs ->
      check bool "has app_bin_dir" true (List.mem_assoc "app_bin_dir" pairs) ;
      check
        bool
        "has history"
        true
        (List.mem_assoc "default_history_mode" pairs) ;
      check
        bool
        "has logging"
        true
        (List.mem_assoc "default_logging_mode" pairs)
  | _ -> fail "expected Assoc"

(* ── Suite ───────────────────────────────────────────────────── *)

let () =
  run
    "Settings"
    [
      ( "roundtrip",
        [
          test_case "default" `Quick test_default_roundtrip;
          test_case "with bin dir" `Quick test_with_bin_dir;
          test_case "with history mode" `Quick test_with_history_mode;
          test_case "with logging mode" `Quick test_with_logging_mode;
        ] );
      ( "deserialization",
        [
          test_case "empty object" `Quick test_empty_json_object;
          test_case "null fields" `Quick test_null_fields;
          test_case "extra fields" `Quick test_extra_fields_ignored;
          test_case "invalid history" `Quick test_invalid_history_mode;
        ] );
      ("serialization", [test_case "structure" `Quick test_to_yojson_structure]);
    ]
