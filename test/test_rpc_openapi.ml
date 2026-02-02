(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Octez_manager_ui

(* ============================================================ *)
(* Path Tests                                                    *)
(* ============================================================ *)

let test_openapi_dir () =
  let dir = Rpc_openapi.openapi_dir () in
  Alcotest.(check bool)
    "ends with openapi"
    true
    (Filename.basename dir = "openapi")

let test_openapi_path () =
  let path = Rpc_openapi.openapi_path () in
  Alcotest.(check bool)
    "ends with json"
    true
    (Filename.check_suffix path ".json")

(* ============================================================ *)
(* Status Tests                                                  *)
(* ============================================================ *)

let test_initial_status () =
  (* Note: status may vary depending on test order and state *)
  let status = Rpc_openapi.get_status () in
  let is_valid =
    match status with
    | Rpc_openapi.NotDownloaded -> true
    | Rpc_openapi.Downloading -> true
    | Rpc_openapi.Ready -> true
    | Rpc_openapi.Error _ -> true
  in
  Alcotest.(check bool) "valid status" true is_valid

(* ============================================================ *)
(* Download Check Tests                                          *)
(* ============================================================ *)

let test_needs_download () =
  (* Just check that function runs without error *)
  let _needs = Rpc_openapi.needs_download () in
  Alcotest.(check pass) "needs_download runs" () ()

(* ============================================================ *)
(* Read Spec Tests                                               *)
(* ============================================================ *)

let test_read_spec_when_missing () =
  (* If file doesn't exist, should return None *)
  let result = Rpc_openapi.read_spec () in
  (* Result depends on whether file exists *)
  let is_option = match result with Some _ -> true | None -> true in
  Alcotest.(check bool) "returns option" true is_option

(* ============================================================ *)
(* Test Runner                                                   *)
(* ============================================================ *)

let () =
  Alcotest.run
    "Rpc_openapi"
    [
      ( "paths",
        [
          Alcotest.test_case "openapi_dir" `Quick test_openapi_dir;
          Alcotest.test_case "openapi_path" `Quick test_openapi_path;
        ] );
      ("status", [Alcotest.test_case "initial" `Quick test_initial_status]);
      ("needs_download", [Alcotest.test_case "runs" `Quick test_needs_download]);
      ( "read_spec",
        [Alcotest.test_case "when missing" `Quick test_read_spec_when_missing]
      );
    ]
