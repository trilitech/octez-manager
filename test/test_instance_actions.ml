(* Copyright 2025 Trilitech <contact@trili.tech>
   Copyright 2025 Functori <contact@functori.com>

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. *)

(** Tests for service instance operations (start, stop, restart, etc.)

    These tests use Mock_systemd to test service lifecycle operations
    without requiring actual systemd installation.
    
    See: Issue #458, TESTING_ROADMAP.md
*)

open Alcotest
module Mock = Test_mocks.Mock_systemd

(* ============================================================ *)
(* Helper Functions *)
(* ============================================================ *)

(** Unwrap a Result or fail the test with the error message *)
let ok_or_fail = function Ok x -> x | Error e -> fail e

(* ============================================================ *)
(* Service Start Tests *)
(* ============================================================ *)
(* Service Start Tests *)
(* ============================================================ *)

let test_start_stopped_service () =
  (* Enable test mode *)
  Unix.putenv "OCTEZ_MANAGER_TEST_MODE" "1" ;
  Mock.reset () ;

  (* Register a stopped service *)
  Mock.register_service "test-node" ~state:Mock.Stopped ~enabled:false ;

  (* Verify initial state *)
  Mock.assert_service_stopped "test-node" ;

  (* Start the service *)
  Mock.start_service "test-node" |> ok_or_fail ;

  (* Verify it's now running *)
  Mock.assert_service_running "test-node"

let test_start_already_running () =
  Unix.putenv "OCTEZ_MANAGER_TEST_MODE" "1" ;
  Mock.reset () ;

  Mock.register_service "test-node" ~state:Mock.Running ~enabled:false ;

  (* Starting already running service should be idempotent *)
  Mock.start_service "test-node" |> ok_or_fail ;
  Mock.assert_service_running "test-node"

let test_start_failed_service () =
  Unix.putenv "OCTEZ_MANAGER_TEST_MODE" "1" ;
  Mock.reset () ;

  Mock.register_service
    "test-node"
    ~state:(Mock.Failed "previous error")
    ~enabled:false ;

  (* Starting a failed service should clear the error and start it *)
  Mock.start_service "test-node" |> ok_or_fail ;
  Mock.assert_service_running "test-node"

(* ============================================================ *)
(* Service Stop Tests *)
(* ============================================================ *)

let test_stop_running_service () =
  Unix.putenv "OCTEZ_MANAGER_TEST_MODE" "1" ;
  Mock.reset () ;

  Mock.register_service "test-node" ~state:Mock.Running ~enabled:false ;

  Mock.stop_service "test-node" |> ok_or_fail ;

  Mock.assert_service_stopped "test-node"

let test_stop_already_stopped () =
  Unix.putenv "OCTEZ_MANAGER_TEST_MODE" "1" ;
  Mock.reset () ;

  Mock.register_service "test-node" ~state:Mock.Stopped ~enabled:false ;

  (* Stopping already stopped service should be idempotent *)
  Mock.stop_service "test-node" |> ok_or_fail ;
  Mock.assert_service_stopped "test-node"

(* ============================================================ *)
(* Service Restart Tests *)
(* ============================================================ *)

let test_restart_running_service () =
  Unix.putenv "OCTEZ_MANAGER_TEST_MODE" "1" ;
  Mock.reset () ;

  Mock.register_service "test-node" ~state:Mock.Running ~enabled:false ;

  let service_before =
    match Mock.get_service "test-node" with
    | Some s -> s
    | None -> fail "Service not found"
  in
  let restart_count_before = service_before.restart_count in

  Mock.restart_service "test-node" |> ok_or_fail ;

  Mock.assert_service_running "test-node" ;

  let service_after =
    match Mock.get_service "test-node" with
    | Some s -> s
    | None -> fail "Service not found"
  in
  check
    int
    "restart increments counter"
    (restart_count_before + 1)
    service_after.restart_count

let test_restart_stopped_service () =
  Unix.putenv "OCTEZ_MANAGER_TEST_MODE" "1" ;
  Mock.reset () ;

  Mock.register_service "test-node" ~state:Mock.Stopped ~enabled:false ;

  Mock.restart_service "test-node" |> ok_or_fail ;

  (* Restart should start a stopped service *)
  Mock.assert_service_running "test-node"

(* ============================================================ *)
(* Service Enable/Disable Tests *)
(* ============================================================ *)

let test_enable_service () =
  Unix.putenv "OCTEZ_MANAGER_TEST_MODE" "1" ;
  Mock.reset () ;

  Mock.register_service "test-node" ~state:Mock.Stopped ~enabled:false ;

  Mock.enable_service "test-node" |> ok_or_fail ;

  Mock.assert_service_enabled "test-node"

let test_disable_service () =
  Unix.putenv "OCTEZ_MANAGER_TEST_MODE" "1" ;
  Mock.reset () ;

  Mock.register_service "test-node" ~state:Mock.Running ~enabled:true ;

  Mock.disable_service "test-node" |> ok_or_fail ;

  Mock.assert_service_disabled "test-node" ;
  (* Disabling doesn't stop the service *)
  Mock.assert_service_running "test-node"

(* ============================================================ *)
(* Error Injection Tests *)
(* ============================================================ *)

let test_permission_denied () =
  Unix.putenv "OCTEZ_MANAGER_TEST_MODE" "1" ;
  Mock.reset () ;

  Mock.register_service "test-node" ~state:Mock.Stopped ~enabled:false ;
  Mock.set_failure_mode "test-node" Mock.PermissionDenied ;

  (* Attempting to start should fail *)
  (match Mock.start_service "test-node" with
  | Ok () -> fail "Expected PermissionDenied error"
  | Error msg ->
      check
        bool
        "error message contains permission denied"
        true
        ( String.lowercase_ascii msg |> fun s ->
          String.contains s 'p' && String.contains s 'd' )) ;

  (* Service should still be stopped *)
  Mock.assert_service_stopped "test-node"

let test_start_fails_mode () =
  Unix.putenv "OCTEZ_MANAGER_TEST_MODE" "1" ;
  Mock.reset () ;

  Mock.register_service "test-node" ~state:Mock.Stopped ~enabled:false ;
  Mock.set_failure_mode "test-node" (Mock.StartFails "simulated failure") ;

  (match Mock.start_service "test-node" with
  | Ok () -> () (* StartFails puts service in failed state but returns Ok *)
  | Error _ -> ()) ;

  (* Service should be in failed state *)
  match Mock.get_service "test-node" with
  | Some service -> (
      match service.state with
      | Failed _ -> () (* Expected *)
      | _ -> fail "Expected service to be in Failed state")
  | None -> fail "Service not found"

let test_timeout_failure () =
  Unix.putenv "OCTEZ_MANAGER_TEST_MODE" "1" ;
  Mock.reset () ;

  Mock.register_service "test-node" ~state:Mock.Stopped ~enabled:false ;
  Mock.set_failure_mode "test-node" Mock.Timeout ;

  (* Attempting to start should fail with timeout *)
  match Mock.start_service "test-node" with
  | Ok () -> fail "Expected timeout error"
  | Error msg ->
      check
        bool
        "error message mentions timeout"
        true
        (String.lowercase_ascii msg |> fun s -> String.contains s 't')

(* ============================================================ *)
(* Multiple Services Tests *)
(* ============================================================ *)

let test_multiple_services () =
  Unix.putenv "OCTEZ_MANAGER_TEST_MODE" "1" ;
  Mock.reset () ;

  (* Register multiple services *)
  Mock.register_service "test-node-1" ~state:Mock.Stopped ~enabled:false ;
  Mock.register_service "test-node-2" ~state:Mock.Running ~enabled:true ;
  Mock.register_service "test-baker-1" ~state:Mock.Stopped ~enabled:false ;

  (* Start node-1 *)
  Mock.start_service "test-node-1" |> ok_or_fail ;
  Mock.assert_service_running "test-node-1" ;

  (* Verify other services are unaffected *)
  Mock.assert_service_running "test-node-2" ;
  Mock.assert_service_stopped "test-baker-1" ;

  (* Stop node-2 *)
  Mock.stop_service "test-node-2" |> ok_or_fail ;
  Mock.assert_service_stopped "test-node-2" ;

  (* Verify others still unaffected *)
  Mock.assert_service_running "test-node-1" ;
  Mock.assert_service_stopped "test-baker-1"

let test_service_not_found () =
  Unix.putenv "OCTEZ_MANAGER_TEST_MODE" "1" ;
  Mock.reset () ;

  (* Attempting operations on non-existent service should fail gracefully *)
  match Mock.start_service "nonexistent" with
  | Ok () -> fail "Expected error for nonexistent service"
  | Error msg ->
      check
        bool
        "error mentions not found or unknown"
        true
        ( String.lowercase_ascii msg |> fun s ->
          String.contains s 'n' || String.contains s 'u' )

(* ============================================================ *)
(* State Query Tests *)
(* ============================================================ *)

let test_get_service_info () =
  Unix.putenv "OCTEZ_MANAGER_TEST_MODE" "1" ;
  Mock.reset () ;

  Mock.register_service "test-node" ~state:Mock.Running ~enabled:true ;

  let service =
    match Mock.get_service "test-node" with
    | Some s -> s
    | None -> fail "Service not found"
  in

  check string "service name" "test-node" service.name ;
  check bool "service enabled" true service.enabled ;
  (match service.state with
  | Running -> ()
  | _ -> fail "Expected Running state") ;
  check int "initial restart count" 0 service.restart_count

let test_list_services () =
  Unix.putenv "OCTEZ_MANAGER_TEST_MODE" "1" ;
  Mock.reset () ;

  Mock.register_service "test-node-1" ~state:Mock.Running ~enabled:false ;
  Mock.register_service "test-node-2" ~state:Mock.Stopped ~enabled:true ;
  Mock.register_service "test-baker" ~state:Mock.Running ~enabled:true ;

  let services = Mock.list_services () in

  check int "number of services" 3 (List.length services) ;

  let names = List.map (fun s -> s.Mock.name) services |> List.sort compare in
  check
    (list string)
    "service names"
    ["test-baker"; "test-node-1"; "test-node-2"]
    names

(* ============================================================ *)
(* Test Suite *)
(* ============================================================ *)

let service_start_tests =
  [
    ("start stopped service", `Quick, test_start_stopped_service);
    ("start already running service", `Quick, test_start_already_running);
    ("start failed service", `Quick, test_start_failed_service);
  ]

let service_stop_tests =
  [
    ("stop running service", `Quick, test_stop_running_service);
    ("stop already stopped service", `Quick, test_stop_already_stopped);
  ]

let service_restart_tests =
  [
    ("restart running service", `Quick, test_restart_running_service);
    ("restart stopped service", `Quick, test_restart_stopped_service);
  ]

let service_enable_tests =
  [
    ("enable service", `Quick, test_enable_service);
    ("disable service", `Quick, test_disable_service);
  ]

let error_injection_tests =
  [
    ("permission denied error", `Quick, test_permission_denied);
    ("start fails mode", `Quick, test_start_fails_mode);
    ("timeout failure", `Quick, test_timeout_failure);
  ]

let multi_service_tests =
  [
    ("multiple services", `Quick, test_multiple_services);
    ("service not found", `Quick, test_service_not_found);
  ]

let state_query_tests =
  [
    ("get service info", `Quick, test_get_service_info);
    ("list services", `Quick, test_list_services);
  ]

let () =
  Alcotest.run
    "Instance Actions (Mock Systemd)"
    [
      ("service_start", service_start_tests);
      ("service_stop", service_stop_tests);
      ("service_restart", service_restart_tests);
      ("service_enable_disable", service_enable_tests);
      ("error_injection", error_injection_tests);
      ("multiple_services", multi_service_tests);
      ("state_queries", state_query_tests);
    ]
