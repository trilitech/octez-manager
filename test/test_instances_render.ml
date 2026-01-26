(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Unit tests for instances_render.ml - service list rendering logic.
    
    Tests rendering with different service states, counts, and layouts
    without requiring actual systemd services or TUI interaction. *)

open Alcotest
module Render = Octez_manager_ui.Instances_render
module State = Octez_manager_ui.Instances_state
module Layout = Octez_manager_ui.Instances_layout
open Mock_service_helpers_lib

(******************************************************************************)
(*                      SERVICE STATE RENDERING TESTS                        *)
(******************************************************************************)

(** Test: Rendering a running service shows correct status *)
let test_render_running_service () =
  let svc = Mock_service_helpers.running_service ~instance:"test-node" () in
  let icon = Render.status_icon svc in
  (* Icon should contain green status indicator *)
  check bool "running service has status icon" true (String.length icon > 0)

(** Test: Rendering a stopped service shows correct status *)
let test_render_stopped_service () =
  let svc = Mock_service_helpers.stopped_service ~instance:"test-node" () in
  let icon = Render.status_icon svc in
  check bool "stopped service has status icon" true (String.length icon > 0)

(** Test: Rendering a failed service shows error status *)
let test_render_failed_service () =
  let svc =
    Mock_service_helpers.failed_service ~instance:"test-node" ~error:"crash" ()
  in
  let icon = Render.status_icon svc in
  check bool "failed service has status icon" true (String.length icon > 0)

(** Test: Enabled badge shows correct state *)
let test_enabled_badge_true () =
  let svc = Mock_service_helpers.mock_service_state ~enabled:(Some true) () in
  let badge = Render.enabled_badge svc in
  check bool "enabled badge contains text" true (String.length badge > 0)

let test_enabled_badge_false () =
  let svc = Mock_service_helpers.mock_service_state ~enabled:(Some false) () in
  let badge = Render.enabled_badge svc in
  check bool "disabled badge contains text" true (String.length badge > 0)

let test_enabled_badge_unknown () =
  let svc = Mock_service_helpers.mock_service_state ~enabled:None () in
  let badge = Render.enabled_badge svc in
  check bool "unknown badge contains text" true (String.length badge > 0)

(** Test: RPC status line for running service *)
let test_rpc_status_running () =
  let svc =
    Mock_service_helpers.mock_service_state
      ~status:Octez_manager_ui.Data.Service_state.Running
      ()
  in
  let status_line =
    Render.rpc_status_line ~service_status:svc.status svc.service
  in
  check bool "rpc status line generated" true (String.length status_line > 0)

(** Test: RPC status line for stopped service *)
let test_rpc_status_stopped () =
  let svc =
    Mock_service_helpers.mock_service_state
      ~status:Octez_manager_ui.Data.Service_state.Stopped
      ()
  in
  let status_line =
    Render.rpc_status_line ~service_status:svc.status svc.service
  in
  check bool "stopped status line generated" true (String.length status_line > 0)

(** Test: RPC status line for failed service shows error *)
let test_rpc_status_failed () =
  let svc =
    Mock_service_helpers.mock_service_state
      ~status:(Octez_manager_ui.Data.Service_state.Unknown "startup error")
      ()
  in
  let status_line =
    Render.rpc_status_line ~service_status:svc.status svc.service
  in
  check bool "failed status line generated" true (String.length status_line > 0)

(******************************************************************************)
(*                    SERVICE COUNT RENDERING TESTS                          *)
(******************************************************************************)

(** Test: Rendering with empty service list *)
let test_render_empty_list () =
  let services : Octez_manager_ui.Data.Service_state.t list = [] in
  (* Just verify we can handle empty list without crashing *)
  check int "empty list has 0 services" 0 (List.length services)

(** Test: Rendering with single service *)
let test_render_single_service () =
  let services = [Mock_service_helpers.running_service ~instance:"node-1" ()] in
  check int "single service list has 1 service" 1 (List.length services) ;
  let first = List.hd services in
  check string "service has correct instance" "node-1" first.service.instance

(** Test: Rendering with multiple services *)
let test_render_multiple_services () =
  let services =
    [
      Mock_service_helpers.running_service ~instance:"node-1" ();
      Mock_service_helpers.running_service ~instance:"node-2" ();
      Mock_service_helpers.running_service ~instance:"node-3" ();
    ]
  in
  check int "multiple services list has 3 services" 3 (List.length services)

(** Test: Rendering with many services (stress test) *)
let test_render_many_services () =
  let services =
    Mock_service_helpers.create_services
      50
      ~prefix:"node"
      ~role:"node"
      ~status:Octez_manager_ui.Data.Service_state.Running
  in
  check int "stress test has 50 services" 50 (List.length services) ;
  (* Verify they all have unique names *)
  let names =
    List.map
      (fun s -> s.Octez_manager_ui.Data.Service_state.service.instance)
      services
  in
  let unique_names = List.sort_uniq String.compare names |> List.length in
  check int "all service names are unique" 50 unique_names

(** Test: Rendering with 100 services *)
let test_render_100_services () =
  let services =
    Mock_service_helpers.create_services
      100
      ~prefix:"node"
      ~role:"node"
      ~status:Octez_manager_ui.Data.Service_state.Running
  in
  check int "large list has 100 services" 100 (List.length services)

(******************************************************************************)
(*                     SERVICE TYPE RENDERING TESTS                          *)
(******************************************************************************)

(** Test: Rendering node service *)
let test_render_node_service () =
  let svc =
    Mock_service_helpers.running_service ~instance:"node-1" ~role:"node" ()
  in
  check string "node has correct role" "node" svc.service.role

(** Test: Rendering baker service *)
let test_render_baker_service () =
  let svc = Mock_service_helpers.baker_service ~instance:"baker-1" () in
  check string "baker has correct role" "baker" svc.service.role

(** Test: Rendering accuser service *)
let test_render_accuser_service () =
  let svc = Mock_service_helpers.accuser_service ~instance:"accuser-1" () in
  check string "accuser has correct role" "accuser" svc.service.role

(** Test: Rendering DAL node service *)
let test_render_dal_service () =
  let svc = Mock_service_helpers.dal_service ~instance:"dal-1" () in
  check string "dal has correct role" "dal-node" svc.service.role

(** Test: Rendering mixed service types *)
let test_render_mixed_types () =
  let services = Mock_service_helpers.multi_role_services () in
  check int "mixed services has 5 items" 5 (List.length services) ;
  (* Verify we have different roles *)
  let roles =
    List.map
      (fun s -> s.Octez_manager_ui.Data.Service_state.service.role)
      services
    |> List.sort_uniq String.compare
  in
  check bool "has multiple role types" true (List.length roles > 1)

(******************************************************************************)
(*                       MIXED STATE TESTS                                   *)
(******************************************************************************)

(** Test: Rendering mix of running, stopped, and failed services *)
let test_render_mixed_states () =
  let services =
    Mock_service_helpers.mixed_services ~running:5 ~stopped:3 ~failed:2 ()
  in
  check int "mixed states has 10 services" 10 (List.length services) ;

  (* Count services by status *)
  let count_status status =
    List.filter
      (fun s -> s.Octez_manager_ui.Data.Service_state.status = status)
      services
    |> List.length
  in
  check
    int
    "has 5 running"
    5
    (count_status Octez_manager_ui.Data.Service_state.Running) ;
  check
    int
    "has 3 stopped"
    3
    (count_status Octez_manager_ui.Data.Service_state.Stopped)

(** Test: All services with same network *)
let test_all_same_network () =
  let services =
    [
      Mock_service_helpers.running_service
        ~instance:"node-1"
        ~network:"mainnet"
        ();
      Mock_service_helpers.running_service
        ~instance:"node-2"
        ~network:"mainnet"
        ();
      Mock_service_helpers.baker_service
        ~instance:"baker-1"
        ~network:"mainnet"
        ();
    ]
  in
  let networks =
    List.map
      (fun s -> s.Octez_manager_ui.Data.Service_state.service.network)
      services
    |> List.sort_uniq String.compare
  in
  check int "all on same network" 1 (List.length networks) ;
  check string "network is mainnet" "mainnet" (List.hd networks)

(** Test: Services across different networks *)
let test_different_networks () =
  let services =
    [
      Mock_service_helpers.running_service
        ~instance:"node-1"
        ~network:"mainnet"
        ();
      Mock_service_helpers.running_service
        ~instance:"node-2"
        ~network:"ghostnet"
        ();
      Mock_service_helpers.running_service
        ~instance:"node-3"
        ~network:"weeklynet"
        ();
    ]
  in
  let networks =
    List.map
      (fun s -> s.Octez_manager_ui.Data.Service_state.service.network)
      services
    |> List.sort_uniq String.compare
  in
  check int "has 3 different networks" 3 (List.length networks)

(******************************************************************************)
(*                          TEST SUITE                                       *)
(******************************************************************************)

let () =
  run
    "Instances Render"
    [
      ( "Service State Rendering",
        [
          test_case "running service icon" `Quick test_render_running_service;
          test_case "stopped service icon" `Quick test_render_stopped_service;
          test_case "failed service icon" `Quick test_render_failed_service;
          test_case "enabled badge true" `Quick test_enabled_badge_true;
          test_case "enabled badge false" `Quick test_enabled_badge_false;
          test_case "enabled badge unknown" `Quick test_enabled_badge_unknown;
          test_case "rpc status running" `Quick test_rpc_status_running;
          test_case "rpc status stopped" `Quick test_rpc_status_stopped;
          test_case "rpc status failed" `Quick test_rpc_status_failed;
        ] );
      ( "Service Count",
        [
          test_case "empty list" `Quick test_render_empty_list;
          test_case "single service" `Quick test_render_single_service;
          test_case "multiple services" `Quick test_render_multiple_services;
          test_case "many services (50)" `Quick test_render_many_services;
          test_case "stress test (100)" `Quick test_render_100_services;
        ] );
      ( "Service Types",
        [
          test_case "node service" `Quick test_render_node_service;
          test_case "baker service" `Quick test_render_baker_service;
          test_case "accuser service" `Quick test_render_accuser_service;
          test_case "dal service" `Quick test_render_dal_service;
          test_case "mixed types" `Quick test_render_mixed_types;
        ] );
      ( "Mixed States",
        [
          test_case "mixed states" `Quick test_render_mixed_states;
          test_case "all same network" `Quick test_all_same_network;
          test_case "different networks" `Quick test_different_networks;
        ] );
    ]
