(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Pure unit tests for instances page navigation (move_selection).

    Tests the multi-column and single-column navigation logic,
    verifying that moving up from managed services goes to the
    correct menu item (Browse RPCs, not Install). *)

open Alcotest
open Octez_manager_ui
open Mock_service_helpers_lib
open Mock_service_helpers
module StringSet = Instances_state.StringSet

let menu_item_count = Instances_state.menu_item_count

let services_start_idx = Instances_state.services_start_idx

(** Helper to create a state with the given services and column count *)
let make_state ?(selected = 0) ?(num_columns = 1) ?(active_column = 0)
    ?(external_services = []) services =
  let column_scroll = Array.make (max 1 num_columns) 0 in
  {
    Instances_state.services;
    external_services;
    selected;
    folded = StringSet.empty;
    external_folded = StringSet.empty;
    last_updated = 0.0;
    num_columns;
    active_column;
    column_scroll;
    view_mode = Instances_state.By_role;
    groups = [];
  }

let move = Instances.For_tests.move_selection

(* ============================================================ *)
(* Single-column navigation tests                               *)
(* ============================================================ *)

let test_single_column_up_from_first_service () =
  (* In single column, moving up from the first service (index 4)
     should skip the separator (index 3) and land on Browse RPCs (index 2) *)
  let services = [running_service ~instance:"node-1" ()] in
  let s = make_state ~selected:services_start_idx ~num_columns:1 services in
  let s' = move s (-1) in
  check int "lands on Browse RPCs" (menu_item_count - 1) s'.selected

let test_single_column_down_from_browse_rpcs () =
  (* Moving down from Browse RPCs (index 2) should skip separator and
     land on first service (index 4) *)
  let services = [running_service ~instance:"node-1" ()] in
  let s = make_state ~selected:(menu_item_count - 1) ~num_columns:1 services in
  let s' = move s 1 in
  check int "lands on first service" services_start_idx s'.selected

let test_single_column_navigate_through_menu () =
  let services = [running_service ~instance:"node-1" ()] in
  let s = make_state ~selected:0 ~num_columns:1 services in
  (* Down from Install -> Manage binaries *)
  let s' = move s 1 in
  check int "Install -> Manage binaries" 1 s'.selected ;
  (* Down from Manage binaries -> Browse RPCs *)
  let s' = move s' 1 in
  check int "Manage binaries -> Browse RPCs" 2 s'.selected ;
  (* Down from Browse RPCs -> first service (skip separator) *)
  let s' = move s' 1 in
  check int "Browse RPCs -> first service" services_start_idx s'.selected

(* ============================================================ *)
(* Multi-column navigation tests                                *)
(* ============================================================ *)

let test_multi_column_up_from_first_service_to_browse_rpcs () =
  (* BUG FIX: In multi-column mode, moving up from the first service
     in a column should go to Browse RPCs (last menu item),
     not Install new instance (first menu item) *)
  let services = multi_role_services () in
  let s =
    make_state
      ~selected:services_start_idx
      ~num_columns:2
      ~active_column:0
      services
  in
  let s' = move s (-1) in
  check
    int
    "lands on Browse RPCs (last menu item)"
    (menu_item_count - 1)
    s'.selected

let test_multi_column_up_from_second_column_to_browse_rpcs () =
  (* Same fix applies when navigating up from column 1 *)
  let services = multi_role_services () in
  (* Find the first service index in column 1 *)
  let sections = Instances_layout.group_by_role services in
  let col1_services =
    Instances_layout.services_in_column ~num_columns:2 ~sections ~services 1
  in
  match col1_services with
  | [] -> (* Column 1 is empty, skip test *) ()
  | first_idx :: _ ->
      let s =
        make_state
          ~selected:(first_idx + services_start_idx)
          ~num_columns:2
          ~active_column:1
          services
      in
      let s' = move s (-1) in
      check
        int
        "from column 1, lands on Browse RPCs"
        (menu_item_count - 1)
        s'.selected

let test_multi_column_down_from_menu_to_service () =
  (* Down from last menu item should go to first service in column 0 *)
  let services = multi_role_services () in
  let s =
    make_state
      ~selected:(menu_item_count - 1)
      ~num_columns:2
      ~active_column:0
      services
  in
  let s' = move s 1 in
  (* Should land on a service (index >= services_start_idx) *)
  check bool "lands on a service" true (s'.selected >= services_start_idx) ;
  check int "active_column is 0" 0 s'.active_column

let test_multi_column_menu_navigation () =
  (* Within the menu area, up/down should work linearly *)
  let services = multi_role_services () in
  let s = make_state ~selected:0 ~num_columns:2 services in
  (* Down through menu *)
  let s' = move s 1 in
  check int "0 -> 1" 1 s'.selected ;
  let s' = move s' 1 in
  check int "1 -> 2" 2 s'.selected ;
  (* Up through menu *)
  let s' = move s' (-1) in
  check int "2 -> 1" 1 s'.selected ;
  let s' = move s' (-1) in
  check int "1 -> 0" 0 s'.selected

let test_multi_column_up_does_not_overshoot_menu () =
  (* Moving up from menu item 0 should stay at 0 *)
  let services = multi_role_services () in
  let s = make_state ~selected:0 ~num_columns:2 services in
  let s' = move s (-1) in
  check int "stays at 0" 0 s'.selected

let test_multi_column_roundtrip () =
  (* Down from Browse RPCs to first service, then back up should return
     to Browse RPCs *)
  let services = multi_role_services () in
  let s =
    make_state
      ~selected:(menu_item_count - 1)
      ~num_columns:2
      ~active_column:0
      services
  in
  (* Down to first service *)
  let s' = move s 1 in
  check bool "now on a service" true (s'.selected >= services_start_idx) ;
  (* Back up to menu *)
  let s' = move s' (-1) in
  check int "roundtrip returns to Browse RPCs" (menu_item_count - 1) s'.selected

(* ============================================================ *)
(* Empty state tests                                            *)
(* ============================================================ *)

let test_empty_state_stays_at_install () =
  let s = make_state ~selected:0 ~num_columns:1 [] in
  let s' = move s 1 in
  check int "stays at Install" 0 s'.selected ;
  let s' = move s (-1) in
  check int "stays at Install (up)" 0 s'.selected

let test_empty_state_multi_column () =
  let s = make_state ~selected:0 ~num_columns:2 [] in
  let s' = move s 1 in
  check int "stays at Install" 0 s'.selected

(* ============================================================ *)
(* Test Suite                                                   *)
(* ============================================================ *)

let () =
  Alcotest.run
    "Instances Navigation"
    [
      ( "single-column",
        [
          ( "up from first service -> Browse RPCs",
            `Quick,
            test_single_column_up_from_first_service );
          ( "down from Browse RPCs -> first service",
            `Quick,
            test_single_column_down_from_browse_rpcs );
          ( "navigate through menu items",
            `Quick,
            test_single_column_navigate_through_menu );
        ] );
      ( "multi-column",
        [
          ( "up from first service -> Browse RPCs (bug fix)",
            `Quick,
            test_multi_column_up_from_first_service_to_browse_rpcs );
          ( "up from column 1 -> Browse RPCs",
            `Quick,
            test_multi_column_up_from_second_column_to_browse_rpcs );
          ( "down from menu -> first service",
            `Quick,
            test_multi_column_down_from_menu_to_service );
          ( "menu navigation is linear",
            `Quick,
            test_multi_column_menu_navigation );
          ( "up does not overshoot menu",
            `Quick,
            test_multi_column_up_does_not_overshoot_menu );
          ( "roundtrip Browse RPCs <-> service",
            `Quick,
            test_multi_column_roundtrip );
        ] );
      ( "empty-state",
        [
          ("stays at Install", `Quick, test_empty_state_stays_at_install);
          ( "multi-column stays at Install",
            `Quick,
            test_empty_state_multi_column );
        ] );
    ]
