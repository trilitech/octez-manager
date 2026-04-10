(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Pure unit tests for instances page navigation (move_selection).

    Tests the multi-column and single-column navigation logic.

    Layout: 0   = radio row (navigable, view mode toggle),
            1   = separator (skipped automatically),
            2+  = services.

    The radio row at index [menu_item_count] (=0) IS navigable.
    Only the separator at index [menu_item_count+1] (=1) is skipped. *)

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
    external_section_folded = false;
    last_updated = 0.0;
    num_columns;
    active_column;
    column_scroll;
    view_mode = Instances_state.By_role;
    groups = [];
    create_menu_open = false;
    create_menu_cursor = 0;
  }

let move = Instances.For_tests.move_selection

(* ============================================================ *)
(* Single-column navigation tests                               *)
(* ============================================================ *)

let test_single_column_up_from_first_service () =
  (* In single column, moving up from the first service (services_start_idx=5)
     should skip the non-navigable zone [radio-row(3), separator(4)] and land
     on Browse RPCs (menu_item_count-1=2). *)
  let services = [running_service ~instance:"node-1" ()] in
  let s = make_state ~selected:services_start_idx ~num_columns:1 services in
  let s' = move s (-1) in
  check int "lands on radio row" menu_item_count s'.selected

let test_single_column_down_from_browse_rpcs () =
  (* Moving down from Browse RPCs (menu_item_count-1=2) should skip the
     non-navigable zone [radio-row(3), separator(4)] and land on the
     first service (services_start_idx=5). *)
  let services = [running_service ~instance:"node-1" ()] in
  let s = make_state ~selected:menu_item_count ~num_columns:1 services in
  let s' = move s 1 in
  check int "radio row -> first service" services_start_idx s'.selected

let test_single_column_navigate_through_menu () =
  let services = [running_service ~instance:"node-1" ()] in
  let s = make_state ~selected:0 ~num_columns:1 services in
  (* Starting at radio row (index 0), down skips separator -> first service *)
  let s' = move s 1 in
  check
    int
    "radio row -> first service (skip sep at 1)"
    services_start_idx
    s'.selected ;
  (* Back up from first service -> radio row *)
  let s'' = move s' (-1) in
  check int "first service -> radio row" menu_item_count s''.selected

(* ============================================================ *)
(* Multi-column navigation tests                                *)
(* ============================================================ *)

let test_multi_column_up_from_first_service_to_radio_row () =
  (* Moving up from the first service in a column should go back to radio row
     (transition between sections allowed). *)
  let services = multi_role_services () in
  let s =
    make_state
      ~selected:services_start_idx
      ~num_columns:2
      ~active_column:0
      services
  in
  let s' = move s (-1) in
  check int "goes to radio row" menu_item_count s'.selected

let test_multi_column_up_from_second_column_to_radio_row () =
  (* Same applies when navigating up from column 1 - goes to radio row *)
  let services = multi_role_services () in
  (* Find the first service index in column 1 *)
  let sections = Instances_layout.group_by_role services in
  (* Convert services to display_items for the layout functions *)
  let display_items =
    List.map (fun svc -> Instances_state.Real_service svc) services
  in
  let col1_services =
    Instances_layout.services_in_column
      ~num_columns:2
      ~sections
      ~display_items
      1
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
      check int "from column 1, goes to radio row" menu_item_count s'.selected

let test_multi_column_down_from_radio_row_to_service () =
  (* Down from radio row should go to first service in column 0 *)
  let services = multi_role_services () in
  let s =
    make_state
      ~selected:menu_item_count
      ~num_columns:2
      ~active_column:0
      services
  in
  let s' = move s 1 in
  check bool "goes to a service" true (s'.selected >= services_start_idx) ;
  check int "active_column is 0" 0 s'.active_column

let test_multi_column_menu_navigation () =
  (* Menu navigation: down from radio transitions to services, up stays at top *)
  let services = multi_role_services () in
  let s = make_state ~selected:menu_item_count ~num_columns:2 services in
  (* Down from radio row -> first service *)
  let s' = move s 1 in
  check bool "goes to service" true (s'.selected >= services_start_idx) ;
  (* Up from radio row stays at radio row (top of menu) *)
  let s_up = move s (-1) in
  check int "stays at radio row" menu_item_count s_up.selected

let test_multi_column_up_does_not_overshoot_menu () =
  (* Moving up from menu item 0 should stay at 0 *)
  let services = multi_role_services () in
  let s = make_state ~selected:0 ~num_columns:2 services in
  let s' = move s (-1) in
  check int "stays at 0" 0 s'.selected

let test_multi_column_roundtrip () =
  (* Can navigate between menu and services *)
  let services = multi_role_services () in
  let s =
    make_state
      ~selected:menu_item_count
      ~num_columns:2
      ~active_column:0
      services
  in
  (* Down to first service *)
  let s' = move s 1 in
  check bool "now on a service" true (s'.selected >= services_start_idx) ;
  (* Back up to radio row *)
  let s'' = move s' (-1) in
  check int "back to radio row" menu_item_count s''.selected

(* ============================================================ *)
(* Empty state tests                                            *)
(* ============================================================ *)

let test_empty_state_navigates_to_ghost () =
  (* When there are no services, ghost "Add new" entries should still be navigable *)
  let s = make_state ~selected:0 ~num_columns:1 [] in
  let s' = move s 1 in
  check
    int
    "radio row -> first ghost (single column)"
    services_start_idx
    s'.selected

let test_empty_state_multi_column_navigates_to_ghost () =
  (* Multi-column: should navigate from radio row to first ghost *)
  let s = make_state ~selected:0 ~num_columns:2 [] in
  let s' = move s 1 in
  check
    bool
    "radio row -> first ghost (multi column)"
    true
    (s'.selected >= services_start_idx)

(* ============================================================ *)
(* Test Suite                                                   *)
(* ============================================================ *)

let () =
  Alcotest.run
    "Instances Navigation"
    [
      ( "single-column",
        [
          ( "up from first service -> radio row",
            `Quick,
            test_single_column_up_from_first_service );
          ( "down from radio row -> first service (skip sep)",
            `Quick,
            test_single_column_down_from_browse_rpcs );
          ( "navigate through radio row and services",
            `Quick,
            test_single_column_navigate_through_menu );
        ] );
      ( "multi-column",
        [
          ( "up from first service -> radio row",
            `Quick,
            test_multi_column_up_from_first_service_to_radio_row );
          ( "up from column 1 -> radio row",
            `Quick,
            test_multi_column_up_from_second_column_to_radio_row );
          ( "down from radio row -> first service",
            `Quick,
            test_multi_column_down_from_radio_row_to_service );
          ( "menu navigation is linear through radio row",
            `Quick,
            test_multi_column_menu_navigation );
          ( "up does not overshoot menu",
            `Quick,
            test_multi_column_up_does_not_overshoot_menu );
          ( "roundtrip Browse RPCs <-> radio row <-> service",
            `Quick,
            test_multi_column_roundtrip );
        ] );
      ( "empty-state",
        [
          ( "navigates to ghost (single column)",
            `Quick,
            test_empty_state_navigates_to_ghost );
          ( "navigates to ghost (multi column)",
            `Quick,
            test_empty_state_multi_column_navigates_to_ghost );
        ] );
    ]
