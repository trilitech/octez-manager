(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Pure unit tests for instances page navigation (move_selection).

    Tests the multi-column and single-column navigation logic.

    Layout: Radio row is visible but NOT navigable (no focus state).
            Services start at index 0.
            View mode toggle is via 'g' keyboard shortcut only.

    After refactor: services_start_idx = 0, navigation skips radio row entirely. *)

open Alcotest
open Octez_manager_ui
open Mock_service_helpers_lib
open Mock_service_helpers
module StringSet = Instances_state.StringSet

let services_start_idx = Instances_state.services_start_idx

(** Helper to create a state with the given services and column count *)
let make_state ?(selected = 0) ?(num_columns = 1) ?(active_column = 0)
    ?(external_services = []) ?(external_section_folded = false) services =
  let column_scroll = Array.make (max 1 num_columns) 0 in
  {
    Instances_state.services;
    external_services;
    selected;
    folded = StringSet.empty;
    external_folded = StringSet.empty;
    external_section_folded;
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
  (* Moving up from first service (index 0) should clamp at 0 since there's
     nothing above it to navigate to. *)
  let services = [running_service ~instance:"node-1" ()] in
  let s = make_state ~selected:services_start_idx ~num_columns:1 services in
  let s' = move s (-1) in
  check int "stays at first service" services_start_idx s'.selected

let test_single_column_down_from_first_service () =
  (* Moving down from first service goes to second service *)
  let services =
    [
      running_service ~instance:"node-1" ();
      running_service ~instance:"node-2" ();
    ]
  in
  let s = make_state ~selected:services_start_idx ~num_columns:1 services in
  let s' = move s 1 in
  check
    int
    "first service -> second service"
    (services_start_idx + 1)
    s'.selected

let test_single_column_navigate_through_services () =
  let services = [running_service ~instance:"node-1" ()] in
  let s = make_state ~selected:0 ~num_columns:1 services in
  (* Starting at first service (index 0), down goes to first ghost *)
  let s' = move s 1 in
  check int "first service -> first ghost" (services_start_idx + 1) s'.selected ;
  (* Back up from ghost -> first service *)
  let s'' = move s' (-1) in
  check int "ghost -> first service" services_start_idx s''.selected

(* ============================================================ *)
(* Multi-column navigation tests                                *)
(* ============================================================ *)

let test_multi_column_up_from_first_service_stays_at_top () =
  (* Moving up from the first service in a column should stay at first service
     (no radio row to navigate to). *)
  let services = multi_role_services () in
  let s =
    make_state
      ~selected:services_start_idx
      ~num_columns:2
      ~active_column:0
      services
  in
  let s' = move s (-1) in
  check int "stays at first service" services_start_idx s'.selected

let test_multi_column_up_from_second_column_stays_at_top () =
  (* Moving up from first item in column 1 should stay at that item *)
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
      check
        int
        "from column 1, stays at top of column"
        (first_idx + services_start_idx)
        s'.selected

let test_multi_column_down_within_column () =
  (* Down from first service should go to next service in same column *)
  let services = multi_role_services () in
  let s =
    make_state
      ~selected:services_start_idx
      ~num_columns:2
      ~active_column:0
      services
  in
  let s' = move s 1 in
  check bool "moves down within services" true (s'.selected > services_start_idx)

let test_multi_column_navigation_starts_at_services () =
  (* Navigation always starts at services, no menu area *)
  let services = multi_role_services () in
  let s = make_state ~selected:0 ~num_columns:2 services in
  (* selected=0 is services_start_idx, which is the first service *)
  check int "initial selected is first service" services_start_idx s.selected ;
  (* Down goes to next service *)
  let s' = move s 1 in
  check bool "down moves within services" true (s'.selected > services_start_idx) ;
  (* Up from first service stays at first service *)
  let s_up = move s (-1) in
  check
    int
    "up from first service stays at top"
    services_start_idx
    s_up.selected

let test_multi_column_up_does_not_overshoot () =
  (* Moving up from first service (index 0) should stay at 0 *)
  let services = multi_role_services () in
  let s = make_state ~selected:0 ~num_columns:2 services in
  let s' = move s (-1) in
  check int "stays at 0 (first service)" services_start_idx s'.selected

let test_multi_column_up_down_navigation () =
  (* Navigate down then up within services *)
  let services = multi_role_services () in
  let s = make_state ~selected:0 ~num_columns:2 ~active_column:0 services in
  (* Down to next service *)
  let s' = move s 1 in
  let first_down = s'.selected in
  check bool "moved down" true (first_down > services_start_idx) ;
  (* Back up *)
  let s'' = move s' (-1) in
  check int "back to first service" services_start_idx s''.selected

(* ============================================================ *)
(* Empty state tests                                            *)
(* ============================================================ *)

let test_empty_state_navigates_to_ghost () =
  (* When there are no services, ghost "Add new" entries should still be navigable *)
  let s = make_state ~selected:0 ~num_columns:1 [] in
  let s' = move s 1 in
  check
    int
    "first ghost -> second ghost (single column)"
    (services_start_idx + 1)
    s'.selected

let test_empty_state_multi_column_navigates_to_ghost () =
  (* Multi-column: should navigate from first ghost down within column.
     In a 2-column layout with ghosts distributed, moving down in column 0
     goes to the next ghost in column 0. *)
  let s = make_state ~selected:0 ~num_columns:2 [] in
  let s' = move s 1 in
  (* The exact target depends on how ghosts are distributed in columns.
     Just verify we moved to a valid ghost index. *)
  check bool "moved to another ghost" true (s'.selected > services_start_idx)

let test_empty_state_ensure_valid_column_preserves_selection () =
  (* ensure_valid_column should NOT reset selection when navigating to ghosts *)
  let s = make_state ~selected:services_start_idx ~num_columns:3 [] in
  let s' = Instances_layout.ensure_valid_column s in
  check
    int
    "ensure_valid_column preserves ghost selection"
    services_start_idx
    s'.selected

let test_empty_state_wide_terminal_multi_column_navigation () =
  (* User scenario: very wide terminal (many columns), empty services, press Down from first ghost *)
  let s = make_state ~selected:0 ~num_columns:5 [] in
  let s' = move s 1 in
  check bool "moved to another ghost" true (s'.selected > services_start_idx) ;
  (* Now simulate refresh (which calls ensure_valid_column) *)
  let s'' = Instances_layout.ensure_valid_column s' in
  check
    bool
    "ensure_valid_column after nav preserves valid selection"
    true
    (s''.selected >= services_start_idx)

(* ============================================================ *)
(* External section navigation tests                            *)
(* ============================================================ *)

(** Minimal External_service.t for navigation tests *)
let make_ext_svc name : Octez_manager_lib.External_service.t =
  let module ES = Octez_manager_lib.External_service in
  let unknown () : _ ES.field =
    {value = None; confidence = ES.Unknown; source = "test"}
  in
  {
    config =
      {
        unit_name = name;
        unit_file_path = None;
        exec_start = "/usr/bin/octez-node run";
        unit_state =
          {active_state = "active"; sub_state = "running"; enabled = Some true};
        user = None;
        group = None;
        working_dir = None;
        environment_files = [];
        role = unknown ();
        binary_path = unknown ();
        binary_version = unknown ();
        data_dir = unknown ();
        rpc_addr = unknown ();
        net_addr = unknown ();
        network = unknown ();
        history_mode = unknown ();
        node_endpoint = unknown ();
        base_dir = unknown ();
        delegates = {value = Some []; confidence = ES.Unknown; source = "test"};
        dal_endpoint = unknown ();
        daily_logs_dir = None;
        extra_args = [];
        parse_warnings = [];
      };
    suggested_instance_name = name;
  }

let test_single_column_j_from_last_managed_reaches_external () =
  (* Single column: pressing down from the last managed service should
     transition into the external section when it is visible. *)
  let svc = running_service ~instance:"node-1" () in
  let ext = make_ext_svc "foo.service" in
  let s =
    make_state
      ~selected:services_start_idx
      ~num_columns:1
      ~external_services:[ext]
      ~external_section_folded:false
      [svc]
  in
  let s' = move s 1 in
  check
    int
    "single-col: j from last managed -> first external"
    (services_start_idx + 1)
    s'.selected

let test_multi_column_col0_j_from_last_managed_reaches_external () =
  (* Multi-column col 0: pressing down from last managed in col 0 should
     transition into the external section when it is visible. *)
  let services = multi_role_services () in
  let ext = make_ext_svc "foo.service" in
  (* Build state first so we can get the ghost-inclusive display_items *)
  let s_init =
    make_state
      ~selected:services_start_idx
      ~num_columns:2
      ~active_column:0
      ~external_services:[ext]
      ~external_section_folded:false
      services
  in
  let display_items = Instances_state.display_ordered_items s_init in
  let sections = Instances_layout.group_by_role services in
  let col0_indices =
    Instances_layout.services_in_column
      ~num_columns:2
      ~sections
      ~display_items
      0
  in
  let last_col0 = List.nth col0_indices (List.length col0_indices - 1) in
  let s =
    {s_init with Instances_state.selected = last_col0 + services_start_idx}
  in
  let s' = move s 1 in
  check
    int
    "multi-col col0: j from last managed -> first external"
    (services_start_idx + List.length display_items)
    s'.selected

let test_multi_column_col1_j_from_last_managed_reaches_external () =
  (* Multi-column col 1: pressing down from the bottom of col 1 should also
     reach the external section — it is rendered below ALL columns, so j from
     any column's bottom should navigate into it when visible. *)
  let services = multi_role_services () in
  let ext = make_ext_svc "foo.service" in
  (* Build state first so we can get the ghost-inclusive display_items *)
  let s_init =
    make_state
      ~selected:services_start_idx
      ~num_columns:2
      ~active_column:1
      ~external_services:[ext]
      ~external_section_folded:false
      services
  in
  let display_items = Instances_state.display_ordered_items s_init in
  let sections = Instances_layout.group_by_role services in
  let col1_indices =
    Instances_layout.services_in_column
      ~num_columns:2
      ~sections
      ~display_items
      1
  in
  match col1_indices with
  | [] -> (* Column 1 empty, test vacuously passes *) ()
  | _ ->
      let last_col1 = List.nth col1_indices (List.length col1_indices - 1) in
      let s =
        {s_init with Instances_state.selected = last_col1 + services_start_idx}
      in
      let s' = move s 1 in
      check
        int
        "multi-col col1: j from last managed reaches external"
        (services_start_idx + List.length display_items)
        s'.selected

let test_folded_external_no_transition () =
  (* When external section is folded, pressing down from the last item in
     col 0 should not transition into the hidden external section. *)
  let svc = running_service ~instance:"node-1" () in
  let ext = make_ext_svc "foo.service" in
  (* Build state first so we can get the ghost-inclusive display_items *)
  let s_init =
    make_state
      ~selected:services_start_idx
      ~num_columns:2
      ~active_column:0
      ~external_services:[ext]
      ~external_section_folded:true
      [svc]
  in
  let display_items = Instances_state.display_ordered_items s_init in
  let sections = Instances_layout.group_by_role [svc] in
  let col0_indices =
    Instances_layout.services_in_column
      ~num_columns:2
      ~sections
      ~display_items
      0
  in
  let last_col0 = List.nth col0_indices (List.length col0_indices - 1) in
  let s =
    {s_init with Instances_state.selected = last_col0 + services_start_idx}
  in
  let s' = move s 1 in
  check
    int
    "folded external: stays at last managed"
    (last_col0 + services_start_idx)
    s'.selected

(* ============================================================ *)
(* Test Suite                                                   *)
(* ============================================================ *)

let () =
  Alcotest.run
    "Instances Navigation"
    [
      ( "single-column",
        [
          ( "up from first service stays at top",
            `Quick,
            test_single_column_up_from_first_service );
          ( "down from first service -> second service",
            `Quick,
            test_single_column_down_from_first_service );
          ( "navigate through services",
            `Quick,
            test_single_column_navigate_through_services );
        ] );
      ( "multi-column",
        [
          ( "up from first service stays at top",
            `Quick,
            test_multi_column_up_from_first_service_stays_at_top );
          ( "up from column 1 stays at top",
            `Quick,
            test_multi_column_up_from_second_column_stays_at_top );
          ("down within column", `Quick, test_multi_column_down_within_column);
          ( "navigation starts at services",
            `Quick,
            test_multi_column_navigation_starts_at_services );
          ( "up does not overshoot",
            `Quick,
            test_multi_column_up_does_not_overshoot );
          ("up/down navigation", `Quick, test_multi_column_up_down_navigation);
        ] );
      ( "empty-state",
        [
          ( "navigates to ghost (single column)",
            `Quick,
            test_empty_state_navigates_to_ghost );
          ( "navigates to ghost (multi column)",
            `Quick,
            test_empty_state_multi_column_navigates_to_ghost );
          ( "ensure_valid_column preserves ghost selection",
            `Quick,
            test_empty_state_ensure_valid_column_preserves_selection );
          ( "wide terminal multi-column navigation",
            `Quick,
            test_empty_state_wide_terminal_multi_column_navigation );
        ] );
      ( "external-section",
        [
          ( "single-col: j from last managed reaches external",
            `Quick,
            test_single_column_j_from_last_managed_reaches_external );
          ( "multi-col col0: j from last managed reaches external",
            `Quick,
            test_multi_column_col0_j_from_last_managed_reaches_external );
          ( "multi-col col1: j from last managed reaches external",
            `Quick,
            test_multi_column_col1_j_from_last_managed_reaches_external );
          ( "folded external: no transition from any column",
            `Quick,
            test_folded_external_no_transition );
        ] );
    ]
