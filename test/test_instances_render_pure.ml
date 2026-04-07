(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Tests for pure rendering functions in instances_render.ml
    and pure logic in instances_actions.ml.

    Focuses on verifying actual output content and semantic properties
    rather than just checking String.length > 0. *)

open Alcotest
open Octez_manager_lib
module Render = Octez_manager_ui.Instances_render
module Actions = Octez_manager_ui.Instances_actions
module Layout = Octez_manager_ui.Instances_layout
module State = Octez_manager_ui.Instances_state
module Mock_service_helpers = Mock_service_helpers_lib.Mock_service_helpers

let string_contains = Test_string_helpers.string_contains

let with_group group_name (svc : State.Service_state.t) =
  let service = {svc.service with Service.group = Some group_name} in
  {svc with service}

(* ================================================================== *)
(* role_header tests                                                   *)
(* ================================================================== *)

let test_role_header_known_roles () =
  check string "node" "Nodes" (Layout.role_header "node") ;
  check string "baker" "Bakers" (Layout.role_header "baker") ;
  check string "accuser" "Accusers" (Layout.role_header "accuser") ;
  check string "dal-node" "DAL Nodes" (Layout.role_header "dal-node") ;
  check string "signatory" "Signatories" (Layout.role_header "signatory")

let test_role_header_unknown_capitalizes () =
  let h = Layout.role_header "foobar" in
  (* Unknown roles get capitalized name *)
  check bool "has capitalized name" true (string_contains ~needle:"Foobar" h)

let test_rebuild_display_cache_groups_and_indexes () =
  let services =
    [
      Mock_service_helpers.running_service ~instance:"node-1" ~role:"node" ();
      with_group
        "alpha"
        (Mock_service_helpers.running_service
           ~instance:"baker-1"
           ~role:"baker"
           ());
      with_group
        "alpha"
        (Mock_service_helpers.running_service
           ~instance:"node-2"
           ~role:"node"
           ());
      Mock_service_helpers.running_service
        ~instance:"accuser-1"
        ~role:"accuser"
        ();
    ]
  in
  let state =
    State.rebuild_display_cache
      State.
        {
          services;
          external_services = [];
          selected = 0;
          folded = State.StringSet.empty;
          external_folded = State.StringSet.empty;
          external_section_folded = false;
          last_updated = 0.0;
          num_columns = 2;
          active_column = 0;
          column_scroll = [||];
          view_mode = State.By_group;
          groups = [];
          display_sections = [];
          ordered_services = [];
          ordered_service_indices = State.StringMap.empty;
          create_menu_open = false;
          create_menu_cursor = 0;
        }
  in
  check
    int
    "ordered services length"
    4
    (List.length state.State.ordered_services) ;
  check
    string
    "group section title"
    "alpha"
    (fst (List.hd state.State.display_sections)) ;
  check
    (option int)
    "node-1 index"
    (Some 2)
    (State.StringMap.find_opt "node-1" state.State.ordered_service_indices) ;
  check
    (option int)
    "accuser-1 index"
    (Some 3)
    (State.StringMap.find_opt "accuser-1" state.State.ordered_service_indices)

let test_column_items_use_cached_indices () =
  let services =
    [
      Mock_service_helpers.running_service ~instance:"node-1" ~role:"node" ();
      Mock_service_helpers.running_service ~instance:"baker-1" ~role:"baker" ();
      Mock_service_helpers.running_service
        ~instance:"accuser-1"
        ~role:"accuser"
        ();
    ]
  in
  let index_by_instance =
    List.mapi
      (fun idx (svc : State.Service_state.t) ->
        (svc.service.Service.instance, idx))
      services
    |> List.fold_left
         (fun acc (instance, idx) -> State.StringMap.add instance idx acc)
         State.StringMap.empty
  in
  let items =
    Layout.column_items
      ~column_groups:
        [("node", [List.hd services]); ("baker", [List.nth services 1])]
      ~index_by_instance
  in
  match items with
  | [
   Layout.Header _;
   Layout.Instance (0, _);
   Layout.Header _;
   Layout.Instance (1, _);
  ] ->
      ()
  | _ -> fail "column_items did not use cached indices"

(* ================================================================== *)
(* truncate_visible tests                                              *)
(* ================================================================== *)

let test_truncate_plain_short () =
  check
    string
    "short unchanged"
    "hello"
    (Render.truncate_visible ~max_width:10 "hello")

let test_truncate_plain_exact () =
  check
    string
    "exact unchanged"
    "hello"
    (Render.truncate_visible ~max_width:5 "hello")

let test_truncate_plain_over () =
  (* truncate_visible adds ANSI reset when it truncates *)
  check
    string
    "truncated with reset"
    "hel\027[0m"
    (Render.truncate_visible ~max_width:3 "hello")

let test_truncate_zero_width () =
  (* Zero visible chars, but still adds reset since text remains *)
  check
    string
    "zero → reset only"
    "\027[0m"
    (Render.truncate_visible ~max_width:0 "hello")

let test_truncate_empty_string () =
  check string "empty → empty" "" (Render.truncate_visible ~max_width:10 "")

let test_truncate_preserves_ansi_prefix () =
  (* \027[32m is green, "gre" is 3 visible chars *)
  let ansi = "\027[32mgreen\027[0m" in
  let result = Render.truncate_visible ~max_width:3 ansi in
  (* Should start with the ANSI escape *)
  check
    bool
    "starts with ESC"
    true
    (String.length result > 0 && result.[0] = '\027')

let test_truncate_ansi_reset_on_cut () =
  let ansi = "\027[32mhello world\027[0m" in
  let truncated = Render.truncate_visible ~max_width:5 ansi in
  (* Should end with reset \027[0m since we cut mid-formatting *)
  let len = String.length truncated in
  check
    bool
    "ends with reset"
    true
    (len >= 4 && String.sub truncated (len - 4) 4 = "\027[0m")

let test_truncate_no_reset_when_not_truncated () =
  let plain = "hi" in
  let result = Render.truncate_visible ~max_width:10 plain in
  check string "no extra chars" "hi" result

(* ================================================================== *)
(* pad_line tests                                                      *)
(* ================================================================== *)

let test_pad_short_line () =
  let result = Render.pad_line ~col_width:10 "hello" in
  check int "padded to 10" 10 (String.length result) ;
  check bool "starts with content" true (String.sub result 0 5 = "hello") ;
  check bool "padded with spaces" true (String.sub result 5 5 = "     ")

let test_pad_exact_line () =
  check
    string
    "exact → no change"
    "hello"
    (Render.pad_line ~col_width:5 "hello")

(* ================================================================== *)
(* summary_line tests                                                  *)
(* ================================================================== *)

let empty_state () =
  State.
    {
      services = [];
      external_services = [];
      selected = 0;
      folded = State.StringSet.empty;
      external_folded = State.StringSet.empty;
      external_section_folded = false;
      last_updated = 0.0;
      num_columns = 1;
      column_scroll = [||];
      active_column = 0;
      view_mode = State.By_role;
      groups = [];
      display_sections = [];
      ordered_services = [];
      ordered_service_indices = State.StringMap.empty;
      create_menu_open = false;
      create_menu_cursor = 0;
    }

let test_summary_empty () =
  let line = Render.summary_line (empty_state ()) in
  check bool "mentions 0" true (string_contains ~needle:"0" line)

let test_summary_managed_only () =
  let state =
    {
      (empty_state ()) with
      services =
        [
          Mock_service_helpers_lib.Mock_service_helpers.running_service
            ~instance:"n1"
            ();
        ];
    }
  in
  let line = Render.summary_line state in
  check bool "says Total" true (string_contains ~needle:"Total" line) ;
  check bool "says 1" true (string_contains ~needle:"1" line)

let test_summary_multiple () =
  let state =
    {
      (empty_state ()) with
      services =
        [
          Mock_service_helpers_lib.Mock_service_helpers.running_service
            ~instance:"n1"
            ();
          Mock_service_helpers_lib.Mock_service_helpers.running_service
            ~instance:"n2"
            ();
          Mock_service_helpers_lib.Mock_service_helpers.stopped_service
            ~instance:"n3"
            ();
        ];
    }
  in
  let line = Render.summary_line state in
  check bool "says 3" true (string_contains ~needle:"3" line)

(* ================================================================== *)
(* journalctl_args tests                                               *)
(* ================================================================== *)

let test_journalctl_args_structure () =
  let args = Actions.journalctl_args "octez-node-test.service" in
  check bool "starts with journalctl" true (List.hd args = "journalctl") ;
  check bool "has -u" true (List.mem "-u" args) ;
  check bool "has unit name" true (List.mem "octez-node-test.service" args) ;
  check bool "has --no-pager" true (List.mem "--no-pager" args) ;
  check bool "has -n" true (List.mem "-n" args) ;
  check bool "has 200" true (List.mem "200" args)

let test_journalctl_args_user_flag () =
  let args = Actions.journalctl_args "test.service" in
  let has_user = List.mem "--user" args in
  let is_root = Paths.is_root () in
  if is_root then check bool "root: no --user" false has_user
  else check bool "non-root: has --user" true has_user

let test_journalctl_args_different_units () =
  let args1 = Actions.journalctl_args "unit-a.service" in
  let args2 = Actions.journalctl_args "unit-b.service" in
  check bool "unit-a in args1" true (List.mem "unit-a.service" args1) ;
  check bool "unit-b in args2" true (List.mem "unit-b.service" args2) ;
  check bool "unit-b not in args1" false (List.mem "unit-b.service" args1)

(* ================================================================== *)
(* TEST SUITE                                                          *)
(* ================================================================== *)

let () =
  run
    "Instances Pure Logic"
    [
      ( "role_header",
        [
          test_case "known roles" `Quick test_role_header_known_roles;
          test_case
            "unknown role capitalizes"
            `Quick
            test_role_header_unknown_capitalizes;
        ] );
      ( "layout_cache",
        [
          test_case
            "cache builds ordered services and indexes"
            `Quick
            test_rebuild_display_cache_groups_and_indexes;
          test_case
            "column items use cached indices"
            `Quick
            test_column_items_use_cached_indices;
        ] );
      ( "truncate_visible",
        [
          test_case "short text" `Quick test_truncate_plain_short;
          test_case "exact width" `Quick test_truncate_plain_exact;
          test_case "over width" `Quick test_truncate_plain_over;
          test_case "zero width" `Quick test_truncate_zero_width;
          test_case "empty string" `Quick test_truncate_empty_string;
          test_case
            "preserves ANSI prefix"
            `Quick
            test_truncate_preserves_ansi_prefix;
          test_case "ANSI reset on cut" `Quick test_truncate_ansi_reset_on_cut;
          test_case
            "no reset when not truncated"
            `Quick
            test_truncate_no_reset_when_not_truncated;
        ] );
      ( "pad_line",
        [
          test_case "short line padded" `Quick test_pad_short_line;
          test_case "exact width" `Quick test_pad_exact_line;
        ] );
      ( "summary_line",
        [
          test_case "empty" `Quick test_summary_empty;
          test_case "managed only" `Quick test_summary_managed_only;
          test_case "multiple" `Quick test_summary_multiple;
        ] );
      ( "journalctl_args",
        [
          test_case "structure" `Quick test_journalctl_args_structure;
          test_case "user flag" `Quick test_journalctl_args_user_flag;
          test_case
            "different units"
            `Quick
            test_journalctl_args_different_units;
        ] );
    ]
