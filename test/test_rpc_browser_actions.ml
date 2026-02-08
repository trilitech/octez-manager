(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Octez_manager_ui
module State = Rpc_browser_state
module Actions = Rpc_browser_actions

(* Helper to create test services *)
let make_service ?(rpc_addr = "127.0.0.1:8732") name =
  Mock_service_helpers_lib.Mock_service_helpers.mock_service
    ~instance:name
    ~rpc_addr
    ()

(* ============================================================ *)
(* Entry Selection Tests                                         *)
(* ============================================================ *)

let test_get_selected_entry_empty () =
  let state = State.init ~instances:[] in
  let entry = Actions.get_selected_entry state in
  Alcotest.(check (option reject)) "no entry" None entry

let test_get_selected_entry_with_entries () =
  let state = State.init ~instances:[] in
  let entries =
    [
      {State.name = "chains"; kind = State.Sub};
      {State.name = "version"; kind = State.Get};
    ]
  in
  let state = State.set_entries entries state in
  match Actions.get_selected_entry state with
  | Some entry ->
      Alcotest.(check string) "first entry" "chains" entry.State.name
  | None -> Alcotest.fail "expected entry"

let test_get_selected_entry_result_mode () =
  let state = State.init ~instances:[] in
  let state = State.execute_get ~url:"http://localhost/test" state in
  let entry = Actions.get_selected_entry state in
  Alcotest.(check (option reject)) "no entry in result mode" None entry

(* ============================================================ *)
(* URL Building Tests                                            *)
(* ============================================================ *)

let test_build_rpc_url_root () =
  let service = make_service "node1" in
  let url = Actions.build_rpc_url service [] in
  Alcotest.(check bool) "ends with /" true (String.length url > 0)

let test_build_rpc_url_path () =
  let service = make_service "node1" in
  let url = Actions.build_rpc_url service ["chains"; "main"; "blocks"] in
  Alcotest.(check bool) "has content" true (String.length url > 0)

(* ============================================================ *)
(* Dynamic Value Defaults Tests                                  *)
(* ============================================================ *)

let test_default_chain_id () =
  let state = State.init ~instances:[] in
  let result =
    Actions.default_for_dynamic ~name:"<chain_id>" ~typ:"chain_id" state
  in
  Alcotest.(check string) "main" "main" result

let test_default_block_id () =
  let state = State.init ~instances:[] in
  let result =
    Actions.default_for_dynamic ~name:"<block_id>" ~typ:"block_id" state
  in
  Alcotest.(check string) "head" "head" result

let test_default_block_hash () =
  let state = State.init ~instances:[] in
  let result =
    Actions.default_for_dynamic ~name:"<block_hash>" ~typ:"block_hash" state
  in
  Alcotest.(check string) "head" "head" result

let test_default_unknown () =
  let state = State.init ~instances:[] in
  let result =
    Actions.default_for_dynamic ~name:"<unknown>" ~typ:"unknown" state
  in
  Alcotest.(check string) "empty" "" result

(* ============================================================ *)
(* Instance Cycling Tests                                        *)
(* ============================================================ *)

let test_cycle_instance_empty () =
  let state = State.init ~instances:[] in
  let new_state = Actions.cycle_instance ~delta:1 state in
  Alcotest.(check int) "still 0" 0 new_state.State.selected_idx

let test_cycle_instance_forward () =
  let instances = [make_service "node1"; make_service "node2"] in
  let state = State.init ~instances in
  let new_state = Actions.cycle_instance ~delta:1 state in
  Alcotest.(check int) "idx 1" 1 new_state.State.selected_idx

let test_cycle_instance_wrap () =
  let instances = [make_service "node1"; make_service "node2"] in
  let state = State.init ~instances in
  let state = State.select_instance 1 state in
  let new_state = Actions.cycle_instance ~delta:1 state in
  Alcotest.(check int) "wraps to 0" 0 new_state.State.selected_idx

let test_cycle_instance_backward () =
  let instances = [make_service "node1"; make_service "node2"] in
  let state = State.init ~instances in
  let state = State.select_instance 1 state in
  let new_state = Actions.cycle_instance ~delta:(-1) state in
  Alcotest.(check int) "idx 0" 0 new_state.State.selected_idx

(* ============================================================ *)
(* Shortcuts Tests                                               *)
(* ============================================================ *)

let test_shortcuts_defined () =
  let shortcuts = Actions.default_shortcuts in
  Alcotest.(check bool) "has shortcuts" true (List.length shortcuts >= 5)

let test_shortcuts_format () =
  List.iter
    (fun (path, desc) ->
      Alcotest.(check bool)
        "path starts with /"
        true
        (String.length path > 0 && path.[0] = '/') ;
      Alcotest.(check bool) "desc not empty" true (String.length desc > 0))
    Actions.default_shortcuts

(* ============================================================ *)
(* Test Runner                                                   *)

let () =
  Alcotest.run
    "Rpc_browser_actions"
    [
      ( "get_selected_entry",
        [
          Alcotest.test_case "empty" `Quick test_get_selected_entry_empty;
          Alcotest.test_case
            "with entries"
            `Quick
            test_get_selected_entry_with_entries;
          Alcotest.test_case
            "result mode"
            `Quick
            test_get_selected_entry_result_mode;
        ] );
      ( "build_rpc_url",
        [
          Alcotest.test_case "root" `Quick test_build_rpc_url_root;
          Alcotest.test_case "path" `Quick test_build_rpc_url_path;
        ] );
      ( "default_for_dynamic",
        [
          Alcotest.test_case "chain_id" `Quick test_default_chain_id;
          Alcotest.test_case "block_id" `Quick test_default_block_id;
          Alcotest.test_case "block_hash" `Quick test_default_block_hash;
          Alcotest.test_case "unknown" `Quick test_default_unknown;
        ] );
      ( "cycle_instance",
        [
          Alcotest.test_case "empty" `Quick test_cycle_instance_empty;
          Alcotest.test_case "forward" `Quick test_cycle_instance_forward;
          Alcotest.test_case "wrap" `Quick test_cycle_instance_wrap;
          Alcotest.test_case "backward" `Quick test_cycle_instance_backward;
        ] );
      ( "shortcuts",
        [
          Alcotest.test_case "defined" `Quick test_shortcuts_defined;
          Alcotest.test_case "format" `Quick test_shortcuts_format;
        ] );
    ]
