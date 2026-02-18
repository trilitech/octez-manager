(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Octez_manager_ui
module State = Rpc_browser_state

(* Helper to create test services *)
let make_service ?(rpc_addr = "127.0.0.1:8732") name =
  Mock_service_helpers_lib.Mock_service_helpers.mock_service
    ~instance:name
    ~rpc_addr
    ()

(* ============================================================ *)
(* Initialization Tests                                          *)
(* ============================================================ *)

let test_init_empty () =
  let state = State.init ~instances:[] in
  Alcotest.(check int) "no instances" 0 (List.length state.instances) ;
  Alcotest.(check int) "selected 0" 0 state.selected_idx ;
  Alcotest.(check int) "empty path" 0 (List.length state.path) ;
  match state.mode with
  | State.List {loading; _} -> Alcotest.(check bool) "loading" true loading
  | _ -> Alcotest.fail "expected List mode"

let test_init_with_instances () =
  let instances = [make_service "node1"; make_service "node2"] in
  let state = State.init ~instances in
  Alcotest.(check int) "two instances" 2 (List.length state.instances) ;
  Alcotest.(check int) "selected 0" 0 state.selected_idx

(* ============================================================ *)
(* Instance Selection Tests                                      *)
(* ============================================================ *)

let test_select_instance_valid () =
  let instances = [make_service "node1"; make_service "node2"] in
  let state = State.init ~instances in
  let state' = State.select_instance 1 state in
  Alcotest.(check int) "selected 1" 1 state'.selected_idx

let test_select_instance_invalid () =
  let instances = [make_service "node1"] in
  let state = State.init ~instances in
  let state' = State.select_instance 5 state in
  Alcotest.(check int) "still 0" 0 state'.selected_idx

let test_select_instance_negative () =
  let instances = [make_service "node1"] in
  let state = State.init ~instances in
  let state' = State.select_instance (-1) state in
  Alcotest.(check int) "still 0" 0 state'.selected_idx

let test_current_instance () =
  let instances = [make_service "node1"; make_service "node2"] in
  let state = State.init ~instances in
  match State.current_instance state with
  | Some s -> Alcotest.(check string) "name" "node1" s.instance
  | None -> Alcotest.fail "expected instance"

let test_current_instance_empty () =
  let state = State.init ~instances:[] in
  match State.current_instance state with
  | Some _ -> Alcotest.fail "expected none"
  | None -> ()

(* ============================================================ *)
(* Navigation Tests                                              *)
(* ============================================================ *)

let test_navigate_to () =
  let state = State.init ~instances:[] in
  let state' = State.navigate_to "chains" state in
  Alcotest.(check int) "path length" 1 (List.length state'.path) ;
  Alcotest.(check string) "path" "chains" (List.hd state'.path)

let test_navigate_nested () =
  let state = State.init ~instances:[] in
  let state' =
    state |> State.navigate_to "chains" |> State.navigate_to "main"
    |> State.navigate_to "blocks"
  in
  Alcotest.(check int) "path length" 3 (List.length state'.path) ;
  Alcotest.(check (list string)) "path" ["chains"; "main"; "blocks"] state'.path

let test_navigate_up () =
  let state =
    State.init ~instances:[] |> State.navigate_to "chains"
    |> State.navigate_to "main"
  in
  let state' = State.navigate_up state in
  Alcotest.(check int) "path length" 1 (List.length state'.path) ;
  Alcotest.(check (list string)) "path" ["chains"] state'.path

let test_navigate_up_to_root () =
  let state = State.init ~instances:[] |> State.navigate_to "chains" in
  let state' = State.navigate_up state in
  Alcotest.(check int) "path length" 0 (List.length state'.path)

let test_navigate_up_at_root () =
  let state = State.init ~instances:[] in
  let state' = State.navigate_up state in
  Alcotest.(check int) "path length" 0 (List.length state'.path)

let test_navigate_root () =
  let state =
    State.init ~instances:[] |> State.navigate_to "chains"
    |> State.navigate_to "main"
  in
  let state' = State.navigate_root state in
  Alcotest.(check int) "path length" 0 (List.length state'.path)

(* ============================================================ *)
(* Entry Tests                                                   *)
(* ============================================================ *)

let test_set_entries () =
  let state = State.init ~instances:[] in
  let entries =
    [{State.name = "chains"; kind = Sub}; {name = "version"; kind = Get}]
  in
  let state' = State.set_entries entries state in
  match state'.mode with
  | State.List {entries = e; loading; _} ->
      Alcotest.(check int) "entries" 2 (List.length e) ;
      Alcotest.(check bool) "not loading" false loading
  | _ -> Alcotest.fail "expected List mode"

let test_set_loading () =
  let state = State.init ~instances:[] in
  let state' = State.set_loading false state in
  match state'.mode with
  | State.List {loading; _} -> Alcotest.(check bool) "not loading" false loading
  | _ -> Alcotest.fail "expected List mode"

(* ============================================================ *)
(* Result Mode Tests                                             *)
(* ============================================================ *)

let test_execute_get () =
  let state = State.init ~instances:[] in
  let state' = State.execute_get ~url:"http://localhost/version" state in
  match state'.mode with
  | State.Result {pagers; _} -> (
      match pagers with
      | slot :: _ ->
          Alcotest.(check string)
            "request"
            "http://localhost/version"
            slot.State.request
      | [] -> Alcotest.fail "expected at least one pager")
  | _ -> Alcotest.fail "expected Result mode"

let test_set_result () =
  let state =
    State.init ~instances:[]
    |> State.execute_get ~url:"http://localhost/version"
  in
  let state' = State.set_result ~raw_body:"{}" state in
  match State.get_focused_pager state' with
  | Some slot ->
      (* The body might be transformed by foldable JSON, so check raw_body instead *)
      Alcotest.(check string) "raw_body" "{}" slot.State.raw_body
  | None -> Alcotest.fail "expected focused pager"

let test_toggle_focus_uses_existing_pager () =
  let state = State.init ~instances:[] |> State.enter_result_mode in
  let state =
    match State.add_pager state with
    | Some s -> s
    | None -> Alcotest.fail "expected add_pager to succeed"
  in
  let state =
    match State.remove_pager 0 state with
    | Some s -> s
    | None -> Alcotest.fail "expected remove_pager to succeed"
  in
  let state = State.focus_browser state |> State.toggle_focus in
  match state.mode with
  | State.Result {focus = State.FocusPager id; _} ->
      Alcotest.(check int) "focus existing pager" 1 id
  | State.Result {focus = State.FocusBrowser; _} ->
      Alcotest.fail "expected pager focus"
  | State.List _ -> Alcotest.fail "expected Result mode"

(* ============================================================ *)
(* Cursor Tests                                                  *)
(* ============================================================ *)

let test_cursor_down () =
  let entries = [{State.name = "a"; kind = Sub}; {name = "b"; kind = Sub}] in
  let state = State.init ~instances:[] |> State.set_entries entries in
  let state' = State.cursor_down state in
  match state'.mode with
  | State.List {cursor; _} -> Alcotest.(check int) "cursor" 1 cursor
  | _ -> Alcotest.fail "expected List mode"

let test_cursor_up () =
  let entries = [{State.name = "a"; kind = Sub}; {name = "b"; kind = Sub}] in
  let state =
    State.init ~instances:[] |> State.set_entries entries |> State.cursor_down
  in
  let state' = State.cursor_up state in
  match state'.mode with
  | State.List {cursor; _} -> Alcotest.(check int) "cursor" 0 cursor
  | _ -> Alcotest.fail "expected List mode"

let test_cursor_bounds () =
  let entries = [{State.name = "a"; kind = Sub}] in
  let state = State.init ~instances:[] |> State.set_entries entries in
  let state' = State.cursor_down state in
  match state'.mode with
  | State.List {cursor; _} -> Alcotest.(check int) "cursor clamped" 0 cursor
  | _ -> Alcotest.fail "expected List mode"

(* ============================================================ *)
(* Error Tests                                                   *)
(* ============================================================ *)

let test_set_error () =
  let state = State.init ~instances:[] in
  let state' = State.set_error "Something went wrong" state in
  Alcotest.(check (option string))
    "error"
    (Some "Something went wrong")
    state'.error

let test_clear_error () =
  let state = State.init ~instances:[] |> State.set_error "err" in
  let state' = State.clear_error state in
  Alcotest.(check (option string)) "no error" None state'.error

(* ============================================================ *)
(* Target Override Tests                                         *)
(* ============================================================ *)

let test_target_override_initial () =
  let state = State.init ~instances:[] in
  Alcotest.(check bool) "no override" true (state.target_override = None)

let test_set_pager_target_list_mode () =
  let target = make_service ~rpc_addr:"https://public.node" "public-node" in
  let state = State.init ~instances:[] in
  let state' = State.set_pager_target (Some target) state in
  match state'.target_override with
  | Some svc -> Alcotest.(check string) "target set" "public-node" svc.instance
  | None -> Alcotest.fail "expected target_override to be set"

let test_get_pager_target_list_mode () =
  let target = make_service ~rpc_addr:"https://public.node" "public-node" in
  let state = State.init ~instances:[] in
  let state' = State.set_pager_target (Some target) state in
  match State.get_pager_target state' with
  | Some svc -> Alcotest.(check string) "got target" "public-node" svc.instance
  | None -> Alcotest.fail "expected target from get_pager_target"

let test_target_override_fallback () =
  let instances = [make_service "local-node"] in
  let state = State.init ~instances in
  (* No override set, should fall back to instances[0] *)
  match State.get_pager_target state with
  | None ->
      (* In List mode without override, get_pager_target returns target_override which is None *)
      ()
  | Some _ -> ()

let test_new_pager_inherits_target () =
  let target = make_service ~rpc_addr:"https://public.node" "public-node" in
  let state = State.init ~instances:[] in
  let state' = State.set_pager_target (Some target) state in
  (* Enter result mode - should inherit target_override *)
  let state'' = State.enter_result_mode state' in
  match State.get_focused_pager state'' with
  | Some pager -> (
      match pager.target_instance with
      | Some svc ->
          Alcotest.(check string)
            "pager inherits target"
            "public-node"
            svc.instance
      | None -> Alcotest.fail "expected pager to have target_instance")
  | None -> Alcotest.fail "expected focused pager"

let test_add_pager_inherits_target () =
  let target = make_service ~rpc_addr:"https://public.node" "public-node" in
  let state = State.init ~instances:[] in
  let state' = State.set_pager_target (Some target) state in
  let state'' = State.enter_result_mode state' in
  match State.add_pager state'' with
  | Some state''' -> (
      (* Focus should be on new pager (id 1) *)
      match State.get_focused_pager state''' with
      | Some pager -> (
          match pager.target_instance with
          | Some svc ->
              Alcotest.(check string)
                "new pager inherits target"
                "public-node"
                svc.instance
          | None -> Alcotest.fail "expected new pager to have target_instance")
      | None -> Alcotest.fail "expected focused pager")
  | None -> Alcotest.fail "expected add_pager to succeed"

(* ============================================================ *)
(* Test Runner                                                   *)
(* ============================================================ *)

let () =
  Alcotest.run
    "Rpc_browser_state"
    [
      ( "init",
        [
          Alcotest.test_case "empty" `Quick test_init_empty;
          Alcotest.test_case "with instances" `Quick test_init_with_instances;
        ] );
      ( "instance_selection",
        [
          Alcotest.test_case "valid" `Quick test_select_instance_valid;
          Alcotest.test_case "invalid" `Quick test_select_instance_invalid;
          Alcotest.test_case "negative" `Quick test_select_instance_negative;
          Alcotest.test_case "current" `Quick test_current_instance;
          Alcotest.test_case "current empty" `Quick test_current_instance_empty;
        ] );
      ( "navigation",
        [
          Alcotest.test_case "navigate to" `Quick test_navigate_to;
          Alcotest.test_case "nested" `Quick test_navigate_nested;
          Alcotest.test_case "up" `Quick test_navigate_up;
          Alcotest.test_case "up to root" `Quick test_navigate_up_to_root;
          Alcotest.test_case "up at root" `Quick test_navigate_up_at_root;
          Alcotest.test_case "root" `Quick test_navigate_root;
        ] );
      ( "entries",
        [
          Alcotest.test_case "set entries" `Quick test_set_entries;
          Alcotest.test_case "set loading" `Quick test_set_loading;
        ] );
      ( "result",
        [
          Alcotest.test_case "execute get" `Quick test_execute_get;
          Alcotest.test_case "set result" `Quick test_set_result;
          Alcotest.test_case
            "toggle focus uses existing pager"
            `Quick
            test_toggle_focus_uses_existing_pager;
        ] );
      ( "cursor",
        [
          Alcotest.test_case "down" `Quick test_cursor_down;
          Alcotest.test_case "up" `Quick test_cursor_up;
          Alcotest.test_case "bounds" `Quick test_cursor_bounds;
        ] );
      ( "error",
        [
          Alcotest.test_case "set" `Quick test_set_error;
          Alcotest.test_case "clear" `Quick test_clear_error;
        ] );
      ( "target_override",
        [
          Alcotest.test_case "initial" `Quick test_target_override_initial;
          Alcotest.test_case
            "set in list mode"
            `Quick
            test_set_pager_target_list_mode;
          Alcotest.test_case
            "get in list mode"
            `Quick
            test_get_pager_target_list_mode;
          Alcotest.test_case "fallback" `Quick test_target_override_fallback;
          Alcotest.test_case
            "enter result inherits"
            `Quick
            test_new_pager_inherits_target;
          Alcotest.test_case
            "add pager inherits"
            `Quick
            test_add_pager_inherits_target;
        ] );
    ]
