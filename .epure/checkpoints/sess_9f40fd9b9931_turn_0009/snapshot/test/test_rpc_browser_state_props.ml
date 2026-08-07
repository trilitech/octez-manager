(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** QCheck state machine properties for rpc_browser_state.ml.

    Generates random sequences of operations and verifies
    invariants hold after every step. *)

module State = Octez_manager_ui.Rpc_browser_state

(* ================================================================== *)
(* Helpers                                                             *)
(* ================================================================== *)

let make_service name =
  Mock_service_helpers_lib.Mock_service_helpers.mock_service ~instance:name ()

let instances = [make_service "node-a"; make_service "node-b"]

let mk_entries n =
  List.init n (fun i -> State.{name = Printf.sprintf "entry_%d" i; kind = Sub})

(** Build a fresh state without I/O by constructing the record directly. *)
let fresh_state () =
  State.
    {
      instances;
      selected_idx = 0;
      path = [];
      mode = List {entries = []; cursor = 0; loading = false};
      openapi_status = NotAvailable;
      error = None;
      dynamic_history = [];
      recent_paths = [];
      cached_entries = [];
      cached_cursor = 0;
      target_override = None;
    }

(* ================================================================== *)
(* Invariant checker                                                   *)
(* ================================================================== *)

let check_invariants state =
  (* selected_idx in bounds *)
  let n_inst = List.length state.State.instances in
  let idx_ok =
    if n_inst = 0 then state.selected_idx >= 0
    else state.selected_idx >= 0 && state.selected_idx < n_inst
  in
  if not idx_ok then
    QCheck.Test.fail_reportf
      "selected_idx=%d out of bounds (instances=%d)"
      state.selected_idx
      n_inst ;
  (* mode invariants *)
  (match state.mode with
  | State.List {cursor; entries; _} ->
      let len = List.length entries in
      if len > 0 && (cursor < 0 || cursor >= len) then
        QCheck.Test.fail_reportf
          "List cursor=%d out of bounds (entries=%d)"
          cursor
          len ;
      if len = 0 && cursor <> 0 then
        QCheck.Test.fail_reportf "List cursor=%d but entries empty" cursor
  | State.Result {pagers; focus; _} -> (
      let n = List.length pagers in
      if n < 1 || n > 10 then
        QCheck.Test.fail_reportf "pager count=%d out of [1,10]" n ;
      (* all IDs unique *)
      let ids = List.map (fun (p : State.pager_slot) -> p.id) pagers in
      let unique = List.sort_uniq Int.compare ids in
      if List.length unique <> List.length ids then
        QCheck.Test.fail_reportf "duplicate pager IDs" ;
      (* all IDs in 0-9 *)
      List.iter
        (fun id ->
          if id < 0 || id > 9 then
            QCheck.Test.fail_reportf "pager id=%d out of 0-9" id)
        ids ;
      (* focus valid *)
      match focus with
      | State.FocusBrowser -> ()
      | State.FocusPager id ->
          if not (List.mem id ids) then
            QCheck.Test.fail_reportf "focus on pager %d which doesn't exist" id)) ;
  (* cached_cursor in bounds *)
  let cached_len = List.length state.cached_entries in
  if cached_len > 0 && state.cached_cursor >= cached_len then
    QCheck.Test.fail_reportf
      "cached_cursor=%d out of bounds (cached=%d)"
      state.cached_cursor
      cached_len ;
  true

(* ================================================================== *)
(* Operations (avoid I/O: skip add_dynamic_value, add_recent_path)     *)
(* ================================================================== *)

type op =
  | NavigateTo of string
  | NavigateUp
  | NavigateRoot
  | SelectInstance of int
  | SetEntries of int (* number of entries *)
  | CursorUp
  | CursorDown
  | EnterResultMode
  | AddPager
  | RemovePager of int
  | FocusPager of int
  | FocusBrowser
  | ToggleFocus
  | SetError of string
  | ClearError
  | CachedCursorUp
  | CachedCursorDown

let apply_op state = function
  | NavigateTo s -> State.navigate_to s state
  | NavigateUp -> State.navigate_up state
  | NavigateRoot -> State.navigate_root state
  | SelectInstance i -> State.select_instance i state
  | SetEntries n -> State.set_entries (mk_entries n) state
  | CursorUp -> State.cursor_up state
  | CursorDown -> State.cursor_down state
  | EnterResultMode -> State.enter_result_mode state
  | AddPager -> (
      match State.add_pager state with Some s -> s | None -> state)
  | RemovePager id -> (
      match State.remove_pager id state with Some s -> s | None -> state)
  | FocusPager id -> State.focus_pager id state
  | FocusBrowser -> State.focus_browser state
  | ToggleFocus -> State.toggle_focus state
  | SetError msg -> State.set_error msg state
  | ClearError -> State.clear_error state
  | CachedCursorUp -> State.cached_cursor_up state
  | CachedCursorDown -> State.cached_cursor_down state

let pp_op = function
  | NavigateTo s -> Printf.sprintf "NavigateTo(%s)" s
  | NavigateUp -> "NavigateUp"
  | NavigateRoot -> "NavigateRoot"
  | SelectInstance i -> Printf.sprintf "SelectInstance(%d)" i
  | SetEntries n -> Printf.sprintf "SetEntries(%d)" n
  | CursorUp -> "CursorUp"
  | CursorDown -> "CursorDown"
  | EnterResultMode -> "EnterResultMode"
  | AddPager -> "AddPager"
  | RemovePager id -> Printf.sprintf "RemovePager(%d)" id
  | FocusPager id -> Printf.sprintf "FocusPager(%d)" id
  | FocusBrowser -> "FocusBrowser"
  | ToggleFocus -> "ToggleFocus"
  | SetError s -> Printf.sprintf "SetError(%s)" s
  | ClearError -> "ClearError"
  | CachedCursorUp -> "CachedCursorUp"
  | CachedCursorDown -> "CachedCursorDown"

(* ================================================================== *)
(* Generators                                                          *)
(* ================================================================== *)

let gen_segment =
  QCheck.Gen.oneof_list
    ["chains"; "main"; "blocks"; "head"; "metadata"; "helpers"]

let gen_op =
  QCheck.Gen.(
    oneof_weighted
      [
        (3, map (fun s -> NavigateTo s) gen_segment);
        (2, return NavigateUp);
        (1, return NavigateRoot);
        (2, map (fun i -> SelectInstance i) (int_range (-1) 3));
        (3, map (fun n -> SetEntries n) (int_range 0 10));
        (3, return CursorUp);
        (3, return CursorDown);
        (2, return EnterResultMode);
        (2, return AddPager);
        (1, map (fun i -> RemovePager i) (int_range 0 9));
        (2, map (fun i -> FocusPager i) (int_range 0 9));
        (1, return FocusBrowser);
        (1, return ToggleFocus);
        (1, map (fun s -> SetError s) (return "test error"));
        (1, return ClearError);
        (1, return CachedCursorUp);
        (1, return CachedCursorDown);
      ])

let gen_ops = QCheck.Gen.list_size (QCheck.Gen.int_range 5 30) gen_op

let show_ops ops = String.concat "; " (List.map pp_op ops)

(* ================================================================== *)
(* Properties                                                          *)
(* ================================================================== *)

(** Invariants hold after every step of a random operation sequence. *)
let prop_invariants_hold =
  QCheck_alcotest.to_alcotest
    (QCheck.Test.make
       ~count:500
       ~name:"invariants hold after random ops"
       (QCheck.make ~print:show_ops gen_ops)
       (fun ops ->
         let _final =
           List.fold_left
             (fun state op ->
               let state' = apply_op state op in
               ignore (check_invariants state') ;
               state')
             (fresh_state ())
             ops
         in
         true))

(** navigate_to then navigate_up restores previous path. *)
let prop_navigate_roundtrip =
  QCheck_alcotest.to_alcotest
    (QCheck.Test.make
       ~count:300
       ~name:"navigate_to then navigate_up roundtrip"
       (QCheck.make gen_segment)
       (fun seg ->
         let state =
           fresh_state ()
           |> State.set_entries (mk_entries 3)
           |> State.navigate_to "chains"
           |> State.set_entries (mk_entries 3)
         in
         let before_path = state.path in
         let after = state |> State.navigate_to seg |> State.navigate_up in
         after.path = before_path))

(** navigate_root is idempotent. *)
let prop_navigate_root_idempotent =
  QCheck_alcotest.to_alcotest
    (QCheck.Test.make
       ~count:200
       ~name:"navigate_root is idempotent"
       (QCheck.make gen_ops)
       (fun ops ->
         let state =
           List.fold_left (fun s op -> apply_op s op) (fresh_state ()) ops
         in
         let once = State.navigate_root state in
         let twice = State.navigate_root once in
         once.path = twice.path))

(** select_instance with invalid index leaves state unchanged. *)
let prop_select_invalid_unchanged =
  QCheck_alcotest.to_alcotest
    (QCheck.Test.make
       ~count:200
       ~name:"select_instance with invalid index is no-op"
       QCheck.(pair (make (Gen.int_range (-100) (-1))) (make gen_ops))
       (fun (bad_idx, ops) ->
         let state =
           List.fold_left (fun s op -> apply_op s op) (fresh_state ()) ops
         in
         let after = State.select_instance bad_idx state in
         state.selected_idx = after.selected_idx))

(** select_instance with valid index changes selected_idx. *)
let prop_select_valid =
  QCheck_alcotest.to_alcotest
    (QCheck.Test.make
       ~count:200
       ~name:"select_instance with valid index updates idx"
       (QCheck.make (QCheck.Gen.int_range 0 1))
       (fun idx ->
         let state = fresh_state () in
         let after = State.select_instance idx state in
         after.selected_idx = idx))

(** add_pager increases count up to 10, then returns None. *)
let prop_add_pager_count =
  QCheck_alcotest.to_alcotest
    (QCheck.Test.make
       ~count:100
       ~name:"add_pager increases count up to 10"
       QCheck.unit
       (fun () ->
         let state = fresh_state () |> State.enter_result_mode in
         let rec go s n =
           if n >= 12 then true
           else
             match State.add_pager s with
             | Some s' ->
                 let ids = State.get_pager_ids s' in
                 let count = List.length ids in
                 if count <> List.length (State.get_pager_ids s) + 1 then
                   QCheck.Test.fail_reportf
                     "add_pager didn't increase count at step %d"
                     n ;
                 go s' (n + 1)
             | None ->
                 let ids = State.get_pager_ids s in
                 List.length ids = 10
         in
         go state 1))

(** remove_pager decreases count (when > 1 pager). *)
let prop_remove_pager_count =
  QCheck_alcotest.to_alcotest
    (QCheck.Test.make
       ~count:100
       ~name:"remove_pager decreases count"
       QCheck.unit
       (fun () ->
         let state = fresh_state () |> State.enter_result_mode in
         (* Add a second pager *)
         match State.add_pager state with
         | None -> QCheck.Test.fail_reportf "couldn't add pager"
         | Some state2 -> (
             let ids = State.get_pager_ids state2 in
             let before_count = List.length ids in
             (* Remove the first pager we find *)
             let id_to_remove = List.hd ids in
             match State.remove_pager id_to_remove state2 with
             | None -> QCheck.Test.fail_reportf "couldn't remove pager"
             | Some state3 ->
                 let after_count = List.length (State.get_pager_ids state3) in
                 after_count = before_count - 1)))

(** Cannot remove the last pager. *)
let prop_cannot_remove_last_pager =
  QCheck_alcotest.to_alcotest
    (QCheck.Test.make
       ~count:50
       ~name:"cannot remove last pager"
       QCheck.unit
       (fun () ->
         let state = fresh_state () |> State.enter_result_mode in
         let ids = State.get_pager_ids state in
         match ids with
         | [id] -> State.remove_pager id state = None
         | _ -> QCheck.Test.fail_reportf "expected exactly 1 pager after enter"))

(** focus_pager then get_focused_pager_id roundtrip. *)
let prop_focus_pager_roundtrip =
  QCheck_alcotest.to_alcotest
    (QCheck.Test.make
       ~count:100
       ~name:"focus_pager roundtrip"
       QCheck.unit
       (fun () ->
         let state = fresh_state () |> State.enter_result_mode in
         (* Add a couple of pagers *)
         let state =
           match State.add_pager state with Some s -> s | None -> state
         in
         let state =
           match State.add_pager state with Some s -> s | None -> state
         in
         let ids = State.get_pager_ids state in
         List.for_all
           (fun id ->
             let state' = State.focus_pager id state in
             State.get_focused_pager_id state' = id)
           ids))

(** cursor_up / cursor_down stay in bounds after random sequences. *)
let prop_cursor_bounds =
  QCheck_alcotest.to_alcotest
    (QCheck.Test.make
       ~count:300
       ~name:"cursor stays in bounds"
       (QCheck.make
          (QCheck.Gen.pair
             (QCheck.Gen.int_range 1 20)
             (QCheck.Gen.list_size
                (QCheck.Gen.int_range 5 30)
                (QCheck.Gen.oneof_list [CursorUp; CursorDown]))))
       (fun (n_entries, moves) ->
         let state =
           fresh_state () |> State.set_entries (mk_entries n_entries)
         in
         let final = List.fold_left (fun s op -> apply_op s op) state moves in
         match final.mode with
         | State.List {cursor; entries; _} ->
             let len = List.length entries in
             cursor >= 0 && (len = 0 || cursor < len)
         | _ -> true (* mode changed *)))

(** enter_result_mode always produces exactly 1 pager. *)
let prop_enter_result_one_pager =
  QCheck_alcotest.to_alcotest
    (QCheck.Test.make
       ~count:200
       ~name:"enter_result_mode has 1 pager"
       (QCheck.make gen_ops)
       (fun ops ->
         let state =
           List.fold_left (fun s op -> apply_op s op) (fresh_state ()) ops
         in
         let result_state = State.enter_result_mode state in
         match result_state.mode with
         | State.Result {pagers; _} -> List.length pagers = 1
         | _ -> false))

(** navigate_root always empties the path. *)
let prop_navigate_root_empties_path =
  QCheck_alcotest.to_alcotest
    (QCheck.Test.make
       ~count:200
       ~name:"navigate_root empties path"
       (QCheck.make gen_ops)
       (fun ops ->
         let state =
           List.fold_left (fun s op -> apply_op s op) (fresh_state ()) ops
         in
         let rooted = State.navigate_root state in
         rooted.path = []))

(** clear_error always results in None error. *)
let prop_clear_error =
  QCheck_alcotest.to_alcotest
    (QCheck.Test.make
       ~count:200
       ~name:"clear_error always clears"
       (QCheck.make gen_ops)
       (fun ops ->
         let state =
           List.fold_left (fun s op -> apply_op s op) (fresh_state ()) ops
         in
         let cleared = State.clear_error state in
         cleared.error = None))

(* ================================================================== *)
(* TEST SUITE                                                          *)
(* ================================================================== *)

let () =
  Alcotest.run
    "RPC Browser State Props"
    [
      ( "state machine",
        [
          prop_invariants_hold;
          prop_navigate_roundtrip;
          prop_navigate_root_idempotent;
          prop_navigate_root_empties_path;
        ] );
      ("instance selection", [prop_select_invalid_unchanged; prop_select_valid]);
      ( "pager management",
        [
          prop_add_pager_count;
          prop_remove_pager_count;
          prop_cannot_remove_last_pager;
          prop_focus_pager_roundtrip;
          prop_enter_result_one_pager;
        ] );
      ("cursor", [prop_cursor_bounds]);
      ("error", [prop_clear_error]);
    ]
