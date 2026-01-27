(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** TUI Command Runner
    
    Declarative test framework for TUI interactions. Tests are written as
    lists of commands that are executed in sequence with built-in regression
    testing and debugging support. *)

module HD = Lib_miaou_internal.Headless_driver
module URF = Ui_regression_framework_lib.Ui_regression_framework

(* ============================================================ *)
(* Types *)
(* ============================================================ *)

type screenshot = string

type wait_condition =
  | PageSwitched of string  (** Wait for navigation to specific page *)
  | ScreenContains of string  (** Wait for text to appear on screen *)
  | ScreenMatches of (screenshot -> bool)  (** Custom predicate *)
  | ModalActive  (** Wait for modal to open *)
  | ModalClosed  (** Wait for modal to close *)
  | MaxIterations of int  (** Timeout (required as last condition) *)

type command =
  | Key of string  (** Send single key *)
  | Type of string  (** Type text character by character *)
  | Comment of string  (** Human-readable comment (no-op) *)
  | WaitFor of wait_condition list  (** Wait for any condition *)
  | Screenshot of string  (** Capture debug screenshot *)
  | CheckRegression of string  (** Compare against baseline *)
  | CheckRegressionWith of {name : string; ignore_patterns : string list option}
      (** Compare with dynamic content masked *)
  | Assert of (screenshot -> bool) * string  (** Custom assertion *)
  | AssertService of string  (** Verify service exists in registry *)
  | Resize of int * int  (** Resize virtual terminal (rows, cols) *)

(* ============================================================ *)
(* Execution Context *)
(* ============================================================ *)

type context = {mutable step_number : int; mutable failed : bool}

let create_context () = {step_number = 0; failed = false}

(* ============================================================ *)
(* Wait Logic *)
(* ============================================================ *)

let check_condition = function
  | PageSwitched target -> (
      (* The multi-page Stateful driver auto-switches when the target page
         is in the Registry, so classify_next() returns `Continue after a
         successful switch. We check consume_last_switch() to detect this.
         Fall back to classify_next() for pages not in the Registry. *)
      match HD.Stateful.consume_last_switch () with
      | Some page when page = target -> true
      | Some _ -> false
      | None -> (
          match HD.Stateful.classify_next () with
          | `SwitchTo page when page = target -> true
          | _ -> false))
  | ScreenContains text -> (
      let screen = HD.get_screen_content () in
      String.length screen > 0
      &&
        try
          let _ = Str.search_forward (Str.regexp_string text) screen 0 in
          true
        with Not_found -> false)
  | ScreenMatches pred ->
      let screen = HD.get_screen_content () in
      pred screen
  | ModalActive -> Miaou_core.Modal_manager.has_active ()
  | ModalClosed -> not (Miaou_core.Modal_manager.has_active ())
  | MaxIterations _ -> false (* Never true - used as timeout *)

let get_max_iterations conditions =
  let rec find = function
    | [] -> 100 (* Default if not specified *)
    | MaxIterations n :: _ -> n
    | _ :: rest -> find rest
  in
  find conditions

let wait_for_condition conditions =
  let max_iter = get_max_iterations conditions in
  let non_timeout_conditions =
    List.filter (function MaxIterations _ -> false | _ -> true) conditions
  in

  let rec loop iteration =
    if iteration >= max_iter then (
      (* Capture debug screenshot on timeout for diagnostics *)
      URF.capture_debug_screenshot "waitfor_timeout" ;
      let screen = HD.get_screen_content () in
      Printf.eprintf
        "  Screen at timeout (%d chars):\n%s\n%!"
        (String.length screen)
        (if String.length screen > 500 then String.sub screen 0 500 ^ "..."
         else screen) ;
      failwith
        (Printf.sprintf
           "WaitFor timeout after %d iterations. Conditions: %s"
           max_iter
           (String.concat
              ", "
              (List.map
                 (function
                   | PageSwitched p -> "PageSwitched(" ^ p ^ ")"
                   | ScreenContains s -> "ScreenContains(" ^ s ^ ")"
                   | ScreenMatches _ -> "ScreenMatches(fn)"
                   | ModalActive -> "ModalActive"
                   | ModalClosed -> "ModalClosed"
                   | MaxIterations n -> "MaxIterations(" ^ string_of_int n ^ ")")
                 conditions))))
    else if List.exists check_condition non_timeout_conditions then ()
    else (
      (* Refresh page state *)
      ignore (HD.Stateful.idle_wait ~iterations:1 ~sleep:0.01 ()) ;
      loop (iteration + 1))
  in
  loop 0

(* ============================================================ *)
(* Command Execution *)
(* ============================================================ *)

let handle_nav_result = function
  | `Continue -> ()
  | `Quit -> failwith "Unexpected Quit during test"
  | `SwitchTo page ->
      Printf.eprintf "  → Switched to page: %s\n%!" page ;
      ()

let rec execute_command ctx = function
  | Comment text ->
      Printf.eprintf "\n%s\n%!" text ;
      ctx.step_number <- ctx.step_number + 1
  | Key key ->
      let result = HD.Stateful.send_key key in
      handle_nav_result result
  | Type text ->
      String.iter (fun c -> execute_command ctx (Key (String.make 1 c))) text
  | WaitFor conditions ->
      Printf.eprintf "  ⏳ Waiting for condition...\n%!" ;
      wait_for_condition conditions ;
      Printf.eprintf "  ✓ Condition met\n%!"
  | Screenshot name ->
      URF.capture_debug_screenshot name ;
      Printf.eprintf "  📸 Screenshot: %s\n%!" name
  | CheckRegression name ->
      Printf.eprintf "  🔍 Regression check: %s\n%!" name ;
      if not (URF.check_regression name) then (
        ctx.failed <- true ;
        failwith (Printf.sprintf "Regression check failed: %s" name))
      else Printf.eprintf "  ✓ Matches baseline\n%!"
  | CheckRegressionWith {name; ignore_patterns} ->
      Printf.eprintf "  🔍 Regression check (masked): %s\n%!" name ;
      let screen = URF.capture_screen () in
      let _masked =
        match ignore_patterns with
        | None -> screen
        | Some patterns ->
            List.fold_left
              (fun acc pattern ->
                let re = Str.regexp pattern in
                Str.global_replace re "{{MASKED}}" acc)
              screen
              patterns
      in
      (* TODO: Use masked content for comparison once URF supports it *)
      if not (URF.check_regression name) then (
        ctx.failed <- true ;
        failwith (Printf.sprintf "Regression check failed: %s" name))
      else Printf.eprintf "  ✓ Matches baseline\n%!"
  | Assert (pred, msg) ->
      Printf.eprintf "  ✓ Assertion: %s\n%!" msg ;
      let screen = HD.get_screen_content () in
      if not (pred screen) then (
        ctx.failed <- true ;
        URF.capture_debug_screenshot ("assertion_failed_" ^ msg) ;
        failwith ("Assertion failed: " ^ msg))
  | AssertService instance -> (
      Printf.eprintf "  ✓ Checking service: %s\n%!" instance ;
      match Octez_manager_lib.Service_registry.find ~instance with
      | Ok (Some svc) ->
          Printf.eprintf
            "  ✓ Service exists: %s (role: %s)\n%!"
            instance
            svc.role
      | Ok None ->
          ctx.failed <- true ;
          failwith (Printf.sprintf "Service not found: %s" instance)
      | Error (`Msg e) ->
          ctx.failed <- true ;
          failwith (Printf.sprintf "Error checking service %s: %s" instance e))
  | Resize (rows, cols) ->
      Printf.eprintf "  ↔ Resize terminal: %dx%d\n%!" rows cols ;
      HD.set_size rows cols ;
      (* Force a re-render at the new size *)
      ignore (HD.Stateful.idle_wait ~iterations:1 ~sleep:0.0 ())

(* ============================================================ *)
(* Main Runner *)
(* ============================================================ *)

let run_commands commands =
  let ctx = create_context () in
  Printf.eprintf "\n=== Running TUI Command Script ===\n%!" ;
  Printf.eprintf "Commands: %d\n\n%!" (List.length commands) ;

  let rec run = function
    | [] -> ()
    | cmd :: rest ->
        execute_command ctx cmd ;
        run rest
  in

  try
    run commands ;
    if ctx.failed then failwith "Test failed with errors"
    else Printf.eprintf "\n=== All commands completed successfully ===\n%!"
  with e ->
    Printf.eprintf "\n❌ Test failed at step %d\n%!" ctx.step_number ;
    URF.capture_debug_screenshot "test_failure" ;
    raise e
