(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Ocaml_lint_hook

(* Helpers *)

let make_recorder () =
  let calls = ref [] in
  let run_command cmd =
    calls := !calls @ [cmd] ;
    0
  in
  (calls, run_command)

(* AC #24: dune build @fmt is executed as part of the pre-build lint hook *)
let test_lint_hook_runs_fmt_check () =
  let calls, run_command = make_recorder () in
  let _result = run_hook ~run_command () in
  Alcotest.(check bool)
    "dune build @fmt was invoked"
    true
    (List.exists (String.equal "dune build @fmt") !calls)

(* AC #24: dune build is executed as part of the pre-build lint hook *)
let test_lint_hook_runs_dune_build () =
  let calls, run_command = make_recorder () in
  let _result = run_hook ~run_command () in
  Alcotest.(check bool)
    "dune build was invoked"
    true
    (List.exists (String.equal "dune build") !calls)

(* AC #24: failure in dune build @fmt blocks dune build from running *)
let test_lint_hook_fmt_failure_blocks_build () =
  let build_called = ref false in
  let run_command cmd =
    if String.equal cmd "dune build @fmt" then 1
    else begin
      if String.equal cmd "dune build" then build_called := true ;
      0
    end
  in
  let result = run_hook ~run_command () in
  Alcotest.(check bool)
    "dune build not called after fmt failure"
    false
    !build_called ;
  Alcotest.(check bool) "fmt failure is an error" true (Result.is_error result)

(* AC #24: failure in dune build blocks the build (result is an error) *)
let test_lint_hook_build_failure_is_error () =
  let run_command cmd = if String.equal cmd "dune build" then 2 else 0 in
  let result = run_hook ~run_command () in
  Alcotest.(check bool)
    "build failure is an error"
    true
    (Result.is_error result)

(* AC #24: when both commands succeed the hook returns Ok *)
let test_lint_hook_both_pass_returns_ok () =
  let run_command _cmd = 0 in
  let result = run_hook ~run_command () in
  Alcotest.(check bool) "both pass gives Ok" true (Result.is_ok result)

(* AC #24: exit_code_of_result maps Ok to 0 so the build is not blocked *)
let test_lint_hook_exit_code_ok () =
  Alcotest.(check int) "exit code 0 for Ok" 0 (exit_code_of_result (Ok ()))

(* AC #24: exit_code_of_result maps Error to non-zero so the build is blocked *)
let test_lint_hook_exit_code_error () =
  let err = Error (Fmt_check_failed 1) in
  Alcotest.(check bool)
    "exit code non-zero for Error"
    true
    (exit_code_of_result err <> 0)

let pre_build_hook_tests =
  [
    ( "dune build @fmt is invoked (AC #24)",
      `Quick,
      test_lint_hook_runs_fmt_check );
    ("dune build is invoked (AC #24)", `Quick, test_lint_hook_runs_dune_build);
    ( "fmt failure blocks build (AC #24)",
      `Quick,
      test_lint_hook_fmt_failure_blocks_build );
    ( "build failure is an error (AC #24)",
      `Quick,
      test_lint_hook_build_failure_is_error );
    ( "both commands pass gives Ok (AC #24)",
      `Quick,
      test_lint_hook_both_pass_returns_ok );
  ]

let exit_code_tests =
  [
    ("exit code 0 for Ok (AC #24)", `Quick, test_lint_hook_exit_code_ok);
    ( "exit code non-zero for Error (AC #24)",
      `Quick,
      test_lint_hook_exit_code_error );
  ]

let () =
  Alcotest.run
    "ocaml_lint_hook_demo_13"
    [
      ("pre-build hook", pre_build_hook_tests);
      ("exit code mapping", exit_code_tests);
    ]
