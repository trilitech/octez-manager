(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Ocaml_lint_hook

let make_noop_runner () =
  let called = ref [] in
  let run_command cmd =
    called := cmd :: !called ;
    0
  in
  (called, run_command)

(* AC-1: dune build @fmt is invoked when the hook runs *)
let test_fmt_check_invoked () =
  let called, run_command = make_noop_runner () in
  let _ = run_hook ~run_command () in
  Alcotest.(check bool)
    "dune build @fmt was invoked"
    true
    (List.exists (String.equal "dune build @fmt") !called)

(* AC-2: dune build is invoked when the hook runs *)
let test_build_invoked () =
  let called, run_command = make_noop_runner () in
  let _ = run_hook ~run_command () in
  Alcotest.(check bool)
    "dune build was invoked"
    true
    (List.exists (String.equal "dune build") !called)

(* AC-3: dune build @fmt failure blocks build (dune build is not called) *)
let test_fmt_failure_blocks_build () =
  let build_called = ref false in
  let run_command cmd =
    if String.equal cmd "dune build @fmt" then 2
    else begin
      if String.equal cmd "dune build" then build_called := true ;
      0
    end
  in
  let result = run_hook ~run_command () in
  Alcotest.(check bool)
    "build was not called after fmt failure"
    false
    !build_called ;
  Alcotest.(check bool) "result is an error" true (Result.is_error result)

(* AC-4: dune build failure produces an error result *)
let test_build_failure_is_error () =
  let run_command cmd = if String.equal cmd "dune build" then 1 else 0 in
  let result = run_hook ~run_command () in
  Alcotest.(check bool)
    "result is an error on build failure"
    true
    (Result.is_error result)

let () =
  Alcotest.run
    "ocaml_lint_hook"
    [
      ( "pre-build hook",
        [
          Alcotest.test_case
            "dune build @fmt is invoked (AC-1)"
            `Quick
            test_fmt_check_invoked;
          Alcotest.test_case
            "dune build is invoked (AC-2)"
            `Quick
            test_build_invoked;
          Alcotest.test_case
            "fmt check failure blocks build (AC-3)"
            `Quick
            test_fmt_failure_blocks_build;
          Alcotest.test_case
            "build failure is an error (AC-4)"
            `Quick
            test_build_failure_is_error;
        ] );
    ]
