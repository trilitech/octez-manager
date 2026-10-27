(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

let issue_url issue =
  Printf.sprintf "https://github.com/trilitech/octez-manager/issues/%d" issue

let known_failure ~issue test () =
  match test () with
  | () ->
      Alcotest.failf
        "quarantined case passed: remove its Test_quarantine.known_failure \
         wrapper (%s)"
        (issue_url issue)
  | exception ((Stack_overflow | Out_of_memory) as e) -> raise e
  | exception e ->
      Printf.printf
        "⚠ known failure (%s): %s\n%!"
        (issue_url issue)
        (Printexc.to_string e)
