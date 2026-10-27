(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

let issue_url issue =
  Printf.sprintf "https://github.com/trilitech/octez-manager/issues/%d" issue

let alcotest_assertion = "Alcotest assertion failure"

let strip_ansi s = Str.global_replace (Str.regexp "\027\\[[0-9;]*m") "" s

let contains ~sub s =
  match Str.search_forward (Str.regexp_string sub) s 0 with
  | _ -> true
  | exception Not_found -> false

(* Alcotest does not export its assertion exception; its printer starts with
   [alcotest_assertion] and includes the failed check as "FAIL <message>". *)
let known_failure ~issue ~expect test () =
  match test () with
  | () ->
      Alcotest.failf
        "quarantined case passed: remove its Test_quarantine.known_failure \
         wrapper (%s)"
        (issue_url issue)
  | exception e
    when String.starts_with ~prefix:alcotest_assertion (Printexc.to_string e) ->
      let printed = strip_ansi (Printexc.to_string e) in
      if contains ~sub:("FAIL " ^ expect) printed then
        Printf.printf "⚠ known failure %S (%s)\n%!" expect (issue_url issue)
      else
        Alcotest.failf
          "quarantined case failed on a different assertion than %S (%s):\n%s"
          expect
          (issue_url issue)
          printed
