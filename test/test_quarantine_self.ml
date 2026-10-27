(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Negative controls for the inverted quarantine helper. *)

let raised f = match f () with () -> false | exception _ -> true

let test_still_failing_case_passes () =
  let wrapped =
    Test_quarantine.known_failure ~issue:1 ~expect:"still red" (fun () ->
        Alcotest.fail "still red")
  in
  Alcotest.(check bool) "known failure is accepted" false (raised wrapped)

let test_crash_is_not_a_known_failure () =
  let wrapped =
    Test_quarantine.known_failure ~issue:1 ~expect:"x" (fun () ->
        raise Not_found)
  in
  Alcotest.(check bool) "crash propagates" true (raised wrapped)

let test_other_assertion_fails () =
  let wrapped =
    Test_quarantine.known_failure ~issue:1 ~expect:"still red" (fun () ->
        Alcotest.fail "a new failure")
  in
  Alcotest.(check bool) "different assertion rejected" true (raised wrapped)

let test_fixed_case_fails () =
  let wrapped =
    Test_quarantine.known_failure ~issue:1 ~expect:"x" (fun () -> ())
  in
  Alcotest.(check bool) "unexpected pass is rejected" true (raised wrapped)

let () =
  Alcotest.run
    "Test_quarantine"
    [
      ( "inversion",
        [
          ("still failing case passes", `Quick, test_still_failing_case_passes);
          ("fixed case fails", `Quick, test_fixed_case_fails);
          ("other assertion fails", `Quick, test_other_assertion_fails);
          ( "crash is not a known failure",
            `Quick,
            test_crash_is_not_a_known_failure );
        ] );
    ]
