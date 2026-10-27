(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Negative controls for the UI regression verification contract. *)

module URF = Ui_regression_framework_lib.Ui_regression_framework
module I = URF.Internal_for_tests

let issue = "https://github.com/trilitech/octez-manager/issues/1071"

let fresh_dir () =
  let dir = Filename.temp_file "om-urf-self" "" in
  Sys.remove dir ;
  Sys.mkdir dir 0o755 ;
  dir

let write path content =
  Out_channel.with_open_bin path (fun oc -> output_string oc content)

let baseline dir name screen =
  write (Filename.concat dir (name ^ ".screen")) ("SIZE:5x1\n" ^ screen)

let entries dir = Sys.readdir dir |> Array.to_list |> List.sort String.compare

let verdict_name = function
  | I.Match -> "match"
  | Mismatch _ -> "mismatch"
  | Missing _ -> "missing"
  | Known_drift _ -> "known-drift"
  | Unknown_drift _ -> "unknown-drift"
  | Unexpected_pass _ -> "unexpected-pass"

let check_verdict expected actual =
  Alcotest.(check string) "verdict" expected (verdict_name actual)

let test_missing_baseline_fails_without_writing () =
  let dir = fresh_dir () in
  check_verdict
    "missing"
    (I.verify ~dir ~quarantine:[] ~name:"absent" ~current:"hello") ;
  Alcotest.(check (list string)) "no file created" [] (entries dir)

let test_mismatch_and_match () =
  let dir = fresh_dir () in
  baseline dir "screen" "hello" ;
  check_verdict
    "match"
    (I.verify ~dir ~quarantine:[] ~name:"screen" ~current:"hello") ;
  check_verdict
    "mismatch"
    (I.verify ~dir ~quarantine:[] ~name:"screen" ~current:"world")

let test_quarantine_is_inverted () =
  let dir = fresh_dir () in
  baseline dir "screen" "hello" ;
  let drift = I.screen_digest "world" in
  let quarantine = [("screen", {I.issue; drift})] in
  check_verdict
    "known-drift"
    (I.verify ~dir ~quarantine ~name:"screen" ~current:"world") ;
  check_verdict
    "unknown-drift"
    (I.verify ~dir ~quarantine ~name:"screen" ~current:"other") ;
  check_verdict
    "unexpected-pass"
    (I.verify ~dir ~quarantine ~name:"screen" ~current:"hello")

let quarantine_result dir content =
  write (Filename.concat dir "QUARANTINE") content ;
  I.load_quarantine ~dir

let test_quarantine_parsing () =
  let dir = fresh_dir () in
  baseline dir "screen" "hello" ;
  let d = I.screen_digest "world" in
  let line name rest = String.concat " " ([name; issue] @ rest) ^ "\n" in
  (match
     quarantine_result dir ("# comment\n\n" ^ line "screen" [d; "stale"])
   with
  | Ok [("screen", {I.issue = i; drift})] ->
      Alcotest.(check string) "issue" issue i ;
      Alcotest.(check string) "drift" d drift
  | _ -> Alcotest.fail "valid quarantine rejected") ;
  let rejected content =
    Alcotest.(check bool)
      ("rejected: " ^ content)
      true
      (Result.is_error (quarantine_result dir content))
  in
  rejected (line "dead" [d; "no baseline file"]) ;
  rejected ("screen https://example.com/1 " ^ d ^ " wrong tracker\n") ;
  rejected (line "screen" ["not-a-digest"; "stale"]) ;
  rejected (line "screen" [d]) ;
  rejected (line "screen" [d; "a"] ^ line "screen" [d; "b"])

let test_unreadable_baseline_is_setup_error () =
  let dir = fresh_dir () in
  Sys.mkdir (Filename.concat dir "screen.screen") 0o755 ;
  match I.verify ~dir ~quarantine:[] ~name:"screen" ~current:"x" with
  | exception URF.Setup_error _ -> ()
  | v -> Alcotest.failf "expected setup error, got %s" (verdict_name v)

let () =
  Alcotest.run
    "UI_Regression_Framework_Contract"
    [
      ( "contract",
        [
          ( "missing baseline fails and writes nothing",
            `Quick,
            test_missing_baseline_fails_without_writing );
          ("mismatch and match", `Quick, test_mismatch_and_match);
          ("quarantine is inverted", `Quick, test_quarantine_is_inverted);
          ("quarantine parsing", `Quick, test_quarantine_parsing);
          ( "unreadable baseline is a setup error",
            `Quick,
            test_unreadable_baseline_is_setup_error );
        ] );
    ]
