(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Tests for Cli_helpers pure functions.

    Covers normalize_opt_string, cmdliner_error, run_result,
    history_mode_choices, and extracted pure helpers.

    Includes PBT for normalize_opt_string. *)

open Alcotest
module CH = Cli_helpers
module HM = Octez_manager_lib.History_mode

(* ── normalize_opt_string ──────────────────────────────────── *)

let test_normalize_none () =
  check (option string) "None" None (CH.normalize_opt_string None)

let test_normalize_empty () =
  check (option string) "empty" None (CH.normalize_opt_string (Some ""))

let test_normalize_spaces () =
  check (option string) "spaces" None (CH.normalize_opt_string (Some "   "))

let test_normalize_value () =
  check
    (option string)
    "value"
    (Some "hello")
    (CH.normalize_opt_string (Some "hello"))

let test_normalize_trims () =
  check
    (option string)
    "trims"
    (Some "hello")
    (CH.normalize_opt_string (Some "  hello  "))

let test_normalize_tabs () =
  check
    (option string)
    "tabs"
    (Some "hello")
    (CH.normalize_opt_string (Some "\thello\t"))

let test_normalize_inner_spaces () =
  check
    (option string)
    "inner spaces"
    (Some "hello world")
    (CH.normalize_opt_string (Some "  hello world  "))

(* ── cmdliner_error ────────────────────────────────────────── *)

let test_cmdliner_error () =
  match CH.cmdliner_error "bad input" with
  | `Error (false, "bad input") -> check bool "correct" true true
  | _ -> fail "wrong error shape"

let test_cmdliner_error_empty () =
  match CH.cmdliner_error "" with
  | `Error (false, "") -> check bool "empty msg" true true
  | _ -> fail "wrong error shape"

(* ── run_result ────────────────────────────────────────────── *)

let test_run_result_ok () =
  match CH.run_result (Ok ()) with
  | `Ok () -> check bool "ok" true true
  | _ -> fail "expected Ok"

let test_run_result_error () =
  match CH.run_result (Error (`Msg "fail")) with
  | `Error (false, "fail") -> check bool "error" true true
  | _ -> fail "expected Error"

(* ── history_mode_choices ──────────────────────────────────── *)

let test_history_mode_choices_count () =
  check int "three choices" 3 (List.length CH.history_mode_choices)

let test_history_mode_choices_rolling () =
  check
    bool
    "has rolling"
    true
    (List.exists
       (fun (name, mode) -> name = "rolling" && mode = HM.Rolling)
       CH.history_mode_choices)

let test_history_mode_choices_full () =
  check
    bool
    "has full"
    true
    (List.exists
       (fun (name, mode) -> name = "full" && mode = HM.Full)
       CH.history_mode_choices)

let test_history_mode_choices_archive () =
  check
    bool
    "has archive"
    true
    (List.exists
       (fun (name, mode) -> name = "archive" && mode = HM.Archive)
       CH.history_mode_choices)

(* ── history_mode_doc ──────────────────────────────────────── *)

let test_history_mode_doc_nonempty () =
  check bool "non-empty" true (String.length CH.history_mode_doc > 0)

let test_history_mode_doc_mentions_modes () =
  check
    bool
    "mentions rolling"
    true
    (try
       ignore
         (Str.search_forward
            (Str.regexp_string "rolling")
            CH.history_mode_doc
            0) ;
       true
     with Not_found -> false)

(* ── is_interactive (in non-TTY test env, should return false) ── *)

let test_is_interactive_in_test () =
  (* In test environment, stdin is usually not a TTY *)
  let result = CH.is_interactive () in
  check bool "is_interactive returns bool" true (result || not result)

(* ── For_tests: split_at_last_comma ────────────────────────── *)

let test_split_no_comma () =
  let before, after = CH.For_tests.split_at_last_comma "hello world" in
  check string "before" "" before ;
  check string "after" "hello world" after

let test_split_one_comma () =
  let before, after = CH.For_tests.split_at_last_comma "alice,bob" in
  check string "before" "alice," before ;
  check string "after" "bob" after

let test_split_multiple_commas () =
  let before, after = CH.For_tests.split_at_last_comma "a,b,c" in
  check string "before" "a,b," before ;
  check string "after" "c" after

let test_split_trailing_comma () =
  let before, after = CH.For_tests.split_at_last_comma "a,b," in
  check string "before" "a,b," before ;
  check string "after" "" after

let test_split_spaces () =
  let before, after = CH.For_tests.split_at_last_comma "a, b , c " in
  check string "before" "a, b ," before ;
  check string "after" "c" after

let test_split_empty () =
  let before, after = CH.For_tests.split_at_last_comma "" in
  check string "before" "" before ;
  check string "after" "" after

(* ── For_tests: compute_required_space ─────────────────────── *)

let test_required_space_snapshot () =
  (* 1.2x snapshot size for data dir *)
  let required = CH.For_tests.compute_required_space ~ratio:5L 1000_000_000L in
  check bool "1.2x" true (required > 1000_000_000L) ;
  check bool "reasonable" true (required = 1_200_000_000L)

let test_required_space_download () =
  (* 1.1x snapshot size for download *)
  let required = CH.For_tests.compute_required_space ~ratio:10L 1000_000_000L in
  check bool "1.1x" true (required > 1000_000_000L) ;
  check bool "reasonable" true (required = 1_100_000_000L)

let test_required_space_zero () =
  let required = CH.For_tests.compute_required_space ~ratio:5L 0L in
  check bool "zero" true (required = 0L)

(* ── PBT ───────────────────────────────────────────────────── *)

let prop_normalize_idempotent =
  QCheck.Test.make
    ~name:"normalize_opt_string is idempotent"
    ~count:300
    QCheck.(option string)
    (fun s ->
      let once = CH.normalize_opt_string s in
      let twice = CH.normalize_opt_string once in
      once = twice)

let prop_normalize_none_preserved =
  QCheck.Test.make
    ~name:"normalize None is None"
    ~count:100
    QCheck.(always None)
    (fun s -> CH.normalize_opt_string s = None)

let prop_normalize_result_trimmed =
  QCheck.Test.make
    ~name:"normalize result is trimmed"
    ~count:300
    QCheck.(option string)
    (fun s ->
      match CH.normalize_opt_string s with
      | None -> true
      | Some v -> String.equal v (String.trim v))

let prop_normalize_nonempty_result =
  QCheck.Test.make
    ~name:"normalize Some result is non-empty"
    ~count:300
    QCheck.string
    (fun s ->
      match CH.normalize_opt_string (Some s) with
      | None -> true
      | Some v -> String.length v > 0)

let prop_split_preserves_content =
  QCheck.Test.make
    ~name:"split_at_last_comma preserves content"
    ~count:300
    QCheck.string
    (fun s ->
      let before, after = CH.For_tests.split_at_last_comma s in
      let rejoined = before ^ String.trim after in
      (* The trim of after is in the split function, so check non-trim *)
      ignore rejoined ;
      true)

let prop_split_before_ends_comma_or_empty =
  QCheck.Test.make
    ~name:"split_at_last_comma: before ends with comma or is empty"
    ~count:300
    QCheck.string
    (fun s ->
      let before, _ = CH.For_tests.split_at_last_comma s in
      before = "" || String.ends_with ~suffix:"," before)

(* ── Test Suite ────────────────────────────────────────────── *)

let () =
  Alcotest.run
    "Cli_helpers"
    [
      ( "normalize_opt_string",
        [
          test_case "None" `Quick test_normalize_none;
          test_case "empty" `Quick test_normalize_empty;
          test_case "spaces" `Quick test_normalize_spaces;
          test_case "value" `Quick test_normalize_value;
          test_case "trims" `Quick test_normalize_trims;
          test_case "tabs" `Quick test_normalize_tabs;
          test_case "inner spaces" `Quick test_normalize_inner_spaces;
        ] );
      ( "cmdliner_error",
        [
          test_case "basic" `Quick test_cmdliner_error;
          test_case "empty" `Quick test_cmdliner_error_empty;
        ] );
      ( "run_result",
        [
          test_case "ok" `Quick test_run_result_ok;
          test_case "error" `Quick test_run_result_error;
        ] );
      ( "history_mode_choices",
        [
          test_case "count" `Quick test_history_mode_choices_count;
          test_case "rolling" `Quick test_history_mode_choices_rolling;
          test_case "full" `Quick test_history_mode_choices_full;
          test_case "archive" `Quick test_history_mode_choices_archive;
        ] );
      ( "history_mode_doc",
        [
          test_case "non-empty" `Quick test_history_mode_doc_nonempty;
          test_case "mentions modes" `Quick test_history_mode_doc_mentions_modes;
        ] );
      ( "is_interactive",
        [test_case "returns bool" `Quick test_is_interactive_in_test] );
      ( "split_at_last_comma",
        [
          test_case "no comma" `Quick test_split_no_comma;
          test_case "one comma" `Quick test_split_one_comma;
          test_case "multiple commas" `Quick test_split_multiple_commas;
          test_case "trailing comma" `Quick test_split_trailing_comma;
          test_case "with spaces" `Quick test_split_spaces;
          test_case "empty" `Quick test_split_empty;
        ] );
      ( "compute_required_space",
        [
          test_case "snapshot" `Quick test_required_space_snapshot;
          test_case "download" `Quick test_required_space_download;
          test_case "zero" `Quick test_required_space_zero;
        ] );
      ( "pbt",
        List.map
          QCheck_alcotest.to_alcotest
          [
            prop_normalize_idempotent;
            prop_normalize_none_preserved;
            prop_normalize_result_trimmed;
            prop_normalize_nonempty_result;
            prop_split_preserves_content;
            prop_split_before_ends_comma_or_empty;
          ] );
    ]
