(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Unit tests for Modal_helpers pure functions.

    Covers first_nonempty_line and wrap_text. *)

open Alcotest
module MH = Octez_manager_ui.Modal_helpers

(* ── first_nonempty_line ──────────────────────────────────────── *)

let test_first_nonempty_empty_list () =
  check (option string) "empty" None (MH.For_tests.first_nonempty_line [])

let test_first_nonempty_all_blank () =
  check
    (option string)
    "all blank"
    None
    (MH.For_tests.first_nonempty_line [""; "  "; "\t"; "   "])

let test_first_nonempty_first_is_content () =
  check
    (option string)
    "first"
    (Some "hello")
    (MH.For_tests.first_nonempty_line ["hello"; "world"])

let test_first_nonempty_skip_blanks () =
  check
    (option string)
    "skip blanks"
    (Some "content")
    (MH.For_tests.first_nonempty_line [""; "  "; "content"; "more"])

let test_first_nonempty_whitespace_only_not_empty () =
  (* A line with spaces is empty per String.trim *)
  check
    (option string)
    "trim spaces"
    None
    (MH.For_tests.first_nonempty_line ["   "])

(* ── wrap_text ────────────────────────────────────────────────── *)

let test_wrap_short_line () =
  let result = MH.For_tests.wrap_text ~width:40 "Hello world" in
  check (list string) "no wrap" ["Hello world"] result

let test_wrap_exact_width () =
  let line = String.make 40 'x' in
  let result = MH.For_tests.wrap_text ~width:40 line in
  check (list string) "exact" [line] result

let test_wrap_long_line () =
  let result =
    MH.For_tests.wrap_text
      ~width:20
      "This is a long sentence that should be wrapped"
  in
  check bool "wrapped" true (List.length result > 1) ;
  List.iter
    (fun line -> check bool "within width" true (String.length line <= 20))
    result

let test_wrap_preserves_newlines () =
  let result =
    MH.For_tests.wrap_text ~width:40 "line one\nline two\nline three"
  in
  check int "three lines" 3 (List.length result) ;
  check string "first" "line one" (List.nth result 0) ;
  check string "second" "line two" (List.nth result 1) ;
  check string "third" "line three" (List.nth result 2)

let test_wrap_empty_string () =
  let result = MH.For_tests.wrap_text ~width:40 "" in
  check (list string) "empty" [""] result

let test_wrap_breaks_at_space () =
  let result = MH.For_tests.wrap_text ~width:10 "one two three four" in
  (* Should break at word boundaries *)
  List.iter
    (fun line -> check bool "within width" true (String.length line <= 10))
    result ;
  let joined = String.concat " " result in
  check string "content preserved" "one two three four" joined

let test_wrap_long_word () =
  (* A single word longer than width must be hard-wrapped *)
  let word = String.make 30 'a' in
  let result = MH.For_tests.wrap_text ~width:10 word in
  check bool "was wrapped" true (List.length result > 1)

let test_wrap_newline_and_long () =
  let input =
    "short\nThis is a longer line that needs wrapping at some point"
  in
  let result = MH.For_tests.wrap_text ~width:25 input in
  check bool "first is short" true (String.length (List.nth result 0) <= 25) ;
  check string "first line" "short" (List.nth result 0) ;
  check bool "more than 2 lines" true (List.length result > 2)

let test_wrap_multiple_spaces () =
  let result =
    MH.For_tests.wrap_text ~width:20 "word1 word2 word3 word4 word5"
  in
  check bool "wrapped" true (List.length result >= 1) ;
  List.iter
    (fun line -> check bool "within width" true (String.length line <= 20))
    result

(* ── extract_major ────────────────────────────────────────────── *)

let test_extract_major_simple () =
  check int "24" 24 (MH.For_tests.extract_major "24.0")

let test_extract_major_three_parts () =
  check int "1" 1 (MH.For_tests.extract_major "1.2.3")

let test_extract_major_single () =
  check int "42" 42 (MH.For_tests.extract_major "42")

let test_extract_major_empty () =
  check int "0" 0 (MH.For_tests.extract_major "")

let test_extract_major_malformed () =
  check int "0" 0 (MH.For_tests.extract_major "abc.def")

let test_extract_major_leading_v () =
  (* "v24.0" - the 'v' makes int_of_string fail, returns 0 *)
  check int "0" 0 (MH.For_tests.extract_major "v24.0")

let test_extract_major_zero () =
  check int "0" 0 (MH.For_tests.extract_major "0.1.2")

(* ── Suite ────────────────────────────────────────────────────── *)

let () =
  run
    "Modal_helpers"
    [
      ( "first_nonempty_line",
        [
          test_case "empty list" `Quick test_first_nonempty_empty_list;
          test_case "all blank" `Quick test_first_nonempty_all_blank;
          test_case
            "first is content"
            `Quick
            test_first_nonempty_first_is_content;
          test_case "skip blanks" `Quick test_first_nonempty_skip_blanks;
          test_case
            "whitespace only"
            `Quick
            test_first_nonempty_whitespace_only_not_empty;
        ] );
      ( "wrap_text",
        [
          test_case "short line" `Quick test_wrap_short_line;
          test_case "exact width" `Quick test_wrap_exact_width;
          test_case "long line" `Quick test_wrap_long_line;
          test_case "preserves newlines" `Quick test_wrap_preserves_newlines;
          test_case "empty string" `Quick test_wrap_empty_string;
          test_case "breaks at space" `Quick test_wrap_breaks_at_space;
          test_case "long word" `Quick test_wrap_long_word;
          test_case "newline and long" `Quick test_wrap_newline_and_long;
          test_case "multiple spaces" `Quick test_wrap_multiple_spaces;
        ] );
      ( "extract_major",
        [
          test_case "simple" `Quick test_extract_major_simple;
          test_case "three parts" `Quick test_extract_major_three_parts;
          test_case "single number" `Quick test_extract_major_single;
          test_case "empty" `Quick test_extract_major_empty;
          test_case "malformed" `Quick test_extract_major_malformed;
          test_case "leading v" `Quick test_extract_major_leading_v;
          test_case "zero major" `Quick test_extract_major_zero;
        ] );
    ]
