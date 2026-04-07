(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Tests for Pkh_validator module

    Tests cover:
    - Valid PKH format for tz1-tz4 prefixes
    - Invalid prefix rejection
    - Wrong length rejection
    - Invalid base58 character rejection
    - Whitespace trimming
*)

open Alcotest
module PV = Octez_manager_ui.Pkh_validator

let validation_result_testable =
  let pp fmt = function
    | PV.Valid -> Format.fprintf fmt "Valid"
    | PV.Invalid reason -> Format.fprintf fmt "Invalid(%s)" reason
  in
  let eq a b =
    match (a, b) with
    | PV.Valid, PV.Valid -> true
    | PV.Invalid _, PV.Invalid _ -> true
    | _ -> false
  in
  testable pp eq

let check_valid msg pkh =
  check validation_result_testable msg Valid (PV.validate_format pkh)

let check_invalid msg pkh =
  match PV.validate_format pkh with
  | PV.Invalid _ -> check bool msg true true
  | PV.Valid -> fail (Printf.sprintf "%s: expected Invalid" msg)

(* ============================================================ *)
(* Valid PKH Tests *)
(* ============================================================ *)

let test_valid_tz1 () =
  check_valid "tz1 address" "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"

let test_valid_tz2 () =
  (* 36 chars: tz2(3) + 33 base58 chars *)
  check_valid "tz2 address" "tz2AaBbCcDdEeFfGgHhJjKkMmNnPpQqRrSsT"

let test_valid_tz3 () =
  check_valid "tz3 address" "tz3AaBbCcDdEeFfGgHhJjKkMmNnPpQqRrSsT"

let test_valid_tz4 () =
  check_valid "tz4 address" "tz4AaBbCcDdEeFfGgHhJjKkMmNnPpQqRrSsT"

let test_valid_with_whitespace () =
  check_valid "trimmed whitespace" "  tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx  "

let test_valid_with_nbsp () =
  (* U+00A0 non-breaking space (\xc2\xa0 in UTF-8) — common when copying from
     browsers or rich-text editors *)
  check_valid
    "non-breaking space stripped"
    "\xc2\xa0tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx\xc2\xa0"

let test_valid_with_bom () =
  (* U+FEFF BOM / zero-width no-break space (\xef\xbb\xbf in UTF-8) —
     sometimes prepended when copying from Windows applications *)
  check_valid "BOM stripped" "\xef\xbb\xbftz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"

let test_valid_with_zwsp () =
  (* U+200B ZERO-WIDTH SPACE (\xe2\x80\x8b in UTF-8) — injected by some
     web UIs between characters *)
  check_valid
    "zero-width space stripped"
    ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" ^ "\xe2\x80\x8b")

let test_valid_with_mixed_unicode () =
  (* BOM at start + NBSP at end — realistic tzkt copy-paste scenario *)
  check_valid
    "BOM + NBSP mixed stripped"
    ("\xef\xbb\xbf" ^ "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" ^ "\xc2\xa0")

(* ============================================================ *)
(* Invalid PKH Tests *)
(* ============================================================ *)

let test_invalid_empty () = check_invalid "empty string" ""

let test_invalid_too_short () = check_invalid "too short" "tz1abc"

let test_invalid_too_long () =
  check_invalid "too long" "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSxXX"

let test_invalid_wrong_prefix () =
  check_invalid "wrong prefix" "KT1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"

let test_invalid_tz0 () =
  check_invalid "tz0 not valid" "tz0KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"

let test_invalid_tz5 () =
  check_invalid "tz5 not valid" "tz5KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"

let test_invalid_base58_chars () =
  (* 0, O, I, l are not in base58 alphabet *)
  check_invalid "invalid base58 char 0" "tz10qTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"

let test_invalid_base58_O () =
  check_invalid "invalid base58 char O" "tz1OqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"

let test_invalid_base58_I () =
  check_invalid "invalid base58 char I" "tz1IqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"

let test_invalid_base58_l () =
  check_invalid "invalid base58 char l" "tz1lqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"

(* ============================================================ *)
(* Test Suite *)
(* ============================================================ *)

let valid_tests =
  [
    ("valid tz1", `Quick, test_valid_tz1);
    ("valid tz2", `Quick, test_valid_tz2);
    ("valid tz3", `Quick, test_valid_tz3);
    ("valid tz4", `Quick, test_valid_tz4);
    ("valid with whitespace", `Quick, test_valid_with_whitespace);
    ("valid with non-breaking space", `Quick, test_valid_with_nbsp);
    ("valid with BOM", `Quick, test_valid_with_bom);
    ("valid with zero-width space", `Quick, test_valid_with_zwsp);
    ("valid with mixed unicode", `Quick, test_valid_with_mixed_unicode);
  ]

let invalid_tests =
  [
    ("empty string", `Quick, test_invalid_empty);
    ("too short", `Quick, test_invalid_too_short);
    ("too long", `Quick, test_invalid_too_long);
    ("wrong prefix (KT1)", `Quick, test_invalid_wrong_prefix);
    ("tz0 not valid", `Quick, test_invalid_tz0);
    ("tz5 not valid", `Quick, test_invalid_tz5);
    ("invalid base58 char 0", `Quick, test_invalid_base58_chars);
    ("invalid base58 char O", `Quick, test_invalid_base58_O);
    ("invalid base58 char I", `Quick, test_invalid_base58_I);
    ("invalid base58 char l", `Quick, test_invalid_base58_l);
  ]

let () =
  Alcotest.run
    "Pkh_validator"
    [("valid_pkh", valid_tests); ("invalid_pkh", invalid_tests)]
