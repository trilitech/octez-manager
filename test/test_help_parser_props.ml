(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Property-based tests for Help_parser module.

    Properties tested:
    - No-crash: strip_ansi/parse_spec/classify_arg_kind never raise
    - Idempotency: strip_ansi(strip_ansi(s)) = strip_ansi(s)
    - classify_arg_kind determinism
    - strip_ansi output contains no ESC bytes
*)

open Octez_manager_lib

(* ============================================================ *)
(* No-crash properties *)
(* ============================================================ *)

let prop_strip_ansi_no_crash =
  QCheck.Test.make
    ~name:"strip_ansi never crashes on random input"
    ~count:300
    QCheck.string
    (fun s ->
      let _result = Help_parser.strip_ansi s in
      true)

let prop_parse_spec_no_crash =
  QCheck.Test.make
    ~name:"parse_spec never crashes on random input"
    ~count:300
    QCheck.string
    (fun s ->
      let _result = Help_parser.parse_spec s in
      true)

let prop_classify_arg_kind_no_crash =
  QCheck.Test.make
    ~name:"classify_arg_kind never crashes on random input"
    ~count:300
    QCheck.(triple (list string) (option string) string)
    (fun (names, arg, doc) ->
      let _result = Help_parser.classify_arg_kind ~names ~arg ~doc in
      true)

let prop_parse_help_node_no_crash =
  QCheck.Test.make
    ~name:"parse_help_node never crashes on random input"
    ~count:300
    QCheck.string
    (fun s ->
      let _result = Help_parser.parse_help_node s in
      true)

(* ============================================================ *)
(* Idempotency properties *)
(* ============================================================ *)

let prop_strip_ansi_idempotent =
  QCheck.Test.make
    ~name:"strip_ansi is idempotent"
    ~count:300
    QCheck.string
    (fun s ->
      let once = Help_parser.strip_ansi s in
      let twice = Help_parser.strip_ansi once in
      String.equal once twice)

(* ============================================================ *)
(* Structural properties *)
(* ============================================================ *)

let prop_strip_ansi_no_esc =
  QCheck.Test.make
    ~name:"strip_ansi output contains no ESC bytes"
    ~count:300
    QCheck.string
    (fun s ->
      let result = Help_parser.strip_ansi s in
      not (String.contains result '\027'))

let prop_classify_deterministic =
  QCheck.Test.make
    ~name:"classify_arg_kind is deterministic"
    ~count:300
    QCheck.(triple (list string) (option string) string)
    (fun (names, arg, doc) ->
      let r1 = Help_parser.classify_arg_kind ~names ~arg ~doc in
      let r2 = Help_parser.classify_arg_kind ~names ~arg ~doc in
      r1 = r2)

let prop_classify_none_is_toggle =
  QCheck.Test.make
    ~name:"classify_arg_kind with arg=None always returns Toggle"
    ~count:300
    QCheck.(pair (list string) string)
    (fun (names, doc) ->
      Help_parser.classify_arg_kind ~names ~arg:None ~doc = Help_parser.Toggle)

let prop_contains_reflexive =
  QCheck.Test.make
    ~name:"contains is reflexive: s always contains itself"
    ~count:300
    QCheck.string
    (fun s -> Help_parser.contains ~needle:s s)

let prop_contains_empty_needle =
  QCheck.Test.make
    ~name:"contains with empty needle is always true"
    ~count:300
    QCheck.string
    (fun s -> Help_parser.contains ~needle:"" s)

(* ============================================================ *)
(* Test Suite *)
(* ============================================================ *)

let props =
  List.map
    QCheck_alcotest.to_alcotest
    [
      prop_strip_ansi_no_crash;
      prop_parse_spec_no_crash;
      prop_classify_arg_kind_no_crash;
      prop_parse_help_node_no_crash;
      prop_strip_ansi_idempotent;
      prop_strip_ansi_no_esc;
      prop_classify_deterministic;
      prop_classify_none_is_toggle;
      prop_contains_reflexive;
      prop_contains_empty_needle;
    ]

let () = Alcotest.run "Help_parser_props" [("properties", props)]
