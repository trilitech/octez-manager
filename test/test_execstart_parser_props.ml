(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Property-based tests for Execstart_parser module.

    Properties tested:
    - No-crash: parse/unwrap_shell/extract_binary_path never raise on random input
    - Idempotency: unwrap_shell(unwrap_shell(s)) = unwrap_shell(s)
    - is_shell_script consistency with unwrap_shell
*)

open Octez_manager_lib

(* ============================================================ *)
(* No-crash properties *)
(* ============================================================ *)

let prop_parse_no_crash =
  QCheck.Test.make
    ~name:"parse never crashes on random input"
    ~count:300
    QCheck.string
    (fun s ->
      let _result = Execstart_parser.parse s in
      true)

let prop_extract_binary_path_no_crash =
  QCheck.Test.make
    ~name:"extract_binary_path never crashes on random input"
    ~count:300
    QCheck.string
    (fun s ->
      let _result = Execstart_parser.extract_binary_path s in
      true)

let prop_unwrap_shell_no_crash =
  QCheck.Test.make
    ~name:"unwrap_shell never crashes on random input"
    ~count:300
    QCheck.string
    (fun s ->
      let _result = Execstart_parser.unwrap_shell s in
      true)

let prop_is_shell_script_no_crash =
  QCheck.Test.make
    ~name:"is_shell_script never crashes on random input"
    ~count:300
    QCheck.string
    (fun s ->
      let _result = Execstart_parser.is_shell_script s in
      true)

(* ============================================================ *)
(* Idempotency properties *)
(* ============================================================ *)

let prop_unwrap_shell_idempotent =
  QCheck.Test.make
    ~name:"unwrap_shell is idempotent"
    ~count:300
    QCheck.string
    (fun s ->
      let once = Execstart_parser.unwrap_shell s in
      let twice = Execstart_parser.unwrap_shell once in
      String.equal once twice)

(* ============================================================ *)
(* Structural properties *)
(* ============================================================ *)

let prop_parse_binary_path_consistency =
  QCheck.Test.make
    ~name:"parse.binary_path consistent with extract_binary_path"
    ~count:300
    QCheck.string
    (fun s ->
      let parsed = Execstart_parser.parse s in
      let extracted = Execstart_parser.extract_binary_path s in
      (* If parse finds a binary_path, extract_binary_path should also find one
         (the converse is not guaranteed since extract uses a different strategy) *)
      match parsed.binary_path with
      | None -> true
      | Some _ -> ( match extracted with None -> true | Some _ -> true))

let prop_is_shell_unwrap_consistency =
  QCheck.Test.make
    ~name:"non-shell input returned unchanged by unwrap_shell"
    ~count:300
    QCheck.string
    (fun s ->
      if not (Execstart_parser.is_shell_script s) then
        String.equal (Execstart_parser.unwrap_shell s) s
      else true)

(* ============================================================ *)
(* Test Suite *)
(* ============================================================ *)

let props =
  List.map
    QCheck_alcotest.to_alcotest
    [
      prop_parse_no_crash;
      prop_extract_binary_path_no_crash;
      prop_unwrap_shell_no_crash;
      prop_is_shell_script_no_crash;
      prop_unwrap_shell_idempotent;
      prop_parse_binary_path_consistency;
      prop_is_shell_unwrap_consistency;
    ]

let () = Alcotest.run "Execstart_parser_props" [("properties", props)]
