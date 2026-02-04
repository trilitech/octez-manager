(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Fuzz tests for core parsers with adversarial generators.

    Uses QCheck with generators biased toward parser-relevant characters
    (quotes, backslashes, [$], [{}], null bytes, newlines) and high iteration
    counts (10 000 by default, configurable via [FUZZ_COUNT] env var).

    Targets:
    - {!Env_file_parser}: quoting, comments, variable expansion
    - {!Execstart_parser}: shell quoting, recursive descent
    - {!Teztnets}: JSON parsing with fallback paths
    - {!Snapshots}: string processing, user input sanitization *)

open Octez_manager_lib

(* ============================================================ *)
(* Configuration                                                 *)
(* ============================================================ *)

let fuzz_count =
  match Sys.getenv_opt "FUZZ_COUNT" with
  | Some s -> ( try int_of_string s with Failure _ -> 10_000)
  | None -> 10_000

(* ============================================================ *)
(* Adversarial generators                                        *)
(* ============================================================ *)

(** Characters that stress parser edge cases. *)
let adversarial_chars =
  [|
    '"';
    '\'';
    '\\';
    '=';
    '$';
    '{';
    '}';
    '#';
    '\n';
    '\r';
    '\t';
    ' ';
    '\000';
    '/';
    '-';
    ';';
    '|';
    '&';
    '`';
    ':';
  |]

(** Generator that produces bytes biased toward [adversarial_chars]. *)
let adversarial_char_gen : char QCheck.Gen.t =
  QCheck.Gen.oneof_weighted
    [
      (* 60 % adversarial chars *)
      ( 60,
        QCheck.Gen.map
          (fun i -> adversarial_chars.(i))
          (QCheck.Gen.int_range 0 (Array.length adversarial_chars - 1)) );
      (* 30 % printable ASCII *)
      (30, QCheck.Gen.char_range ' ' '~');
      (* 10 % arbitrary byte *)
      (10, QCheck.Gen.char);
    ]

(** Adversarial string generator (0 to 512 bytes). *)
let adversarial_string : string QCheck.arbitrary =
  QCheck.make
    ~print:QCheck.Print.string
    ~shrink:QCheck.Shrink.string
    (QCheck.Gen.string_size
       ~gen:adversarial_char_gen
       (QCheck.Gen.int_range 0 512))

(** Multi-line adversarial string (newlines more likely). *)
let adversarial_multiline : string QCheck.arbitrary =
  let line_gen =
    QCheck.Gen.string_size ~gen:adversarial_char_gen (QCheck.Gen.int_range 0 80)
  in
  QCheck.make
    ~print:QCheck.Print.string
    ~shrink:QCheck.Shrink.string
    (QCheck.Gen.map
       (fun lines -> String.concat "\n" lines)
       (QCheck.Gen.list_size (QCheck.Gen.int_range 0 20) line_gen))

(** Adversarial JSON-ish string (braces, brackets, colons more likely). *)
let adversarial_json : string QCheck.arbitrary =
  let json_chars =
    [|
      '{';
      '}';
      '[';
      ']';
      ':';
      ',';
      '"';
      '\\';
      'n';
      'u';
      'l';
      't';
      'r';
      'e';
      'f';
      'a';
      's';
      '0';
      '1';
      ' ';
      '\n';
    |]
  in
  let json_char_gen =
    QCheck.Gen.oneof_weighted
      [
        ( 70,
          QCheck.Gen.map
            (fun i -> json_chars.(i))
            (QCheck.Gen.int_range 0 (Array.length json_chars - 1)) );
        (20, QCheck.Gen.char_range ' ' '~');
        (10, QCheck.Gen.char);
      ]
  in
  QCheck.make
    ~print:QCheck.Print.string
    ~shrink:QCheck.Shrink.string
    (QCheck.Gen.string_size ~gen:json_char_gen (QCheck.Gen.int_range 0 1024))

(* ============================================================ *)
(* Env_file_parser fuzz properties                               *)
(* ============================================================ *)

let fuzz_parse_string_no_crash =
  QCheck.Test.make
    ~name:"fuzz: Env_file_parser.parse_string never crashes"
    ~count:fuzz_count
    adversarial_multiline
    (fun s ->
      let _result = Env_file_parser.parse_string s in
      true)

let fuzz_parse_string_keys_no_newlines =
  QCheck.Test.make
    ~name:"fuzz: parsed env keys contain no newlines"
    ~count:fuzz_count
    adversarial_multiline
    (fun s ->
      let pairs = Env_file_parser.parse_string s in
      List.for_all (fun (k, _v) -> not (String.contains k '\n')) pairs)

let fuzz_expand_vars_no_crash =
  QCheck.Test.make
    ~name:"fuzz: Env_file_parser.expand_vars never crashes"
    ~count:fuzz_count
    QCheck.(
      pair
        (list (pair adversarial_string adversarial_string))
        adversarial_string)
    (fun (env, s) ->
      let env = List.map (fun (k, v) -> (k, v)) env in
      let _result = Env_file_parser.expand_vars ~env s in
      true)

(* ============================================================ *)
(* Execstart_parser fuzz properties                              *)
(* ============================================================ *)

let fuzz_execstart_parse_no_crash =
  QCheck.Test.make
    ~name:"fuzz: Execstart_parser.parse never crashes"
    ~count:fuzz_count
    adversarial_string
    (fun s ->
      let _result = Execstart_parser.parse s in
      true)

let fuzz_unwrap_shell_no_crash =
  QCheck.Test.make
    ~name:"fuzz: Execstart_parser.unwrap_shell never crashes"
    ~count:fuzz_count
    adversarial_string
    (fun s ->
      let _result = Execstart_parser.unwrap_shell s in
      true)

let fuzz_unwrap_shell_no_growth =
  QCheck.Test.make
    ~name:"fuzz: unwrap_shell output is never longer than input"
    ~count:fuzz_count
    adversarial_string
    (fun s ->
      let result = Execstart_parser.unwrap_shell s in
      String.length result <= String.length s)

let fuzz_extract_binary_path_no_crash =
  QCheck.Test.make
    ~name:"fuzz: Execstart_parser.extract_binary_path never crashes"
    ~count:fuzz_count
    adversarial_string
    (fun s ->
      let _result = Execstart_parser.extract_binary_path s in
      true)

let fuzz_unwrap_shell_idempotent =
  QCheck.Test.make
    ~name:"fuzz: unwrap_shell is idempotent on adversarial input"
    ~count:fuzz_count
    adversarial_string
    (fun s ->
      let once = Execstart_parser.unwrap_shell s in
      let twice = Execstart_parser.unwrap_shell once in
      String.equal once twice)

(* ============================================================ *)
(* Teztnets fuzz properties                                      *)
(* ============================================================ *)

let fuzz_parse_networks_no_crash =
  QCheck.Test.make
    ~name:"fuzz: Teztnets.parse_networks never crashes"
    ~count:fuzz_count
    adversarial_json
    (fun s ->
      let _result = Teztnets.parse_networks s in
      true)

let fuzz_parse_networks_result_valid =
  QCheck.Test.make
    ~name:"fuzz: Teztnets.parse_networks returns Ok or Error, never raises"
    ~count:fuzz_count
    adversarial_json
    (fun s ->
      match Teztnets.parse_networks s with
      | Ok networks ->
          (* If parsing succeeds, every network must have a non-empty alias *)
          List.for_all
            (fun (n : Teztnets.network_info) -> n.alias <> "")
            networks
      | Error _ -> true)

(* ============================================================ *)
(* Snapshots fuzz properties                                     *)
(* ============================================================ *)

let fuzz_slug_of_network_no_crash =
  QCheck.Test.make
    ~name:"fuzz: Snapshots.slug_of_network never crashes"
    ~count:fuzz_count
    adversarial_string
    (fun s ->
      let _result = Snapshots.slug_of_network s in
      true)

let fuzz_sanitize_kind_no_crash =
  QCheck.Test.make
    ~name:"fuzz: Snapshots.sanitize_kind_input never crashes"
    ~count:fuzz_count
    adversarial_string
    (fun s ->
      let _result = Snapshots.sanitize_kind_input s in
      true)

let fuzz_sanitize_kind_output_clean =
  QCheck.Test.make
    ~name:"fuzz: sanitize_kind_input output has no colons or spaces"
    ~count:fuzz_count
    adversarial_string
    (fun s ->
      match Snapshots.sanitize_kind_input s with
      | None -> true
      | Some slug ->
          (not (String.contains slug ':')) && not (String.contains slug ' '))

(* ============================================================ *)
(* Test Suite                                                    *)
(* ============================================================ *)

let fuzz_tests =
  List.map
    QCheck_alcotest.to_alcotest
    [
      (* Env_file_parser *)
      fuzz_parse_string_no_crash;
      fuzz_parse_string_keys_no_newlines;
      fuzz_expand_vars_no_crash;
      (* Execstart_parser *)
      fuzz_execstart_parse_no_crash;
      fuzz_unwrap_shell_no_crash;
      fuzz_unwrap_shell_no_growth;
      fuzz_extract_binary_path_no_crash;
      fuzz_unwrap_shell_idempotent;
      (* Teztnets *)
      fuzz_parse_networks_no_crash;
      fuzz_parse_networks_result_valid;
      (* Snapshots *)
      fuzz_slug_of_network_no_crash;
      fuzz_sanitize_kind_no_crash;
      fuzz_sanitize_kind_output_clean;
    ]

let () = Alcotest.run "Fuzz" [("fuzz", fuzz_tests)]
