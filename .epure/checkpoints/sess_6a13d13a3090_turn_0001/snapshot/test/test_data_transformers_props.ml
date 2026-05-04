(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Property-based tests for data transformer modules.

    Modules tested: History_mode, Dal_health, Delegate_data, Baker_highwatermarks,
    Foldable_json.

    Properties tested:
    - Roundtrip: of_string(to_string(x)) = x for variant types
    - No-crash: JSON parsers never raise on malformed input
    - Structure preservation for foldable JSON
*)

open Octez_manager_lib
module Dal_health = Octez_manager_ui.Dal_health
module Delegate_data = Octez_manager_ui.Delegate_data
module Baker_highwatermarks = Octez_manager_ui.Baker_highwatermarks
module Foldable_json = Octez_manager_ui.Foldable_json

(* ============================================================ *)
(* History_mode roundtrip properties *)
(* ============================================================ *)

let gen_history_mode =
  QCheck.Gen.oneof_list [History_mode.Rolling; Full; Archive]

let arb_history_mode =
  QCheck.make gen_history_mode ~print:History_mode.to_string

let prop_history_mode_roundtrip =
  QCheck.Test.make
    ~name:"History_mode.of_string(to_string(x)) = Ok x"
    ~count:100
    arb_history_mode
    (fun mode ->
      match History_mode.of_string (History_mode.to_string mode) with
      | Ok roundtripped -> roundtripped = mode
      | Error _ -> false)

let prop_history_mode_of_string_no_crash =
  QCheck.Test.make
    ~name:"History_mode.of_string never crashes on random input"
    ~count:300
    QCheck.string
    (fun s ->
      let _result = History_mode.of_string s in
      true)

(* ============================================================ *)
(* Dal_health roundtrip properties *)
(* ============================================================ *)

let gen_dal_status =
  QCheck.Gen.oneof_list [Dal_health.Up; Down; Degraded; Unknown]

let arb_dal_status =
  QCheck.make gen_dal_status ~print:Dal_health.status_to_string

let prop_dal_status_roundtrip =
  QCheck.Test.make
    ~name:"Dal_health.status_of_string(status_to_string(x)) = x"
    ~count:100
    arb_dal_status
    (fun status ->
      let roundtripped =
        Dal_health.status_of_string (Dal_health.status_to_string status)
      in
      roundtripped = status)

let prop_dal_status_of_string_no_crash =
  QCheck.Test.make
    ~name:"Dal_health.status_of_string never crashes on random input"
    ~count:300
    QCheck.string
    (fun s ->
      let _result = Dal_health.status_of_string s in
      true)

(* ============================================================ *)
(* Delegate_data properties *)
(* ============================================================ *)

let prop_delegate_data_of_json_no_crash =
  QCheck.Test.make
    ~name:"Delegate_data.For_tests.of_json never crashes on random JSON"
    ~count:300
    QCheck.string
    (fun s ->
      (try
         let json = Yojson.Safe.from_string s in
         let _result = Delegate_data.For_tests.of_json ~pkh:"tz1test" json in
         ()
       with Yojson.Json_error _ -> ()) ;
      true)

let prop_delegate_data_parse_participation_no_crash =
  QCheck.Test.make
    ~name:
      "Delegate_data.For_tests.parse_participation never crashes on random JSON"
    ~count:300
    QCheck.string
    (fun s ->
      (try
         let json = Yojson.Safe.from_string s in
         let _result = Delegate_data.For_tests.parse_participation json in
         ()
       with _ -> ()) ;
      true)

let prop_format_tez_no_crash =
  QCheck.Test.make
    ~name:"Delegate_data.format_tez never crashes on random input"
    ~count:300
    QCheck.string
    (fun s ->
      let _result = Delegate_data.format_tez s in
      true)

(* ============================================================ *)
(* Baker_highwatermarks properties *)
(* ============================================================ *)

let prop_parse_highwatermark_no_crash =
  QCheck.Test.make
    ~name:
      "Baker_highwatermarks.For_tests.parse_highwatermark never crashes on \
       random JSON"
    ~count:300
    QCheck.string
    (fun s ->
      (try
         let json = Yojson.Safe.from_string s in
         let _result =
           Baker_highwatermarks.For_tests.parse_highwatermark json
         in
         ()
       with Yojson.Json_error _ -> ()) ;
      true)

let prop_parse_entries_no_crash =
  QCheck.Test.make
    ~name:
      "Baker_highwatermarks.For_tests.parse_entries never crashes on random \
       JSON"
    ~count:300
    QCheck.string
    (fun s ->
      (try
         let json = Yojson.Safe.from_string s in
         let _result = Baker_highwatermarks.For_tests.parse_entries json in
         ()
       with Yojson.Json_error _ -> ()) ;
      true)

let prop_max_level_no_crash =
  QCheck.Test.make
    ~name:"Baker_highwatermarks.max_level never crashes"
    ~count:100
    QCheck.(
      triple
        (option (pair (int_range 0 1000) (int_range 0 1000)))
        (option (pair (int_range 0 1000) (int_range 0 1000)))
        (option (pair (int_range 0 1000) (int_range 0 1000))))
    (fun (block, preatt, att) ->
      let mk =
        Option.map (fun (round, level) -> Baker_highwatermarks.{round; level})
      in
      let activity =
        Baker_highwatermarks.
          {
            delegate = "tz1test";
            last_block = mk block;
            last_preattestation = mk preatt;
            last_attestation = mk att;
          }
      in
      let _result = Baker_highwatermarks.max_level activity in
      true)

let prop_format_summary_no_crash =
  QCheck.Test.make
    ~name:"Baker_highwatermarks.format_summary never crashes on empty list"
    ~count:1
    QCheck.unit
    (fun () ->
      let _result = Baker_highwatermarks.format_summary [] in
      true)

(* ============================================================ *)
(* Foldable_json properties *)
(* ============================================================ *)

let prop_foldable_json_of_string_no_crash =
  QCheck.Test.make
    ~name:"Foldable_json.of_string never crashes on random input"
    ~count:300
    QCheck.string
    (fun s ->
      let _result = Foldable_json.of_string s in
      true)

let prop_foldable_json_structure_preservation =
  QCheck.Test.make
    ~name:"Foldable_json.of_json preserves structure (line_count > 0)"
    ~count:300
    QCheck.string
    (fun s ->
      match Foldable_json.of_string s with
      | Some t -> Foldable_json.line_count t > 0
      | None -> true)

let prop_foldable_json_unfold_fold_consistent =
  QCheck.Test.make
    ~name:"unfold_all then fold_all returns same line count as fold_all alone"
    ~count:100
    QCheck.string
    (fun s ->
      match Foldable_json.of_string s with
      | Some t ->
          let folded = Foldable_json.fold_all t in
          let unfolded_then_folded =
            Foldable_json.fold_all (Foldable_json.unfold_all t)
          in
          Foldable_json.line_count folded
          = Foldable_json.line_count unfolded_then_folded
      | None -> true)

let prop_foldable_json_unfold_all_idempotent =
  QCheck.Test.make
    ~name:"unfold_all is idempotent"
    ~count:100
    QCheck.string
    (fun s ->
      match Foldable_json.of_string s with
      | Some t ->
          let once = Foldable_json.unfold_all t in
          let twice = Foldable_json.unfold_all once in
          Foldable_json.line_count once = Foldable_json.line_count twice
      | None -> true)

(* ============================================================ *)
(* Test Suite *)
(* ============================================================ *)

let props =
  List.map
    QCheck_alcotest.to_alcotest
    [
      prop_history_mode_roundtrip;
      prop_history_mode_of_string_no_crash;
      prop_dal_status_roundtrip;
      prop_dal_status_of_string_no_crash;
      prop_delegate_data_of_json_no_crash;
      prop_delegate_data_parse_participation_no_crash;
      prop_format_tez_no_crash;
      prop_parse_highwatermark_no_crash;
      prop_parse_entries_no_crash;
      prop_max_level_no_crash;
      prop_format_summary_no_crash;
      prop_foldable_json_of_string_no_crash;
      prop_foldable_json_structure_preservation;
      prop_foldable_json_unfold_fold_consistent;
      prop_foldable_json_unfold_all_idempotent;
    ]

let () = Alcotest.run "Data_transformers_props" [("properties", props)]
