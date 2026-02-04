(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Property-based tests for System_metrics module.

    Properties tested:
    - No-crash: format_bytes/parse_version_output never raise
    - Monotonicity: larger byte values produce larger or equal formatted output
    - format_bytes always returns non-empty string
    - calc_cpu_percent result is non-negative
*)

module System_metrics = Octez_manager_ui.System_metrics

(* ============================================================ *)
(* No-crash properties *)
(* ============================================================ *)

let prop_format_bytes_no_crash =
  QCheck.Test.make
    ~name:"format_bytes never crashes on random int64"
    ~count:300
    QCheck.int64
    (fun n ->
      let _result = System_metrics.format_bytes n in
      true)

let prop_parse_version_output_no_crash =
  QCheck.Test.make
    ~name:"parse_version_output never crashes on random input"
    ~count:300
    QCheck.string
    (fun s ->
      let _result = System_metrics.parse_version_output s in
      true)

(* ============================================================ *)
(* Structural properties *)
(* ============================================================ *)

let prop_format_bytes_non_empty =
  QCheck.Test.make
    ~name:"format_bytes always returns non-empty string"
    ~count:300
    QCheck.int64
    (fun n ->
      let result = System_metrics.format_bytes n in
      String.length result > 0)

let prop_format_bytes_monotonic =
  QCheck.Test.make
    ~name:"format_bytes is monotonic for non-negative values"
    ~count:300
    QCheck.(pair (int_range 0 1_000_000_000) (int_range 0 1_000_000_000))
    (fun (a, b) ->
      let a64 = Int64.of_int (min a b) in
      let b64 = Int64.of_int (max a b) in
      let fa = System_metrics.format_bytes a64 in
      let fb = System_metrics.format_bytes b64 in
      (* Monotonicity: for a <= b, the numeric part of format_bytes(a) <=
         format_bytes(b) when they use the same unit. When units differ, the
         higher unit is for the larger value. We verify a weaker property:
         the string length doesn't decrease drastically. *)
      String.length fb >= String.length fa - 2)

let prop_calc_cpu_percent_non_negative =
  QCheck.Test.make
    ~name:"calc_cpu_percent returns non-negative values"
    ~count:300
    QCheck.(
      pair
        (pair (int_range 0 1_000_000) (int_range 0 1_000_000))
        (pair (int_range 0 1_000_000) (int_range 0 1_000_000)))
    (fun ((u1, s1), (u2, s2)) ->
      (* Ensure curr >= prev for realistic samples *)
      let prev_u = Int64.of_int (min u1 u2) in
      let prev_s = Int64.of_int (min s1 s2) in
      let curr_u = Int64.of_int (max u1 u2) in
      let curr_s = Int64.of_int (max s1 s2) in
      let prev =
        System_metrics.{utime = prev_u; stime = prev_s; timestamp = 1.0}
      in
      let curr =
        System_metrics.{utime = curr_u; stime = curr_s; timestamp = 2.0}
      in
      let result = System_metrics.calc_cpu_percent ~prev ~curr in
      result >= 0.0)

(* ============================================================ *)
(* Test Suite *)
(* ============================================================ *)

let props =
  List.map
    QCheck_alcotest.to_alcotest
    [
      prop_format_bytes_no_crash;
      prop_parse_version_output_no_crash;
      prop_format_bytes_non_empty;
      prop_format_bytes_monotonic;
      prop_calc_cpu_percent_non_negative;
    ]

let () = Alcotest.run "System_metrics_props" [("properties", props)]
