(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Unit tests for System_metrics pure functions. *)

open Alcotest
module SM = Octez_manager_ui.System_metrics

(* ── calc_cpu_percent ────────────────────────────────────────── *)

let make_sample ~utime ~stime ~timestamp = SM.{utime; stime; timestamp}

let test_cpu_normal () =
  let prev = make_sample ~utime:100L ~stime:50L ~timestamp:1000.0 in
  let curr = make_sample ~utime:200L ~stime:100L ~timestamp:1001.0 in
  let pct = SM.calc_cpu_percent ~prev ~curr in
  (* 150 ticks in 1s = 150 ticks / 100 ticks_per_sec = 150% *)
  check (float 0.1) "150%" 150.0 pct

let test_cpu_zero_delta_time () =
  let prev = make_sample ~utime:100L ~stime:50L ~timestamp:1000.0 in
  let curr = make_sample ~utime:200L ~stime:100L ~timestamp:1000.0 in
  check (float 0.01) "zero delta" 0.0 (SM.calc_cpu_percent ~prev ~curr)

let test_cpu_negative_delta_time () =
  let prev = make_sample ~utime:100L ~stime:50L ~timestamp:1001.0 in
  let curr = make_sample ~utime:200L ~stime:100L ~timestamp:1000.0 in
  check (float 0.01) "negative delta" 0.0 (SM.calc_cpu_percent ~prev ~curr)

let test_cpu_no_ticks () =
  let prev = make_sample ~utime:100L ~stime:50L ~timestamp:1000.0 in
  let curr = make_sample ~utime:100L ~stime:50L ~timestamp:1001.0 in
  check (float 0.01) "no ticks" 0.0 (SM.calc_cpu_percent ~prev ~curr)

let test_cpu_half_core () =
  let prev = make_sample ~utime:0L ~stime:0L ~timestamp:0.0 in
  let curr = make_sample ~utime:50L ~stime:0L ~timestamp:1.0 in
  check (float 0.1) "50%" 50.0 (SM.calc_cpu_percent ~prev ~curr)

let test_cpu_multi_core () =
  let prev = make_sample ~utime:0L ~stime:0L ~timestamp:0.0 in
  let curr = make_sample ~utime:300L ~stime:100L ~timestamp:1.0 in
  (* 400 ticks / 100 = 400% = 4 cores fully loaded *)
  check (float 0.1) "400%" 400.0 (SM.calc_cpu_percent ~prev ~curr)

let test_cpu_small_interval () =
  let prev = make_sample ~utime:1000L ~stime:500L ~timestamp:0.0 in
  let curr = make_sample ~utime:1010L ~stime:505L ~timestamp:0.1 in
  (* 15 ticks in 0.1s = 15 / 10 = 150% *)
  check (float 0.1) "small interval" 150.0 (SM.calc_cpu_percent ~prev ~curr)

(* ── format_bytes ────────────────────────────────────────────── *)

let test_format_bytes_zero () = check string "0B" "0B" (SM.format_bytes 0L)

let test_format_bytes_small () =
  check string "512B" "512B" (SM.format_bytes 512L)

let test_format_bytes_kb () = check string "1K" "1K" (SM.format_bytes 1024L)

let test_format_bytes_mb () = check string "1M" "1M" (SM.format_bytes 1048576L)

let test_format_bytes_gb () =
  check string "1.0G" "1.0G" (SM.format_bytes 1073741824L)

let test_format_bytes_tb () =
  check string "1.0T" "1.0T" (SM.format_bytes 1099511627776L)

let test_format_bytes_large_gb () =
  (* 50 GB *)
  check string "50.0G" "50.0G" (SM.format_bytes (Int64.mul 50L 1073741824L))

let test_format_bytes_500mb () =
  check string "500M" "500M" (SM.format_bytes (Int64.mul 500L 1048576L))

(* ── parse_version_output ────────────────────────────────────── *)

let test_parse_version_simple () =
  check
    (option string)
    "21.0"
    (Some "21.0")
    (SM.parse_version_output "Octez 21.0")

let test_parse_version_multiline () =
  let output = "octez-node\nversion: Octez 21.0\nbuild info" in
  check
    (option string)
    "multiline"
    (Some "21.0")
    (SM.parse_version_output output)

let test_parse_version_missing () =
  check
    (option string)
    "missing"
    None
    (SM.parse_version_output "no version here")

let test_parse_version_empty () =
  check (option string) "empty" None (SM.parse_version_output "")

let test_parse_version_partial () =
  check (option string) "partial" None (SM.parse_version_output "Octez beta")

let test_parse_version_with_extra () =
  let output = "Octez 20.3 (abcdef12)" in
  check
    (option string)
    "with extra"
    (Some "20.3")
    (SM.parse_version_output output)

(* ── Suite ───────────────────────────────────────────────────── *)

let () =
  run
    "System_metrics"
    [
      ( "calc_cpu_percent",
        [
          test_case "normal" `Quick test_cpu_normal;
          test_case "zero delta time" `Quick test_cpu_zero_delta_time;
          test_case "negative delta" `Quick test_cpu_negative_delta_time;
          test_case "no ticks" `Quick test_cpu_no_ticks;
          test_case "half core" `Quick test_cpu_half_core;
          test_case "multi core" `Quick test_cpu_multi_core;
          test_case "small interval" `Quick test_cpu_small_interval;
        ] );
      ( "format_bytes",
        [
          test_case "zero" `Quick test_format_bytes_zero;
          test_case "small" `Quick test_format_bytes_small;
          test_case "KB" `Quick test_format_bytes_kb;
          test_case "MB" `Quick test_format_bytes_mb;
          test_case "GB" `Quick test_format_bytes_gb;
          test_case "TB" `Quick test_format_bytes_tb;
          test_case "large GB" `Quick test_format_bytes_large_gb;
          test_case "500MB" `Quick test_format_bytes_500mb;
        ] );
      ( "parse_version_output",
        [
          test_case "simple" `Quick test_parse_version_simple;
          test_case "multiline" `Quick test_parse_version_multiline;
          test_case "missing" `Quick test_parse_version_missing;
          test_case "empty" `Quick test_parse_version_empty;
          test_case "partial" `Quick test_parse_version_partial;
          test_case "with extra" `Quick test_parse_version_with_extra;
        ] );
    ]
