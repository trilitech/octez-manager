(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Tests for Charts module.

    Covers trim_chart_padding, render_bg_queue_chart,
    render_service_status_chart, render_latency_chart,
    render_key_to_render_chart, render_summary_bars. *)

open Alcotest
module Charts = Octez_manager_ui.Charts
module Metrics = Octez_manager_ui.Metrics

(* ── Helpers ─────────────────────────────────────────────────── *)

let make_snapshot ?(timestamp = 0.0) ?(bg_queue_depth = 0) ?(bg_queue_max = 0)
    ?(services_active = 0) ?(services_total = 0) ?render_p50 ?render_p90
    ?render_p99 ?key_to_render_p50 ?key_to_render_p90 ?bg_wait_p50 ?bg_wait_p90
    () : Metrics.metrics_snapshot =
  {
    timestamp;
    bg_queue_depth;
    bg_queue_max;
    services_active;
    services_total;
    render_p50;
    render_p90;
    render_p99;
    key_to_render_p50;
    key_to_render_p90;
    bg_wait_p50;
    bg_wait_p90;
  }

let contains s sub =
  try
    ignore (Str.search_forward (Str.regexp_string sub) s 0) ;
    true
  with Not_found -> false

(* ── trim_chart_padding ─────────────────────────────────────── *)

let test_trim_empty () = check string "empty" "" (Charts.trim_chart_padding "")

let test_trim_no_trailing () =
  check string "unchanged" "hello" (Charts.trim_chart_padding "hello")

let test_trim_trailing_spaces () =
  check string "trimmed" "hello" (Charts.trim_chart_padding "hello   ")

let test_trim_multiline () =
  let input = "line1   \nline2  \nline3" in
  let result = Charts.trim_chart_padding input in
  let lines = String.split_on_char '\n' result in
  check int "three lines" 3 (List.length lines) ;
  check string "first" "line1" (List.nth lines 0) ;
  check string "second" "line2" (List.nth lines 1)

let test_trim_all_spaces () =
  check string "empty lines" "\n" (Charts.trim_chart_padding "   \n   ")

(* ── render_bg_queue_chart ──────────────────────────────────── *)

let test_bg_queue_empty () =
  let r = Charts.render_bg_queue_chart [] ~width:40 ~height:10 in
  check bool "no data msg" true (contains r "No data")

let test_bg_queue_with_data () =
  let samples =
    [
      make_snapshot ~bg_queue_depth:2 ~timestamp:0.0 ();
      make_snapshot ~bg_queue_depth:7 ~timestamp:1.0 ();
      make_snapshot ~bg_queue_depth:3 ~timestamp:2.0 ();
    ]
  in
  let r = Charts.render_bg_queue_chart samples ~width:40 ~height:10 in
  check bool "has Current" true (contains r "Current:") ;
  check bool "has Avg" true (contains r "Avg:") ;
  check bool "has Max" true (contains r "Max:")

let test_bg_queue_single_sample () =
  let samples = [make_snapshot ~bg_queue_depth:5 ~timestamp:0.0 ()] in
  let r = Charts.render_bg_queue_chart samples ~width:40 ~height:10 in
  check bool "renders" true (String.length r > 0)

(* ── render_service_status_chart ────────────────────────────── *)

let test_service_status_empty () =
  let r = Charts.render_service_status_chart [] ~width:40 ~height:10 in
  check bool "no data msg" true (contains r "No data")

let test_service_status_with_data () =
  let samples =
    [
      make_snapshot ~services_active:3 ~services_total:5 ();
      make_snapshot ~services_active:4 ~services_total:5 ();
    ]
  in
  let r = Charts.render_service_status_chart samples ~width:40 ~height:10 in
  check bool "renders" true (String.length r > 0)

(* ── render_latency_chart ───────────────────────────────────── *)

let test_latency_empty () =
  let r = Charts.render_latency_chart [] ~width:40 ~height:10 in
  check bool "no data msg" true (contains r "No data")

let test_latency_all_none () =
  let r = Charts.render_latency_chart [make_snapshot ()] ~width:40 ~height:10 in
  check bool "no render data" true (contains r "No render data")

let test_latency_with_data () =
  let samples =
    [make_snapshot ~render_p50:5.0 ~render_p90:12.0 ~render_p99:25.0 ()]
  in
  let r = Charts.render_latency_chart samples ~width:60 ~height:10 in
  check bool "has p50" true (contains r "p50:") ;
  check bool "has p90" true (contains r "p90:") ;
  check bool "has p99" true (contains r "p99:")

let test_latency_partial () =
  let samples = [make_snapshot ~render_p50:3.0 ()] in
  let r = Charts.render_latency_chart samples ~width:60 ~height:10 in
  check bool "has p50 value" true (contains r "3.0ms") ;
  check bool "has N/A for p90" true (contains r "N/A")

(* ── render_key_to_render_chart ─────────────────────────────── *)

let test_key_to_render_empty () =
  let r = Charts.render_key_to_render_chart [] ~width:40 ~height:10 in
  check bool "no data msg" true (contains r "No data")

let test_key_to_render_all_none () =
  let r =
    Charts.render_key_to_render_chart [make_snapshot ()] ~width:40 ~height:10
  in
  check bool "no interaction" true (contains r "No interaction")

let test_key_to_render_with_data () =
  let samples =
    [make_snapshot ~key_to_render_p50:8.0 ~key_to_render_p90:15.0 ()]
  in
  let r = Charts.render_key_to_render_chart samples ~width:60 ~height:10 in
  check bool "has p50" true (contains r "p50:") ;
  check bool "has p90" true (contains r "p90:")

(* ── render_summary_bars ────────────────────────────────────── *)

let test_summary_empty () =
  let r = Charts.render_summary_bars [] ~width:60 ~height:8 in
  check bool "no data" true (contains r "No data")

let test_summary_with_data () =
  let samples =
    [make_snapshot ~services_active:3 ~services_total:5 ~bg_queue_depth:0 ()]
  in
  let r = Charts.render_summary_bars samples ~width:60 ~height:8 in
  check bool "has Services" true (contains r "Services:")

let test_summary_idle_queue () =
  let samples = [make_snapshot ~bg_queue_depth:0 ()] in
  let r = Charts.render_summary_bars samples ~width:60 ~height:8 in
  check bool "has idle" true (contains r "idle")

let test_summary_busy_queue () =
  let samples = [make_snapshot ~bg_queue_depth:5 ()] in
  let r = Charts.render_summary_bars samples ~width:60 ~height:8 in
  check bool "has tasks" true (contains r "tasks")

let test_summary_with_render_p99 () =
  let samples = [make_snapshot ~render_p99:10.5 ()] in
  let r = Charts.render_summary_bars samples ~width:60 ~height:8 in
  check bool "has Render" true (contains r "Render:") ;
  check bool "has ms" true (contains r "10.5ms")

let test_summary_no_render_p99 () =
  let samples = [make_snapshot ()] in
  let r = Charts.render_summary_bars samples ~width:60 ~height:8 in
  (* No render_p99 means no Render line *)
  check bool "no Render line" false (contains r "Render:")

(* ── Test suite ──────────────────────────────────────────────── *)

let () =
  Alcotest.run
    "Charts"
    [
      ( "trim_chart_padding",
        [
          test_case "empty" `Quick test_trim_empty;
          test_case "no trailing" `Quick test_trim_no_trailing;
          test_case "trailing spaces" `Quick test_trim_trailing_spaces;
          test_case "multiline" `Quick test_trim_multiline;
          test_case "all spaces" `Quick test_trim_all_spaces;
        ] );
      ( "render_bg_queue_chart",
        [
          test_case "empty" `Quick test_bg_queue_empty;
          test_case "with data" `Quick test_bg_queue_with_data;
          test_case "single sample" `Quick test_bg_queue_single_sample;
        ] );
      ( "render_service_status_chart",
        [
          test_case "empty" `Quick test_service_status_empty;
          test_case "with data" `Quick test_service_status_with_data;
        ] );
      ( "render_latency_chart",
        [
          test_case "empty" `Quick test_latency_empty;
          test_case "all None" `Quick test_latency_all_none;
          test_case "with data" `Quick test_latency_with_data;
          test_case "partial" `Quick test_latency_partial;
        ] );
      ( "render_key_to_render_chart",
        [
          test_case "empty" `Quick test_key_to_render_empty;
          test_case "all None" `Quick test_key_to_render_all_none;
          test_case "with data" `Quick test_key_to_render_with_data;
        ] );
      ( "render_summary_bars",
        [
          test_case "empty" `Quick test_summary_empty;
          test_case "with data" `Quick test_summary_with_data;
          test_case "idle queue" `Quick test_summary_idle_queue;
          test_case "busy queue" `Quick test_summary_busy_queue;
          test_case "with render p99" `Quick test_summary_with_render_p99;
          test_case "no render p99" `Quick test_summary_no_render_p99;
        ] );
    ]
