(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Tests for system metrics scheduler module. *)

module System_metrics_scheduler = Octez_manager_ui.System_metrics_scheduler

(** Test helpers *)
let version_pair = Alcotest.testable Fmt.(pair int int) ( = )

let version_status =
  let pp fmt status =
    let open System_metrics_scheduler.For_test in
    match status with
    | Latest -> Fmt.string fmt "Latest"
    | MinorBehind -> Fmt.string fmt "MinorBehind"
    | MajorBehind -> Fmt.string fmt "MajorBehind"
    | DevOrRC -> Fmt.string fmt "DevOrRC"
    | Unknown -> Fmt.string fmt "Unknown"
  in
  Alcotest.testable pp ( = )

(** {2 Version Parsing Tests} *)

let test_parse_version_with_v_prefix () =
  let open System_metrics_scheduler.For_test in
  Alcotest.(check (option version_pair))
    "parse v23.3"
    (Some (23, 3))
    (parse_version "v23.3")

let test_parse_version_without_prefix () =
  let open System_metrics_scheduler.For_test in
  Alcotest.(check (option version_pair))
    "parse 23.3"
    (Some (23, 3))
    (parse_version "23.3")

let test_parse_version_single_number () =
  let open System_metrics_scheduler.For_test in
  Alcotest.(check (option version_pair))
    "parse 23"
    (Some (23, 0))
    (parse_version "23")

let test_parse_version_with_patch () =
  let open System_metrics_scheduler.For_test in
  Alcotest.(check (option version_pair))
    "parse 23.3.1 (ignores patch)"
    (Some (23, 3))
    (parse_version "23.3.1")

let test_parse_version_empty () =
  let open System_metrics_scheduler.For_test in
  Alcotest.(check (option version_pair)) "parse empty" None (parse_version "")

let test_parse_version_invalid () =
  let open System_metrics_scheduler.For_test in
  Alcotest.(check (option version_pair))
    "parse invalid"
    None
    (parse_version "abc") ;
  Alcotest.(check (option version_pair))
    "parse dots only"
    None
    (parse_version "...") ;
  Alcotest.(check (option version_pair)) "parse v only" None (parse_version "v")

let test_parse_version_with_whitespace () =
  let open System_metrics_scheduler.For_test in
  Alcotest.(check (option version_pair))
    "parse with whitespace"
    (Some (23, 3))
    (parse_version "  23.3  ")

(** {2 RC/Dev Detection Tests} *)

let test_is_rc_or_dev_with_rc () =
  let open System_metrics_scheduler.For_test in
  Alcotest.(check bool) "23.3-rc1 is RC" true (is_rc_or_dev "23.3-rc1") ;
  Alcotest.(check bool) "23.3-rc is RC" true (is_rc_or_dev "23.3-rc") ;
  Alcotest.(check bool) "23.3-RC1 is RC" true (is_rc_or_dev "23.3-RC1")

let test_is_rc_or_dev_with_dev () =
  let open System_metrics_scheduler.For_test in
  Alcotest.(check bool) "23.3-dev is dev" true (is_rc_or_dev "23.3-dev") ;
  Alcotest.(check bool) "23.3-DEV is dev" true (is_rc_or_dev "23.3-DEV")

let test_is_rc_or_dev_stable () =
  let open System_metrics_scheduler.For_test in
  Alcotest.(check bool) "23.3 is stable" false (is_rc_or_dev "23.3") ;
  Alcotest.(check bool) "v23.3 is stable" false (is_rc_or_dev "v23.3")

(** {2 Version Status Comparison Tests} *)

let test_version_status_latest () =
  let open System_metrics_scheduler.For_test in
  set_latest_version (Some (23, 3)) ;
  Alcotest.(check version_status)
    "23.3 is latest"
    Latest
    (check_version_status ~running:"23.3") ;
  clear_all ()

let test_version_status_minor_behind () =
  let open System_metrics_scheduler.For_test in
  set_latest_version (Some (23, 5)) ;
  Alcotest.(check version_status)
    "23.3 is minor behind"
    MinorBehind
    (check_version_status ~running:"23.3") ;
  clear_all ()

let test_version_status_major_behind () =
  let open System_metrics_scheduler.For_test in
  set_latest_version (Some (24, 0)) ;
  Alcotest.(check version_status)
    "23.3 is major behind"
    MajorBehind
    (check_version_status ~running:"23.3") ;
  clear_all ()

let test_version_status_ahead () =
  let open System_metrics_scheduler.For_test in
  set_latest_version (Some (23, 3)) ;
  (* Running newer than latest - treat as latest *)
  Alcotest.(check version_status)
    "24.0 is ahead (treated as latest)"
    Latest
    (check_version_status ~running:"24.0") ;
  clear_all ()

let test_version_status_rc_or_dev () =
  let open System_metrics_scheduler.For_test in
  set_latest_version (Some (23, 3)) ;
  Alcotest.(check version_status)
    "23.3-rc1 is RC"
    DevOrRC
    (check_version_status ~running:"23.3-rc1") ;
  Alcotest.(check version_status)
    "23.3-dev is dev"
    DevOrRC
    (check_version_status ~running:"23.3-dev") ;
  clear_all ()

let test_version_status_unknown_no_latest () =
  let open System_metrics_scheduler.For_test in
  set_latest_version None ;
  Alcotest.(check version_status)
    "unknown when no latest version"
    Unknown
    (check_version_status ~running:"23.3") ;
  clear_all ()

let test_version_status_unknown_invalid () =
  let open System_metrics_scheduler.For_test in
  set_latest_version (Some (23, 3)) ;
  Alcotest.(check version_status)
    "unknown for invalid version"
    Unknown
    (check_version_status ~running:"invalid") ;
  clear_all ()

(** {2 Version Color Tests} *)

let test_version_color_codes () =
  let open System_metrics_scheduler.For_test in
  Alcotest.(check string) "latest is green" "\027[32m" (version_color Latest) ;
  Alcotest.(check string)
    "minor behind is yellow"
    "\027[33m"
    (version_color MinorBehind) ;
  Alcotest.(check string)
    "major behind is red"
    "\027[31m"
    (version_color MajorBehind) ;
  Alcotest.(check string) "dev/rc is blue" "\027[34m" (version_color DevOrRC) ;
  Alcotest.(check string) "unknown is no color" "" (version_color Unknown)

(** {2 Visibility Tracking Tests} *)

let test_mark_visible () =
  System_metrics_scheduler.For_test.clear_all () ;
  System_metrics_scheduler.clear_visibility () ;
  System_metrics_scheduler.mark_visible ~role:"node" ~instance:"test" ;
  let key = "node/test" in
  Alcotest.(check bool)
    "instance is visible"
    true
    (System_metrics_scheduler.For_test.is_visible key) ;
  System_metrics_scheduler.For_test.clear_all ()

let test_mark_hidden () =
  System_metrics_scheduler.For_test.clear_all () ;
  System_metrics_scheduler.clear_visibility () ;
  System_metrics_scheduler.mark_visible ~role:"node" ~instance:"test" ;
  System_metrics_scheduler.mark_hidden ~role:"node" ~instance:"test" ;
  let key = "node/test" in
  Alcotest.(check bool)
    "instance is hidden"
    false
    (System_metrics_scheduler.For_test.is_visible key) ;
  System_metrics_scheduler.For_test.clear_all ()

let test_clear_visibility () =
  System_metrics_scheduler.For_test.clear_all () ;
  System_metrics_scheduler.clear_visibility () ;
  System_metrics_scheduler.mark_visible ~role:"node" ~instance:"test1" ;
  System_metrics_scheduler.mark_visible ~role:"baker" ~instance:"test2" ;
  System_metrics_scheduler.clear_visibility () ;
  Alcotest.(check bool)
    "test1 cleared"
    false
    (System_metrics_scheduler.For_test.is_visible "node/test1") ;
  Alcotest.(check bool)
    "test2 cleared"
    false
    (System_metrics_scheduler.For_test.is_visible "baker/test2") ;
  System_metrics_scheduler.For_test.clear_all ()

let test_effective_interval_visible () =
  System_metrics_scheduler.For_test.clear_all () ;
  System_metrics_scheduler.clear_visibility () ;
  System_metrics_scheduler.mark_visible ~role:"node" ~instance:"test" ;
  let key = "node/test" in
  let interval =
    System_metrics_scheduler.For_test.effective_interval ~key ~base_interval:1.0
  in
  Alcotest.(check (float 0.01)) "visible interval is 1x base" 1.0 interval ;
  System_metrics_scheduler.For_test.clear_all ()

let test_effective_interval_hidden () =
  System_metrics_scheduler.For_test.clear_all () ;
  System_metrics_scheduler.clear_visibility () ;
  (* Don't mark as visible, so it's hidden *)
  let key = "node/test" in
  let interval =
    System_metrics_scheduler.For_test.effective_interval ~key ~base_interval:1.0
  in
  Alcotest.(check (float 0.01)) "hidden interval is 4x base" 4.0 interval ;
  System_metrics_scheduler.For_test.clear_all ()

(** {2 Metrics Access Tests} *)

let test_get_version_missing () =
  System_metrics_scheduler.For_test.clear_all () ;
  let version =
    System_metrics_scheduler.get_version ~role:"node" ~instance:"nonexistent"
  in
  Alcotest.(check (option string))
    "missing instance has no version"
    None
    version ;
  System_metrics_scheduler.For_test.clear_all ()

let test_get_disk_size_missing () =
  System_metrics_scheduler.For_test.clear_all () ;
  let size =
    System_metrics_scheduler.get_disk_size ~role:"node" ~instance:"nonexistent"
  in
  Alcotest.(check (option int64)) "missing instance has no disk size" None size ;
  System_metrics_scheduler.For_test.clear_all ()

let test_render_cpu_chart_no_data () =
  System_metrics_scheduler.For_test.clear_all () ;
  let chart =
    System_metrics_scheduler.render_cpu_chart
      ~role:"node"
      ~instance:"test"
      ~focus:false
  in
  Alcotest.(check (option (pair string (float 0.01))))
    "no data returns None"
    None
    chart ;
  System_metrics_scheduler.For_test.clear_all ()

let test_render_mem_sparkline_no_data () =
  System_metrics_scheduler.For_test.clear_all () ;
  let sparkline =
    System_metrics_scheduler.render_mem_sparkline
      ~role:"node"
      ~instance:"test"
      ~focus:false
  in
  Alcotest.(check string) "no data returns empty" "" sparkline ;
  System_metrics_scheduler.For_test.clear_all ()

(** {2 Test Suite} *)

let version_parsing_tests =
  [
    ("parse version with v prefix", `Quick, test_parse_version_with_v_prefix);
    ("parse version without prefix", `Quick, test_parse_version_without_prefix);
    ("parse single number version", `Quick, test_parse_version_single_number);
    ("parse version with patch", `Quick, test_parse_version_with_patch);
    ("parse empty version", `Quick, test_parse_version_empty);
    ("parse invalid version", `Quick, test_parse_version_invalid);
    ("parse version with whitespace", `Quick, test_parse_version_with_whitespace);
  ]

let rc_dev_detection_tests =
  [
    ("detect RC versions", `Quick, test_is_rc_or_dev_with_rc);
    ("detect dev versions", `Quick, test_is_rc_or_dev_with_dev);
    ("stable versions not RC/dev", `Quick, test_is_rc_or_dev_stable);
  ]

let version_status_tests =
  [
    ("version status latest", `Quick, test_version_status_latest);
    ("version status minor behind", `Quick, test_version_status_minor_behind);
    ("version status major behind", `Quick, test_version_status_major_behind);
    ("version status ahead", `Quick, test_version_status_ahead);
    ("version status RC or dev", `Quick, test_version_status_rc_or_dev);
    ( "version status unknown no latest",
      `Quick,
      test_version_status_unknown_no_latest );
    ( "version status unknown invalid",
      `Quick,
      test_version_status_unknown_invalid );
  ]

let version_color_tests =
  [("version color codes", `Quick, test_version_color_codes)]

let visibility_tests =
  [
    ("mark instance visible", `Quick, test_mark_visible);
    ("mark instance hidden", `Quick, test_mark_hidden);
    ("clear all visibility", `Quick, test_clear_visibility);
    ("effective interval visible", `Quick, test_effective_interval_visible);
    ("effective interval hidden", `Quick, test_effective_interval_hidden);
  ]

let metrics_access_tests =
  [
    ("get version missing", `Quick, test_get_version_missing);
    ("get disk size missing", `Quick, test_get_disk_size_missing);
    ("render CPU chart no data", `Quick, test_render_cpu_chart_no_data);
    ( "render memory sparkline no data",
      `Quick,
      test_render_mem_sparkline_no_data );
  ]

let () =
  Alcotest.run
    "System Metrics Scheduler"
    [
      ("version parsing", version_parsing_tests);
      ("RC/dev detection", rc_dev_detection_tests);
      ("version status comparison", version_status_tests);
      ("version color codes", version_color_tests);
      ("visibility tracking", visibility_tests);
      ("metrics access", metrics_access_tests);
    ]
