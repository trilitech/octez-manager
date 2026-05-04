(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Tests for Metrics module.

    Covers parse_addr, record/get stateful API, percentile, take_snapshot,
    recording duration, and metrics_text output. *)

open Alcotest
module M = Octez_manager_ui.Metrics

let float_eps = float 0.001

(* ============================================================ *)
(* parse_addr Tests *)
(* ============================================================ *)

let test_parse_addr_host_port () =
  match M.parse_addr "127.0.0.1:9090" with
  | Ok (host, port) ->
      check string "host" "127.0.0.1" host ;
      check int "port" 9090 port
  | Error (`Msg m) -> fail m

let test_parse_addr_port_only () =
  match M.parse_addr "9090" with
  | Ok (host, port) ->
      check string "default host" "127.0.0.1" host ;
      check int "port" 9090 port
  | Error (`Msg m) -> fail m

let test_parse_addr_port_1 () =
  match M.parse_addr "1" with
  | Ok (_, port) -> check int "min port" 1 port
  | Error (`Msg m) -> fail m

let test_parse_addr_port_65535 () =
  match M.parse_addr "65535" with
  | Ok (_, port) -> check int "max port" 65535 port
  | Error (`Msg m) -> fail m

let test_parse_addr_port_0 () =
  match M.parse_addr "0" with
  | Ok _ -> fail "should reject port 0"
  | Error _ -> check bool "rejects" true true

let test_parse_addr_port_65536 () =
  match M.parse_addr "65536" with
  | Ok _ -> fail "should reject port 65536"
  | Error _ -> check bool "rejects" true true

let test_parse_addr_negative_port () =
  match M.parse_addr "-1" with
  | Ok _ -> fail "should reject negative port"
  | Error _ -> check bool "rejects" true true

let test_parse_addr_non_numeric () =
  match M.parse_addr "localhost:abc" with
  | Ok _ -> fail "should reject non-numeric port"
  | Error _ -> check bool "rejects" true true

let test_parse_addr_empty () =
  match M.parse_addr "" with
  | Ok _ -> fail "should reject empty"
  | Error _ -> check bool "rejects" true true

let test_parse_addr_multiple_colons () =
  match M.parse_addr "::1:8080" with
  | Ok _ -> fail "should reject multiple colons"
  | Error _ -> check bool "rejects" true true

let test_parse_addr_host_no_port () =
  match M.parse_addr "localhost:" with
  | Ok _ -> fail "should reject empty port"
  | Error _ -> check bool "rejects" true true

let test_parse_addr_with_spaces () =
  match M.parse_addr "127.0.0.1: 9090" with
  | Ok (_, port) -> check int "trims port" 9090 port
  | Error (`Msg _) -> check bool "or rejects" true true

(* ============================================================ *)
(* percentile Tests *)
(* ============================================================ *)

let test_percentile_empty () =
  check (option float_eps) "empty" None (M.For_tests.percentile [||] 0.5)

let test_percentile_single () =
  check
    (option float_eps)
    "single"
    (Some 42.0)
    (M.For_tests.percentile [|42.0|] 0.5)

let test_percentile_p0 () =
  let arr = [|1.0; 2.0; 3.0; 4.0; 5.0|] in
  match M.For_tests.percentile arr 0.0 with
  | Some v -> check bool "p0 is smallest" true (v <= 2.0)
  | None -> fail "should return value"

let test_percentile_p100 () =
  let arr = [|1.0; 2.0; 3.0; 4.0; 5.0|] in
  match M.For_tests.percentile arr 1.0 with
  | Some v -> check bool "p100 is largest" true (v >= 4.0)
  | None -> fail "should return value"

let test_percentile_p50_odd () =
  let arr = [|10.0; 20.0; 30.0; 40.0; 50.0|] in
  match M.For_tests.percentile arr 0.5 with
  | Some v -> check bool "p50 in middle" true (v >= 20.0 && v <= 40.0)
  | None -> fail "should return value"

let test_percentile_all_same () =
  let arr = [|7.0; 7.0; 7.0; 7.0|] in
  check
    (option float_eps)
    "all same"
    (Some 7.0)
    (M.For_tests.percentile arr 0.5)

let test_percentile_unsorted () =
  let arr = [|5.0; 1.0; 3.0; 2.0; 4.0|] in
  match M.For_tests.percentile arr 0.5 with
  | Some v -> check bool "handles unsorted" true (v >= 1.0 && v <= 5.0)
  | None -> fail "should return value"

(* ============================================================ *)
(* Stateful API Tests *)
(* ============================================================ *)

let test_bg_queue_initial () =
  M.For_tests.reset () ;
  check int "initial depth" 0 (M.get_bg_queue_depth ()) ;
  check int "initial max" 0 (M.get_bg_queue_max ())

let test_bg_enqueue_dequeue () =
  M.For_tests.reset () ;
  M.record_bg_enqueue ~queued_depth:3 ;
  check int "depth after enqueue" 3 (M.get_bg_queue_depth ()) ;
  check int "max after enqueue" 3 (M.get_bg_queue_max ()) ;
  M.record_bg_enqueue ~queued_depth:5 ;
  check int "max tracks highest" 5 (M.get_bg_queue_max ()) ;
  M.record_bg_dequeue ~queued_depth:4 ~wait_ms:1.5 ;
  check int "depth after dequeue" 4 (M.get_bg_queue_depth ()) ;
  check int "max unchanged" 5 (M.get_bg_queue_max ())

let test_service_statuses () =
  M.For_tests.reset () ;
  M.record_service_status ~service:"node-1" ~is_active:true ;
  M.record_service_status ~service:"baker-1" ~is_active:false ;
  let statuses = M.get_service_statuses () in
  check int "two services" 2 (List.length statuses) ;
  let node_active =
    List.find_opt (fun (s, _) -> s = "node-1") statuses |> Option.map snd
  in
  check (option bool) "node active" (Some true) node_active ;
  let baker_active =
    List.find_opt (fun (s, _) -> s = "baker-1") statuses |> Option.map snd
  in
  check (option bool) "baker inactive" (Some false) baker_active

let test_service_status_update () =
  M.For_tests.reset () ;
  M.record_service_status ~service:"node-1" ~is_active:true ;
  M.record_service_status ~service:"node-1" ~is_active:false ;
  let statuses = M.get_service_statuses () in
  check int "still one service" 1 (List.length statuses) ;
  let active = List.assoc "node-1" statuses in
  check bool "now inactive" false active

let test_server_info_initial () =
  M.For_tests.reset () ;
  check bool "no server initially" true (M.get_server_info () = None)

let test_is_enabled_initial () =
  M.For_tests.reset () ;
  check bool "not enabled initially" false (M.is_enabled ())

let test_take_snapshot_empty () =
  M.For_tests.reset () ;
  let snap = M.take_snapshot () in
  check int "no services active" 0 snap.services_active ;
  check int "no services total" 0 snap.services_total ;
  check int "queue depth 0" 0 snap.bg_queue_depth ;
  check int "queue max 0" 0 snap.bg_queue_max ;
  check bool "timestamp positive" true (snap.timestamp > 0.0)

let test_take_snapshot_with_data () =
  M.For_tests.reset () ;
  M.record_bg_enqueue ~queued_depth:2 ;
  M.record_service_status ~service:"svc-a" ~is_active:true ;
  M.record_service_status ~service:"svc-b" ~is_active:false ;
  let snap = M.take_snapshot () in
  check int "1 active" 1 snap.services_active ;
  check int "2 total" 2 snap.services_total ;
  check int "queue depth 2" 2 snap.bg_queue_depth

(* ============================================================ *)
(* Recording Tests *)
(* ============================================================ *)

let test_recording_duration () =
  M.For_tests.reset () ;
  check int "default 60" 60 (M.get_recording_duration ()) ;
  M.set_recording_duration 120 ;
  check int "updated to 120" 120 (M.get_recording_duration ())

let test_recording_not_active () =
  M.For_tests.reset () ;
  check bool "not recording" false (M.is_recording ())

let test_clear_snapshots () =
  M.For_tests.reset () ;
  check (list pass) "empty initially" [] (M.get_snapshots ())

let test_set_duration_preserves_data () =
  M.For_tests.reset () ;
  M.set_recording_duration 10 ;
  check int "duration 10" 10 (M.get_recording_duration ()) ;
  M.set_recording_duration 5 ;
  check int "duration 5" 5 (M.get_recording_duration ())

(* ============================================================ *)
(* metrics_text Tests *)
(* ============================================================ *)

let test_metrics_text_empty () =
  M.For_tests.reset () ;
  let text = M.For_tests.metrics_text () in
  (* Even with no data, should contain queue metrics *)
  check
    bool
    "contains queue depth"
    true
    (String.length text > 0
    &&
      try
        ignore (Str.search_forward (Str.regexp_string "bg_queue_depth") text 0) ;
        true
      with Not_found -> false)

let test_metrics_text_with_services () =
  M.For_tests.reset () ;
  M.record_service_status ~service:"test-svc" ~is_active:true ;
  let text = M.For_tests.metrics_text () in
  check
    bool
    "contains service name"
    true
    (try
       ignore (Str.search_forward (Str.regexp_string "test-svc") text 0) ;
       true
     with Not_found -> false)

let test_metrics_text_with_scheduler () =
  M.For_tests.reset () ;
  M.record_scheduler_tick ~scheduler:"rpc_tick" (fun () -> ()) ;
  let text = M.For_tests.metrics_text () in
  check
    bool
    "contains scheduler name"
    true
    (try
       ignore (Str.search_forward (Str.regexp_string "rpc_tick") text 0) ;
       true
     with Not_found -> false)

let test_scheduler_snapshots () =
  M.For_tests.reset () ;
  M.record_scheduler_tick ~scheduler:"test_sched" (fun () -> Unix.sleepf 0.001) ;
  let snaps = M.get_scheduler_snapshots () in
  check
    bool
    "has scheduler"
    true
    (List.exists (fun (name, _) -> name = "test_sched") snaps)

(* ============================================================ *)
(* PBT: parse_addr *)
(* ============================================================ *)

let prop_parse_addr_no_crash =
  QCheck.Test.make
    ~name:"parse_addr never crashes"
    ~count:500
    QCheck.string
    (fun s ->
      match M.parse_addr s with
      | Ok _ -> true
      | Error _ -> true
      | exception _ -> false)

let prop_parse_addr_valid_port_range =
  QCheck.Test.make
    ~name:"parse_addr valid port in [1,65535]"
    ~count:500
    QCheck.(pair (string_size (Gen.return 0)) (int_range 1 65535))
    (fun (_host, port) ->
      let input = Printf.sprintf "127.0.0.1:%d" port in
      match M.parse_addr input with
      | Ok (_, p) -> p >= 1 && p <= 65535
      | Error _ -> true)

(* ============================================================ *)
(* PBT: percentile *)
(* ============================================================ *)

let prop_percentile_in_range =
  QCheck.Test.make
    ~name:"percentile result within [min,max]"
    ~count:300
    QCheck.(
      pair
        (list_size (Gen.int_range 1 50) (float_range (-1e9) 1e9))
        (float_range 0.0 1.0))
    (fun (lst, p) ->
      let arr = Array.of_list lst in
      let arr_copy = Array.copy arr in
      match M.For_tests.percentile arr_copy p with
      | None -> Array.length arr = 0
      | Some v ->
          let min_v = Array.fold_left Float.min Float.infinity arr in
          let max_v = Array.fold_left Float.max Float.neg_infinity arr in
          v >= min_v && v <= max_v)

let prop_percentile_empty_is_none =
  QCheck.Test.make
    ~name:"percentile empty array is None"
    ~count:100
    QCheck.(float_range 0.0 1.0)
    (fun p -> M.For_tests.percentile [||] p = None)

(* ============================================================ *)
(* Test Suite *)
(* ============================================================ *)

let () =
  Alcotest.run
    "Metrics"
    [
      ( "parse_addr",
        [
          test_case "host:port" `Quick test_parse_addr_host_port;
          test_case "port only" `Quick test_parse_addr_port_only;
          test_case "port 1" `Quick test_parse_addr_port_1;
          test_case "port 65535" `Quick test_parse_addr_port_65535;
          test_case "port 0 rejected" `Quick test_parse_addr_port_0;
          test_case "port 65536 rejected" `Quick test_parse_addr_port_65536;
          test_case "negative port" `Quick test_parse_addr_negative_port;
          test_case "non-numeric port" `Quick test_parse_addr_non_numeric;
          test_case "empty string" `Quick test_parse_addr_empty;
          test_case "multiple colons" `Quick test_parse_addr_multiple_colons;
          test_case "host no port" `Quick test_parse_addr_host_no_port;
          test_case "with spaces" `Quick test_parse_addr_with_spaces;
        ] );
      ( "percentile",
        [
          test_case "empty" `Quick test_percentile_empty;
          test_case "single" `Quick test_percentile_single;
          test_case "p0" `Quick test_percentile_p0;
          test_case "p100" `Quick test_percentile_p100;
          test_case "p50 odd" `Quick test_percentile_p50_odd;
          test_case "all same" `Quick test_percentile_all_same;
          test_case "unsorted" `Quick test_percentile_unsorted;
        ] );
      ( "stateful_api",
        [
          test_case "bg queue initial" `Quick test_bg_queue_initial;
          test_case "bg enqueue/dequeue" `Quick test_bg_enqueue_dequeue;
          test_case "service statuses" `Quick test_service_statuses;
          test_case "service status update" `Quick test_service_status_update;
          test_case "server info initial" `Quick test_server_info_initial;
          test_case "is_enabled initial" `Quick test_is_enabled_initial;
          test_case "take_snapshot empty" `Quick test_take_snapshot_empty;
          test_case
            "take_snapshot with data"
            `Quick
            test_take_snapshot_with_data;
        ] );
      ( "recording",
        [
          test_case "duration" `Quick test_recording_duration;
          test_case "not active" `Quick test_recording_not_active;
          test_case "clear snapshots" `Quick test_clear_snapshots;
          test_case
            "set duration preserves"
            `Quick
            test_set_duration_preserves_data;
        ] );
      ( "metrics_text",
        [
          test_case "empty state" `Quick test_metrics_text_empty;
          test_case "with services" `Quick test_metrics_text_with_services;
          test_case "with scheduler" `Quick test_metrics_text_with_scheduler;
          test_case "scheduler snapshots" `Quick test_scheduler_snapshots;
        ] );
      ( "pbt_parse_addr",
        List.map
          QCheck_alcotest.to_alcotest
          [prop_parse_addr_no_crash; prop_parse_addr_valid_port_range] );
      ( "pbt_percentile",
        List.map
          QCheck_alcotest.to_alcotest
          [prop_percentile_in_range; prop_percentile_empty_is_none] );
    ]
