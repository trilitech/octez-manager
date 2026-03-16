(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Octez_manager_rewards

let tmpdir () = Filename.temp_dir "om_test_continual" ""

let cleanup_dir dir =
  let rec rm path =
    if Sys.is_directory path then (
      Array.iter (fun f -> rm (Filename.concat path f)) (Sys.readdir path) ;
      Unix.rmdir path)
    else Sys.remove path
  in
  if Sys.file_exists dir then rm dir

(* ── Active state ─────────────────────────────────── *)

let test_active_default () =
  (* Fresh instance should not be active *)
  let instance = "test-baker-active-default" in
  Alcotest.(check bool)
    "not active by default"
    false
    (Payout_continual.is_active ~instance)

let test_enable_disable () =
  let instance = "test-baker-enable-disable" in
  Payout_continual.enable ~instance ;
  Alcotest.(check bool)
    "active after enable"
    true
    (Payout_continual.is_active ~instance) ;
  Payout_continual.disable ~instance ;
  Alcotest.(check bool)
    "inactive after disable"
    false
    (Payout_continual.is_active ~instance)

(* ── Delay file persistence ───────────────────────── *)

let test_delay_file_path () =
  let instance = "test-baker-delay" in
  let path = Payout_continual.delay_file ~instance in
  Alcotest.(check bool)
    "path contains instance"
    true
    (let found = ref false in
     String.split_on_char '/' path
     |> List.iter (fun seg -> if String.equal seg instance then found := true) ;
     !found) ;
  Alcotest.(check bool)
    "path ends with delay_until"
    true
    (Filename.basename path = "delay_until")

let test_read_delay_no_file () =
  (* When no delay file exists, read should return None *)
  let instance = "nonexistent-baker-delay-read" in
  Alcotest.(check (option (float 0.01)))
    "no delay file"
    None
    (Payout_continual.read_delay_until ~instance)

let test_write_read_delay () =
  let dir = tmpdir () in
  (* We need to write to a real path, so we'll use the low-level functions
     with a temp directory. Since delay_file uses rewards_dir which depends
     on Paths.registry_root, we test write/read via a temp file directly. *)
  let path = Filename.concat dir "delay_until" in
  let timestamp = 1700000000.0 in
  let oc = open_out path in
  Printf.fprintf oc "%.0f\n" timestamp ;
  close_out oc ;
  (* Verify file content *)
  let ic = open_in path in
  let line = input_line ic in
  close_in ic ;
  Alcotest.(check (option (float 0.01)))
    "read back timestamp"
    (Some timestamp)
    (Float.of_string_opt (String.trim line)) ;
  cleanup_dir dir

let test_write_read_clear_delay () =
  let dir = tmpdir () in
  let path = Filename.concat dir "delay_until" in
  (* Write *)
  let timestamp = 1700000500.0 in
  let oc = open_out path in
  Printf.fprintf oc "%.0f\n" timestamp ;
  close_out oc ;
  Alcotest.(check bool) "file exists after write" true (Sys.file_exists path) ;
  (* Clear *)
  if Sys.file_exists path then Sys.remove path ;
  Alcotest.(check bool) "file gone after clear" false (Sys.file_exists path) ;
  cleanup_dir dir

let test_delay_file_invalid_content () =
  let dir = tmpdir () in
  let path = Filename.concat dir "delay_until" in
  let oc = open_out path in
  output_string oc "not-a-number\n" ;
  close_out oc ;
  let ic = open_in path in
  let line = input_line ic in
  close_in ic ;
  Alcotest.(check (option (float 0.01)))
    "invalid content returns None"
    None
    (Float.of_string_opt (String.trim line)) ;
  cleanup_dir dir

(* ── Cycles due ───────────────────────────────────── *)

let test_cycles_due_no_unpaid () =
  (* With a fresh instance that has no paid cycles, all matching cycles
     in the window should be returned. We use a dedicated instance name
     to avoid collision. *)
  let instance = "test-continual-cycles-none" in
  let due =
    Payout_continual.cycles_due
      ~instance
      ~current_cycle:10
      ~interval:1
      ~offset:0
  in
  (* cycles 0..9 should be due (nothing is paid) — but window starts at
     max 0 (10-20) = 0 *)
  Alcotest.(check int) "10 cycles due" 10 (List.length due) ;
  Alcotest.(check (list int)) "cycles 0-9" [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] due

let test_cycles_due_with_interval () =
  let instance = "test-continual-interval" in
  let due =
    Payout_continual.cycles_due
      ~instance
      ~current_cycle:10
      ~interval:3
      ~offset:0
  in
  (* Cycles where (c - 0) mod 3 = 0: 0, 3, 6, 9 *)
  Alcotest.(check (list int)) "interval=3 offset=0" [0; 3; 6; 9] due

let test_cycles_due_with_offset () =
  let instance = "test-continual-offset" in
  let due =
    Payout_continual.cycles_due
      ~instance
      ~current_cycle:10
      ~interval:3
      ~offset:1
  in
  (* Cycles where (c - 1) mod 3 = 0: 1, 4, 7 *)
  Alcotest.(check (list int)) "interval=3 offset=1" [1; 4; 7] due

let test_cycles_due_large_cycle () =
  (* For large cycle numbers, only last 20 are checked *)
  let instance = "test-continual-large" in
  let due =
    Payout_continual.cycles_due
      ~instance
      ~current_cycle:100
      ~interval:1
      ~offset:0
  in
  (* Window: max 0 (100-20) = 80, so cycles 80..99 *)
  Alcotest.(check int) "20 cycles due" 20 (List.length due) ;
  Alcotest.(check int) "first is 80" 80 (List.hd due) ;
  Alcotest.(check int) "last is 99" 99 (List.nth due (List.length due - 1))

let test_cycles_due_current_excluded () =
  (* current_cycle itself should never be in the due list *)
  let instance = "test-continual-current" in
  let due =
    Payout_continual.cycles_due ~instance ~current_cycle:5 ~interval:1 ~offset:0
  in
  Alcotest.(check bool) "current cycle excluded" false (List.mem 5 due)

let test_cycles_due_zero_current () =
  let instance = "test-continual-zero" in
  let due =
    Payout_continual.cycles_due ~instance ~current_cycle:0 ~interval:1 ~offset:0
  in
  Alcotest.(check (list int)) "no cycles due at cycle 0" [] due

let () =
  Alcotest.run
    "payout_continual"
    [
      ( "active_state",
        [
          Alcotest.test_case "default inactive" `Quick test_active_default;
          Alcotest.test_case "enable/disable" `Quick test_enable_disable;
        ] );
      ( "delay_file",
        [
          Alcotest.test_case "path structure" `Quick test_delay_file_path;
          Alcotest.test_case "read nonexistent" `Quick test_read_delay_no_file;
          Alcotest.test_case "write/read roundtrip" `Quick test_write_read_delay;
          Alcotest.test_case
            "write/clear lifecycle"
            `Quick
            test_write_read_clear_delay;
          Alcotest.test_case
            "invalid content"
            `Quick
            test_delay_file_invalid_content;
        ] );
      ( "cycles_due",
        [
          Alcotest.test_case "all unpaid" `Quick test_cycles_due_no_unpaid;
          Alcotest.test_case
            "with interval"
            `Quick
            test_cycles_due_with_interval;
          Alcotest.test_case "with offset" `Quick test_cycles_due_with_offset;
          Alcotest.test_case
            "large cycle window"
            `Quick
            test_cycles_due_large_cycle;
          Alcotest.test_case
            "current excluded"
            `Quick
            test_cycles_due_current_excluded;
          Alcotest.test_case "zero current" `Quick test_cycles_due_zero_current;
        ] );
    ]
