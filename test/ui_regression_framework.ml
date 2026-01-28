(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** UI Regression Testing Framework
    
    Provides infrastructure for capturing and comparing UI screenshots
    to detect visual regressions. Uses headless driver to render UI
    and compares rendered output pixel-perfect.
    
    Usage:
      - Run with --update-regressions to capture baseline screenshots
      - Normal runs compare against baseline and fail on differences
*)

module HD = Lib_miaou_internal.Headless_driver

(* ============================================================ *)
(* Configuration *)
(* ============================================================ *)

let regression_dir = "test/ui_regressions"

let update_mode = ref false

let () =
  (* Check if UPDATE_REGRESSIONS env var is set, or --update-regressions flag *)
  (match Sys.getenv_opt "UPDATE_REGRESSIONS" with
  | Some "1" | Some "true" -> update_mode := true
  | _ -> ()) ;

  (* Also check command line for backward compatibility *)
  if Array.length Sys.argv > 1 then
    for i = 1 to Array.length Sys.argv - 1 do
      if Sys.argv.(i) = "--update-regressions" then update_mode := true
    done

(* ============================================================ *)
(* Filesystem Utilities *)
(* ============================================================ *)

let ensure_dir path =
  let rec ensure_parent p =
    if p = "." || p = "/" || p = "" then ()
    else
      let parent = Filename.dirname p in
      if parent <> p then ensure_parent parent ;
      if not (Sys.file_exists p) then Unix.mkdir p 0o755
  in
  ensure_parent path

let read_file path =
  try
    let ic = open_in path in
    let len = in_channel_length ic in
    let content = really_input_string ic len in
    close_in ic ;
    Some content
  with _ -> None

let write_file path content =
  let oc = open_out path in
  output_string oc content ;
  close_out oc

(* ============================================================ *)
(* Screen Capture *)
(* ============================================================ *)

(** Strip ANSI escape codes from screen content *)
let strip_ansi s =
  let re = Str.regexp "\027\\[[0-9;]*m" in
  Str.global_replace re "" s

(** Get current rendered screen as text *)
let capture_screen () =
  let raw = HD.get_screen_content () in
  strip_ansi raw

(** Get screen with size metadata *)
let capture_screen_with_metadata () =
  let content = capture_screen () in
  let lines = String.split_on_char '\n' content in
  let height = List.length lines in
  let width =
    List.fold_left (fun acc line -> max acc (String.length line)) 0 lines
  in
  (width, height, content)

(* ============================================================ *)
(* Regression Storage *)
(* ============================================================ *)

(** Generate regression file path for a test *)
let regression_path test_name =
  Filename.concat regression_dir (test_name ^ ".screen")

(** Normalize ephemeral URL date suffixes so baselines stay stable.
    e.g. weeklynet-2026-01-28 → weeklynet-YYYY-MM-DD *)
let normalize_ephemeral_urls s =
  let re = Str.regexp "weeklynet-[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]" in
  Str.global_replace re "weeklynet-YYYY-MM-DD" s

(** Save screen capture as baseline *)
let save_baseline test_name =
  ensure_dir regression_dir ;
  let path = regression_path test_name in
  let width, height, content = capture_screen_with_metadata () in
  let metadata = Printf.sprintf "SIZE:%dx%d\n" width height in
  write_file path (metadata ^ normalize_ephemeral_urls content) ;
  Printf.printf "✓ Saved baseline for %s\n%!" test_name

(** Load baseline screen capture *)
let load_baseline test_name =
  let path = regression_path test_name in
  match read_file path with
  | None -> None
  | Some content -> (
      (* Parse metadata *)
      let lines = String.split_on_char '\n' content in
      match lines with
      | meta :: rest ->
          let screen_content = String.concat "\n" rest in
          Some (meta, screen_content)
      | [] -> None)

(* ============================================================ *)
(* Comparison *)
(* ============================================================ *)

(** Compare two screen captures line by line *)
let compare_screens baseline actual =
  let baseline_lines =
    String.split_on_char '\n' (normalize_ephemeral_urls baseline)
  in
  let actual_lines =
    String.split_on_char '\n' (normalize_ephemeral_urls actual)
  in

  let max_lines = max (List.length baseline_lines) (List.length actual_lines) in
  let differences = ref [] in

  for i = 0 to max_lines - 1 do
    let baseline_line =
      if i < List.length baseline_lines then List.nth baseline_lines i else ""
    in
    let actual_line =
      if i < List.length actual_lines then List.nth actual_lines i else ""
    in

    if baseline_line <> actual_line then
      differences := (i + 1, baseline_line, actual_line) :: !differences
  done ;

  List.rev !differences

(** Generate diff report *)
let format_diff test_name differences =
  let buf = Buffer.create 1024 in
  Buffer.add_string
    buf
    (Printf.sprintf "\n❌ UI Regression detected in: %s\n\n" test_name) ;
  Buffer.add_string buf "Differences:\n" ;
  Buffer.add_string buf "────────────────────────────────────────\n" ;

  List.iter
    (fun (line_num, baseline, actual) ->
      Buffer.add_string buf (Printf.sprintf "Line %d:\n" line_num) ;
      Buffer.add_string
        buf
        (Printf.sprintf
           "  Expected: %s\n"
           (if baseline = "" then "(empty)" else baseline)) ;
      Buffer.add_string
        buf
        (Printf.sprintf
           "  Actual:   %s\n\n"
           (if actual = "" then "(empty)" else actual)))
    differences ;

  Buffer.add_string buf "────────────────────────────────────────\n" ;
  Buffer.add_string buf "To update baseline: run with --update-regressions\n" ;
  Buffer.contents buf

(** Generate diff report with full screenshot display for CI *)
let format_diff_with_screenshot test_name differences actual_content =
  let diff_text = format_diff test_name differences in
  let is_ci =
    match Sys.getenv_opt "CI" with
    | Some "true" -> true
    | _ -> (
        match Sys.getenv_opt "GITHUB_ACTIONS" with
        | Some "true" -> true
        | _ -> false)
  in
  if is_ci then
    diff_text ^ "\n\n"
    ^ "╔════════════════════════════════════════════════════════════════╗\n"
    ^ "║                    ACTUAL SCREENSHOT                           ║\n"
    ^ "╚════════════════════════════════════════════════════════════════╝\n"
    ^ actual_content ^ "\n"
    ^ "╚════════════════════════════════════════════════════════════════╝\n"
  else diff_text

(* ============================================================ *)
(* Test API *)
(* ============================================================ *)

(** Main regression test function *)
let check_regression test_name =
  let _width, _height, current = capture_screen_with_metadata () in

  if !update_mode then (
    save_baseline test_name ;
    true)
  else
    match load_baseline test_name with
    | None ->
        Printf.printf "⚠ No baseline for %s, creating...\n%!" test_name ;
        save_baseline test_name ;
        true
    | Some (_meta, baseline) ->
        let differences = compare_screens baseline current in
        if differences = [] then (
          Printf.printf "✓ %s: UI matches baseline\n%!" test_name ;
          true)
        else (
          Printf.printf
            "%s"
            (format_diff_with_screenshot test_name differences current) ;
          false)

(** Assert that current screen matches baseline *)
let assert_ui_regression test_name =
  if not (check_regression test_name) then
    failwith (Printf.sprintf "UI regression in %s" test_name)

(** Display screenshot in terminal (for CI logs) *)
let display_screenshot content =
  let is_ci =
    match Sys.getenv_opt "CI" with
    | Some "true" -> true
    | _ -> (
        match Sys.getenv_opt "GITHUB_ACTIONS" with
        | Some "true" -> true
        | _ -> false)
  in
  if is_ci then (
    Printf.printf
      "\n╔════════════════════════════════════════════════════════════════╗\n" ;
    Printf.printf
      "║                        SCREENSHOT                              ║\n" ;
    Printf.printf
      "╚════════════════════════════════════════════════════════════════╝\n" ;
    Printf.printf "%s\n" content ;
    Printf.printf
      "╚════════════════════════════════════════════════════════════════╝\n\n%!")

(** Capture named screenshot for debugging *)
let capture_debug_screenshot name =
  let path = Filename.concat regression_dir (name ^ ".debug.screen") in
  ensure_dir regression_dir ;
  let content = capture_screen () in
  write_file path content ;
  Printf.printf "📸 Debug screenshot saved: %s\n%!" path ;
  display_screenshot content

(* ============================================================ *)
(* Test Suite Helpers *)
(* ============================================================ *)

(** Run a test scenario and check for regressions at each step *)
let test_scenario name steps =
  List.iteri
    (fun i (step_name, action) ->
      action () ;
      let full_name = Printf.sprintf "%s_step%d_%s" name i step_name in
      assert_ui_regression full_name)
    steps

(** Test a single UI state *)
let test_ui_state name setup_fn =
  setup_fn () ;
  assert_ui_regression name
