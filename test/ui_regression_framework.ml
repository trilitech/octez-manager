(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** UI Regression Testing Framework

    Captures rendered screens and compares them with committed baselines.
    See the interface for the verification contract. *)

module HD = Lib_miaou_internal.Headless_driver

(* ============================================================ *)
(* Configuration *)
(* ============================================================ *)

let regression_dir_env = "OM_UI_REGRESSION_DIR"

let quarantine_file = "QUARANTINE"

let issue_url_prefix = "https://github.com/trilitech/octez-manager/issues/"

let update_mode = ref false

let () =
  (match Sys.getenv_opt "UPDATE_REGRESSIONS" with
  | Some "1" | Some "true" -> update_mode := true
  | _ -> ()) ;
  if Array.exists (String.equal "--update-regressions") Sys.argv then
    update_mode := true

let is_ci () =
  let set name =
    match Sys.getenv_opt name with Some "true" -> true | _ -> false
  in
  set "CI" || set "GITHUB_ACTIONS"

exception Setup_error of string

let setup_error fmt = Printf.ksprintf (fun msg -> raise (Setup_error msg)) fmt

let is_dir path = Sys.file_exists path && Sys.is_directory path

(** Baseline directory lookup. Under [dune runtest] the cwd is
    [_build/default/test] and the baselines are copied there as declared
    dependencies; under [dune exec] from the repository root they live in
    [test/ui_regressions]. A missing directory is a setup error, never an
    empty baseline set. *)
let resolve_regression_dir () =
  match Sys.getenv_opt regression_dir_env with
  | Some dir when is_dir dir -> Ok dir
  | Some dir ->
      Error (Printf.sprintf "%s=%s is not a directory" regression_dir_env dir)
  | None -> (
      match List.find_opt is_dir ["ui_regressions"; "test/ui_regressions"] with
      | Some dir -> Ok dir
      | None ->
          Error
            (Printf.sprintf
               "no baseline directory: neither ui_regressions nor \
                test/ui_regressions exists under %s (set %s)"
               (Sys.getcwd ())
               regression_dir_env))

let regression_dir () =
  match resolve_regression_dir () with
  | Ok dir -> dir
  | Error msg -> setup_error "%s" msg

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
  match open_in_bin path with
  | exception Sys_error msg -> Error msg
  | ic ->
      Fun.protect
        ~finally:(fun () -> close_in_noerr ic)
        (fun () ->
          match really_input_string ic (in_channel_length ic) with
          | content -> Ok content
          | exception (Sys_error msg | Failure msg) -> Error msg
          | exception End_of_file -> Error "unexpected end of file")

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
let regression_path ~dir test_name = Filename.concat dir (test_name ^ ".screen")

(** Normalize ephemeral URL date suffixes so baselines stay stable.
    e.g. weeklynet-2026-01-28 → weeklynet-YYYY-MM-DD *)
let normalize_ephemeral_urls s =
  let re = Str.regexp "weeklynet-[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]" in
  Str.global_replace re "weeklynet-YYYY-MM-DD" s

(** Save screen capture as baseline *)
let save_baseline test_name =
  let dir = regression_dir () in
  let path = regression_path ~dir test_name in
  let width, height, content = capture_screen_with_metadata () in
  let metadata = Printf.sprintf "SIZE:%dx%d\n" width height in
  write_file path (metadata ^ normalize_ephemeral_urls content) ;
  Printf.printf "✓ Saved baseline for %s\n%!" test_name

(** Load a baseline. [Ok None] means the file does not exist; a file that
    exists but cannot be read is an [Error]. *)
let load_baseline ~dir test_name =
  let path = regression_path ~dir test_name in
  if not (Sys.file_exists path) then Ok None
  else
    match read_file path with
    | Error msg -> Error (Printf.sprintf "cannot read baseline %s: %s" path msg)
    | Ok content -> (
        match String.index_opt content '\n' with
        | Some i ->
            Ok
              (Some (String.sub content (i + 1) (String.length content - i - 1)))
        | None -> Error (Printf.sprintf "baseline %s has no SIZE header" path))

(* ============================================================ *)
(* Quarantine *)
(* ============================================================ *)

(** Parse one non-comment quarantine line: [name issue-url reason...]. *)
let parse_quarantine_line ~dir line =
  let words = String.split_on_char ' ' line |> List.filter (fun w -> w <> "") in
  match words with
  | name :: issue :: _ :: _ ->
      if not (String.starts_with ~prefix:issue_url_prefix issue) then
        Error
          (Printf.sprintf "%s: issue must start with %s" name issue_url_prefix)
      else if not (Sys.file_exists (regression_path ~dir name)) then
        Error (Printf.sprintf "%s: quarantined baseline does not exist" name)
      else Ok (name, issue)
  | _ ->
      Error (Printf.sprintf "malformed line (name issue-url reason): %s" line)

let load_quarantine ~dir =
  let path = Filename.concat dir quarantine_file in
  if not (Sys.file_exists path) then Ok []
  else
    match read_file path with
    | Error msg -> Error (Printf.sprintf "cannot read %s: %s" path msg)
    | Ok content ->
        String.split_on_char '\n' content
        |> List.map String.trim
        |> List.filter (fun l -> l <> "" && l.[0] <> '#')
        |> List.fold_left
             (fun acc line ->
               match (acc, parse_quarantine_line ~dir line) with
               | Error _, _ -> acc
               | Ok _, Error msg -> Error (path ^ ": " ^ msg)
               | Ok entries, Ok (name, issue) ->
                   if List.mem_assoc name entries then
                     Error (Printf.sprintf "%s: duplicate entry %s" path name)
                   else Ok ((name, issue) :: entries))
             (Ok [])

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
  if is_ci () then
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

type verdict =
  | Match
  | Mismatch of (int * string * string) list
  | Missing of string
  | Known_drift of string
  | Unexpected_pass of string

let verify ~dir ~quarantine ~name ~current =
  match load_baseline ~dir name with
  | Error msg -> setup_error "%s" msg
  | Ok None -> Missing (regression_path ~dir name)
  | Ok (Some baseline) -> (
      let differences = compare_screens baseline current in
      match (List.assoc_opt name quarantine, differences) with
      | None, [] -> Match
      | None, diffs -> Mismatch diffs
      | Some issue, [] -> Unexpected_pass issue
      | Some issue, _ -> Known_drift issue)

let check_setup () =
  if !update_mode && is_ci () then
    Error "baseline update mode is refused when CI=true or GITHUB_ACTIONS=true"
  else
    Result.bind (resolve_regression_dir ()) (fun dir ->
        Result.map ignore (load_quarantine ~dir))

let report name current = function
  | Match ->
      Printf.printf "✓ %s: UI matches baseline\n%!" name ;
      true
  | Mismatch diffs ->
      Printf.printf "%s" (format_diff_with_screenshot name diffs current) ;
      false
  | Missing path ->
      Printf.printf
        "❌ Missing baseline %s for %s. Create it explicitly with \
         UPDATE_REGRESSIONS=1 dune exec <test>.exe from the repository root.\n\
         %!"
        path
        name ;
      false
  | Known_drift issue ->
      Printf.printf "⚠ %s: quarantined known drift (%s)\n%!" name issue ;
      true
  | Unexpected_pass issue ->
      Printf.printf
        "❌ %s matches its baseline but is quarantined (%s): remove it from %s\n\
         %!"
        name
        issue
        quarantine_file ;
      false

(** Main regression test function *)
let check_regression test_name =
  let _width, _height, current = capture_screen_with_metadata () in
  (match check_setup () with Ok () -> () | Error msg -> setup_error "%s" msg) ;
  if !update_mode then (
    save_baseline test_name ;
    true)
  else
    let dir = regression_dir () in
    let quarantine =
      match load_quarantine ~dir with
      | Ok q -> q
      | Error msg -> setup_error "%s" msg
    in
    report test_name current (verify ~dir ~quarantine ~name:test_name ~current)

(** Assert that current screen matches baseline *)
let assert_ui_regression test_name =
  if not (check_regression test_name) then
    failwith (Printf.sprintf "UI regression in %s" test_name)

(** Display screenshot in terminal (for CI logs) *)
let display_screenshot content =
  if is_ci () then (
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
let debug_dir () =
  match Sys.getenv_opt "OM_UI_DEBUG_DIR" with
  | Some dir -> dir
  | None -> Filename.concat (Filename.get_temp_dir_name ()) "om-ui-debug"

let capture_debug_screenshot name =
  let dir = debug_dir () in
  let path = Filename.concat dir (name ^ ".debug.screen") in
  ensure_dir dir ;
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
