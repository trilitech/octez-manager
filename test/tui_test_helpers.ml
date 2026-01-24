(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** TUI Test Helpers

    Reusable utilities for TUI integration tests using the miaou headless driver.
    Provides:
    - Environment isolation
    - Modal waiting with retries
    - Character-by-character text input
    - Screen content assertions with diff
    - Form navigation helpers
*)

module HD = Lib_miaou_internal.Headless_driver
module Modal_manager = Miaou.Core.Modal_manager

(* ============================================================ *)
(* Mock System Capability for Tests *)
(* ============================================================ *)

(** Mock Miaou System capability that provides minimal functionality for TUI tests
    without requiring actual system commands or file I/O *)
let mock_miaou_system : Miaou_interfaces.System.t =
  {
    file_exists = (fun _path -> true);
    is_directory = (fun _path -> true);
    read_file = (fun _path -> Ok "mock file content");
    write_file = (fun _path _content -> Ok ());
    mkdir = (fun _path -> Ok ());
    run_command =
      (fun ~argv:_ ~cwd:_ ->
        Ok {Miaou_interfaces.System.exit_code = 0; stdout = ""; stderr = ""});
    get_current_user_info = (fun () -> Ok ("testuser", "testgroup"));
    get_disk_usage = (fun ~path:_ -> Ok 1000000L);
    list_dir = (fun _path -> Ok ["."; ".."; "test"]);
    probe_writable = (fun ~path:_ -> Ok true);
    get_env_var = Sys.getenv_opt;
  }

(* ============================================================ *)
(* Environment Setup *)
(* ============================================================ *)

let test_root = ref ""

(** Setup isolated test environment with temporary directories *)
let setup_test_env () =
  let tmp_dir =
    Filename.get_temp_dir_name ()
    ^ "/octez-manager-tui-test-"
    ^ string_of_int (Unix.getpid ())
    ^ "-"
    ^ string_of_int (int_of_float (Unix.gettimeofday () *. 1000.) mod 100000)
  in
  let rec ensure_dir path =
    if path = "." || path = "/" || path = "" then ()
    else
      let parent = Filename.dirname path in
      if parent <> path then ensure_dir parent ;
      try Unix.mkdir path 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  in
  ensure_dir tmp_dir ;
  let set_env key subdir =
    let path = Filename.concat tmp_dir subdir in
    ensure_dir path ;
    Unix.putenv key path ;
    path
  in
  ignore (set_env "HOME" "home") ;
  ignore (set_env "XDG_CONFIG_HOME" "config") ;
  ignore (set_env "XDG_DATA_HOME" "data") ;
  ignore (set_env "XDG_STATE_HOME" "state") ;
  test_root := tmp_dir ;
  (* Register capabilities - this registers all octez-manager capabilities *)
  Octez_manager_lib.Capabilities.register () ;
  (* Register mock Miaou System capability for file browser widget *)
  (* This provides the System capability that Miaou's file browser needs *)
  Miaou_interfaces.System.set mock_miaou_system ;
  (* Reset headless driver state *)
  HD.set_size 24 80 ;
  HD.set_limits ~iterations:500 ~seconds:10.0 () ;
  HD.Key_queue.clear () ;
  HD.Screen.clear () ;
  Modal_manager.clear () ;
  tmp_dir

(** Cleanup test environment *)
let cleanup_test_env tmp_dir =
  let rec rm_rf path =
    if Sys.file_exists path then
      if Sys.is_directory path then (
        Sys.readdir path
        |> Array.iter (fun name -> rm_rf (Filename.concat path name)) ;
        try Unix.rmdir path with _ -> ())
      else try Sys.remove path with _ -> ()
  in
  rm_rf tmp_dir

(** Run a test with isolated environment *)
let with_test_env f =
  let tmp_dir = setup_test_env () in
  Fun.protect ~finally:(fun () -> cleanup_test_env tmp_dir) f

(* ============================================================ *)
(* Modal Helpers *)
(* ============================================================ *)

(** Wait until a modal becomes active, with polling and retries *)
let rec wait_until_modal_active ?(iterations = 100) () =
  if Modal_manager.has_active () then true
  else if iterations <= 0 then false
  else (
    Unix.sleepf 0.001 ;
    wait_until_modal_active ~iterations:(iterations - 1) ())

(** Wait until no modal is active *)
let rec wait_until_no_modal ?(iterations = 100) () =
  if not (Modal_manager.has_active ()) then true
  else if iterations <= 0 then false
  else (
    Unix.sleepf 0.001 ;
    wait_until_no_modal ~iterations:(iterations - 1) ())

(** Send a key and wait briefly for state to settle *)
let send_key_and_wait ?(wait_iterations = 2) key =
  let result = HD.Stateful.send_key key in
  ignore (HD.Stateful.idle_wait ~iterations:wait_iterations ~sleep:0.0 ()) ;
  result

(** Try to open a modal by sending Enter, with retries *)
let try_open_modal ?(max_attempts = 20) () =
  let rec loop attempts =
    if Modal_manager.has_active () then true
    else if attempts <= 0 then false
    else (
      ignore (HD.Stateful.send_key "Enter") ;
      ignore (HD.Stateful.idle_wait ~iterations:2 ~sleep:0.0 ()) ;
      if Modal_manager.has_active () then true else loop (attempts - 1))
  in
  loop max_attempts

(* ============================================================ *)
(* Text Input Helpers *)
(* ============================================================ *)

(** Type a string character by character *)
let type_string s =
  String.iter
    (fun c ->
      ignore (HD.Stateful.send_key (String.make 1 c)) ;
      ignore (HD.Stateful.idle_wait ~iterations:1 ~sleep:0.0 ()))
    s

(** Type a string and press Enter to confirm *)
let type_and_confirm s =
  type_string s ;
  ignore (send_key_and_wait "Enter")

(* ============================================================ *)
(* Navigation Helpers *)
(* ============================================================ *)

(** Navigate down n times *)
let navigate_down ?(wait = true) n =
  for _ = 1 to n do
    if wait then ignore (send_key_and_wait "Down")
    else ignore (HD.Stateful.send_key "Down")
  done

(** Navigate up n times *)
let navigate_up ?(wait = true) n =
  for _ = 1 to n do
    if wait then ignore (send_key_and_wait "Up")
    else ignore (HD.Stateful.send_key "Up")
  done

(* ============================================================ *)
(* Screen Content Helpers *)
(* ============================================================ *)

(** Check if string contains substring *)
let contains_substring haystack needle =
  let nlen = String.length needle in
  let hlen = String.length haystack in
  if nlen = 0 then true
  else if nlen > hlen then false
  else
    let rec loop i =
      if i + nlen > hlen then false
      else if String.sub haystack i nlen = needle then true
      else loop (i + 1)
    in
    loop 0

(** Strip ANSI escape codes from string *)
let strip_ansi s =
  let buf = Buffer.create (String.length s) in
  let len = String.length s in
  let rec loop i =
    if i >= len then Buffer.contents buf
    else if s.[i] = '\027' then
      let rec skip j =
        if j >= len then j
        else
          let c = s.[j] in
          if (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') then j + 1
          else skip (j + 1)
      in
      loop (skip (i + 1))
    else (
      Buffer.add_char buf s.[i] ;
      loop (i + 1))
  in
  loop 0

(** Get current screen content with ANSI stripped *)
let get_screen_text () = strip_ansi (HD.get_screen_content ())

(** Line-by-line diff for snapshot comparison *)
let diff_strings ~expected ~actual =
  let el = String.split_on_char '\n' expected in
  let al = String.split_on_char '\n' actual in
  let max_len = max (List.length el) (List.length al) in
  let get l i = match List.nth_opt l i with Some s -> s | None -> "" in
  let buf = Buffer.create 256 in
  for i = 0 to max_len - 1 do
    let e = get el i in
    let a = get al i in
    if not (String.equal e a) then
      Buffer.add_string
        buf
        (Printf.sprintf "Line %d:\n- %s\n+ %s\n" (i + 1) e a)
  done ;
  Buffer.contents buf

(** Assert screen contains expected text *)
let assert_screen_contains text =
  let screen = get_screen_text () in
  if not (contains_substring screen text) then
    Alcotest.fail
      (Printf.sprintf
         "Screen does not contain '%s'.\nActual screen:\n%s"
         text
         screen)

(** Assert screen matches expected snapshot (with diff on failure) *)
let assert_screen_snapshot expected =
  let actual = get_screen_text () in
  if not (String.equal (String.trim expected) (String.trim actual)) then
    let diff = diff_strings ~expected ~actual in
    Alcotest.fail ("Screen snapshot mismatch:\n" ^ diff)

(* ============================================================ *)
(* Flow Drivers *)
(* ============================================================ *)

(** Drive through the create menu from instances page.
    Opens create menu with 'c', selects an item, returns result. *)
let drive_create_menu_selection ~item_index () =
  (* Open create menu *)
  ignore (send_key_and_wait "c") ;
  if not (wait_until_modal_active ()) then
    Alcotest.fail "Create menu modal did not open" ;
  (* Navigate to item *)
  navigate_down item_index ;
  (* Select item *)
  let result = send_key_and_wait "Enter" in
  result

(** Drive through opening an instance action menu.
    Navigates to the instance (by index) and presses Enter. *)
let drive_instance_action ~instance_index () =
  (* Navigate past the Install menu item to services *)
  navigate_down (instance_index + 2) ;
  (* services start at index 2 *)
  (* Open action menu *)
  ignore (send_key_and_wait "Enter") ;
  wait_until_modal_active ()
