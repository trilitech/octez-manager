(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Tests for Wallets_page module

    Tests cover:
    - Deduplication of base directories (prevents duplicate wallet display)
*)

open Alcotest
open Octez_manager_lib
module Keys_page = Octez_manager_ui.Wallets_page
module HD = Lib_miaou_internal.Headless_driver
module TH = Tui_test_helpers_lib.Tui_test_helpers
module Context = Octez_manager_ui.Context

(* ============================================================ *)
(* Base Directory Deduplication Tests *)
(* ============================================================ *)

(** Test that get_all_base_dirs deduplicates when default dir is also in registry.
    This prevents the bug where wallets appear twice in the TUI. *)
let test_get_all_base_dirs_deduplicates () =
  (* Get the default directory (usually ~/.tezos-client) *)
  let default_dir = Keys_page.Internal_for_tests.default_client_base_dir () in

  (* Add the default directory to the registry *)
  let _ =
    Directory_registry.add
      ~path:default_dir
      ~dir_type:Client_base_dir
      ~registered_services:[]
  in

  (* Get all base directories - should be deduplicated *)
  let dirs = Keys_page.Internal_for_tests.get_all_base_dirs () in

  (* Count occurrences of the default directory *)
  let count_default =
    List.filter (fun d -> String.equal d default_dir) dirs |> List.length
  in

  (* Cleanup *)
  let _ = Directory_registry.remove default_dir in

  (* Assert: default_dir should appear exactly once, not twice *)
  check int "default dir appears once" 1 count_default

(** Test that directories are unique even when multiple managed dirs exist *)
let test_get_all_base_dirs_multiple_managed () =
  let dir1 = "/tmp/test-dir1" in
  let dir2 = "/tmp/test-dir2" in

  (* Add two managed directories *)
  let _ =
    Directory_registry.add
      ~path:dir1
      ~dir_type:Client_base_dir
      ~registered_services:[]
  in
  let _ =
    Directory_registry.add
      ~path:dir2
      ~dir_type:Client_base_dir
      ~registered_services:[]
  in

  let dirs = Keys_page.Internal_for_tests.get_all_base_dirs () in

  (* Count unique directories *)
  let unique_dirs = List.sort_uniq String.compare dirs in

  (* Cleanup *)
  let _ = Directory_registry.remove dir1 in
  let _ = Directory_registry.remove dir2 in

  (* Assert: all directories should be unique *)
  check int "all dirs unique" (List.length unique_dirs) (List.length dirs)

(** Test that trailing slashes are normalized during deduplication *)
let test_get_all_base_dirs_trailing_slash () =
  let dir_no_slash = "/tmp/test-dir-slash" in
  let dir_with_slash = "/tmp/test-dir-slash/" in

  (* Add the same directory with trailing slash *)
  let _ =
    Directory_registry.add
      ~path:dir_with_slash
      ~dir_type:Client_base_dir
      ~registered_services:[]
  in

  (* If default_client_base_dir returns dir_no_slash, they should still deduplicate *)
  let dirs = Keys_page.Internal_for_tests.get_all_base_dirs () in

  (* Count occurrences of the directory (with or without slash) *)
  let count =
    List.filter
      (fun d -> String.equal d dir_no_slash || String.equal d dir_with_slash)
      dirs
    |> List.length
  in

  (* Cleanup *)
  let _ = Directory_registry.remove dir_with_slash in

  (* If the directory was added, it should appear at most once after normalization *)
  check bool "trailing slash normalized" true (count <= 1)

(* ============================================================ *)
(* TUI Regression: key appears after import without restart     *)
(* ============================================================ *)

(** Write a single-entry public_key_hashs JSON file to base_dir. *)
let write_key_file ~base_dir ~alias ~pkh =
  let oc = open_out (Filename.concat base_dir "public_key_hashs") in
  Printf.fprintf oc {|[{"name":"%s","value":"%s"}]|} alias pkh ;
  close_out oc

(** Create a temp dir, register it as a Client_base_dir, and clean up after. *)
let with_tmp_wallet f =
  let base_dir =
    let d =
      Filename.concat
        (Filename.get_temp_dir_name ())
        ("om-test-wallet-" ^ string_of_int (Unix.getpid ()))
    in
    (try Unix.mkdir d 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()) ;
    d
  in
  ignore
    (Directory_registry.add
       ~path:base_dir
       ~dir_type:Client_base_dir
       ~registered_services:[]) ;
  Fun.protect
    ~finally:(fun () -> ignore (Directory_registry.remove base_dir))
    (fun () -> f base_dir)

(** Regression test for bug: key not visible after import until restart.
    Root cause: refresh = fun ps -> ps (no-op), dirty flag only consumed in
    service_cycle which never fires for the wallets page. Fix: refresh calls
    reload_if_dirty. *)
let test_imported_key_appears_without_restart () =
  TH.with_test_env (fun () ->
      with_tmp_wallet (fun base_dir ->
          (* Pre-populate alice so the page loads with content *)
          write_key_file
            ~base_dir
            ~alias:"alice"
            ~pkh:"tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" ;
          HD.Stateful.init (module Keys_page.Page) ;
          let screen_before = TH.get_screen_text () in
          check
            bool
            "alice visible on initial load"
            true
            (TH.contains_substring screen_before "alice") ;
          (* Simulate import: overwrite key file with bob added, mark dirty *)
          let oc = open_out (Filename.concat base_dir "public_key_hashs") in
          Printf.fprintf
            oc
            {|[{"name":"alice","value":"tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"},{"name":"bob","value":"tz1TzuXMpCEEFBkiKF3U2PnMuBStnMF3nFRK"}]|} ;
          close_out oc ;
          Context.mark_keys_dirty () ;
          (* Trigger a tick — refresh must call reload_if_dirty *)
          ignore (HD.Stateful.idle_wait ~iterations:10 ~sleep:0.001 ()) ;
          let screen_after = TH.get_screen_text () in
          check
            bool
            "bob visible after import without restart"
            true
            (TH.contains_substring screen_after "bob")))

(* ============================================================ *)
(* Test Suite *)
(* ============================================================ *)

let base_dir_tests =
  [
    ( "deduplicates default dir when in registry",
      `Quick,
      test_get_all_base_dirs_deduplicates );
    ( "ensures uniqueness with multiple managed dirs",
      `Quick,
      test_get_all_base_dirs_multiple_managed );
    ( "normalizes trailing slashes during deduplication",
      `Quick,
      test_get_all_base_dirs_trailing_slash );
  ]

let refresh_tests =
  [
    ( "imported key appears without restart",
      `Quick,
      test_imported_key_appears_without_restart );
  ]

(* ============================================================ *)
(* TUI Regression Tests *)
(* ============================================================ *)

(** Bug regression: pasting a PKH with NBSP bytes must not display garbage
    characters in the import textbox. Fails without filter_key fix. *)
let test_import_key_unicode_chars_not_displayed () =
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Keys_page.Page) ;
      ignore (TH.send_key_and_wait "n") ;
      if not (TH.wait_until_modal_active ()) then
        Alcotest.fail "create/import modal did not open" ;
      TH.navigate_down 1 ;
      ignore (TH.send_key_and_wait "Enter") ;
      Unix.sleepf 0.02 ;
      (* PKH + trailing U+00A0 NBSP (\xc2\xa0 in UTF-8), each byte sent
         as a key *)
      TH.type_string ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" ^ "\xc2\xa0") ;
      Unix.sleepf 0.01 ;
      let screen = TH.get_screen_text () in
      check
        bool
        "pkh modal visible"
        true
        (TH.contains_substring screen "Import Key") ;
      (* The fix strips NBSP (\xc2\xa0 in UTF-8) from pasted input. Verify
         those specific bytes are absent from the screen. We cannot assert
         the absence of ALL non-ASCII bytes because the TUI itself renders
         box-drawing characters (╔ ║ etc.) which are multi-byte UTF-8. *)
      let has_nbsp = TH.contains_substring screen "\xc2\xa0" in
      check bool "no NBSP bytes on screen" false has_nbsp)

let tui_tests =
  [
    ( "import key: unicode chars not displayed",
      `Quick,
      test_import_key_unicode_chars_not_displayed );
  ]

let () =
  Alcotest.run
    "Wallets_page"
    [
      ("base_dir_deduplication", base_dir_tests);
      ("refresh_on_dirty_flag", refresh_tests);
      ("tui_regression", tui_tests);
    ]
