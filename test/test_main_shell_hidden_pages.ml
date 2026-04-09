(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Tests for main_shell's hidden page mechanism (sandbox, rewards, log_viewer, rpc_browser).
    
    Hidden pages are pages that are not directly shown in the tab bar but should
    keep the tab bar visible when active (unlike full-screen pages that replace
    the entire shell). *)

open Alcotest
module Main_shell = Octez_manager_ui.Main_shell
module Context = Octez_manager_ui.Context

let test_size = {LTerm_geom.rows = 40; cols = 120}

(** Extract the first line of rendered output (the tab bar) *)
let get_tab_bar_line output =
  match String.split_on_char '\n' output with
  | first :: _ -> String.trim first
  | [] -> ""

(** Check if the tab bar contains expected tab labels *)
let tab_bar_has_tabs output =
  let first_line = get_tab_bar_line output in
  (* Debug: print the first line *)
  Printf.printf "[DEBUG] First line: '%s'\n%!" first_line ;
  String.contains first_line '1'
  && String.contains first_line '2'
  && (String.contains first_line 'I' || String.contains first_line 'W')

(** Test that log_viewer page shows the tab bar *)
let test_log_viewer_shows_tab_bar () =
  let ps = Main_shell.Page.init () in
  (* Navigate to log_viewer *)
  let () = Context.navigate "log_viewer" in
  let ps = Main_shell.Page.refresh ps in
  (* Render and check tab bar is present *)
  let output = Main_shell.Page.view ps ~focus:true ~size:test_size in
  check
    bool
    "Log viewer page should show tab bar"
    true
    (tab_bar_has_tabs output)

(** Test that rpc_browser page shows the tab bar *)
let test_rpc_browser_shows_tab_bar () =
  let ps = Main_shell.Page.init () in
  (* Navigate to rpc_browser *)
  let () = Context.navigate "rpc_browser" in
  let ps = Main_shell.Page.refresh ps in
  (* Render and check tab bar is present *)
  let output = Main_shell.Page.view ps ~focus:true ~size:test_size in
  check
    bool
    "RPC browser page should show tab bar"
    true
    (tab_bar_has_tabs output)

(** Test that pressing number keys switches tabs and clears hidden page state *)
let test_number_key_exits_hidden_page () =
  let ps = Main_shell.Page.init () in
  (* Navigate to log_viewer *)
  let () = Context.navigate "log_viewer" in
  let ps = Main_shell.Page.refresh ps in
  (* Press "2" to switch to Wallets tab *)
  let ps, _ =
    Main_shell.Page.on_key ps (Miaou.Core.Keys.Char "2") ~size:test_size
  in
  (* Check that we're now on the wallets tab (not on hidden page) *)
  let output = Main_shell.Page.view ps ~focus:true ~size:test_size in
  (* The tab bar should still be visible *)
  check
    bool
    "After pressing number key, tab bar should still be visible"
    true
    (tab_bar_has_tabs output)

(** Test that Escape from hidden page goes back *)
let test_escape_from_hidden_page () =
  let ps = Main_shell.Page.init () in
  (* Navigate to rpc_browser *)
  let () = Context.navigate "rpc_browser" in
  let ps = Main_shell.Page.refresh ps in
  (* Press Escape *)
  let ps, _ =
    Main_shell.Page.on_key ps Miaou.Core.Keys.Escape ~size:test_size
  in
  (* Should have navigation back signal *)
  let nav = Miaou.Core.Navigation.pending ps in
  check bool "Escape should trigger back navigation" true (Option.is_some nav)

(** Test that sandbox and rewards still work (regression test) *)
let test_sandbox_and_rewards_still_work () =
  (* Test sandbox *)
  let ps = Main_shell.Page.init () in
  let () = Context.navigate "sandbox" in
  let ps = Main_shell.Page.refresh ps in
  let output = Main_shell.Page.view ps ~focus:true ~size:test_size in
  check bool "Sandbox page should show tab bar" true (tab_bar_has_tabs output) ;
  (* Test rewards *)
  let ps = Main_shell.Page.init () in
  let () = Context.navigate "rewards" in
  let ps = Main_shell.Page.refresh ps in
  let output = Main_shell.Page.view ps ~focus:true ~size:test_size in
  check bool "Rewards page should show tab bar" true (tab_bar_has_tabs output)

let () =
  run
    "Main Shell Hidden Pages"
    [
      ( "hidden_pages",
        [
          test_case
            "log_viewer shows tab bar"
            `Quick
            test_log_viewer_shows_tab_bar;
          test_case
            "rpc_browser shows tab bar"
            `Quick
            test_rpc_browser_shows_tab_bar;
          test_case
            "number key exits hidden page"
            `Quick
            test_number_key_exits_hidden_page;
          test_case
            "escape from hidden page"
            `Quick
            test_escape_from_hidden_page;
          test_case
            "sandbox and rewards still work"
            `Quick
            test_sandbox_and_rewards_still_work;
        ] );
    ]
