(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Unit tests for Binaries_page pure functions.

    Covers format_size, build_items, move_up, move_down, toggle expansions,
    and filter_latest_n_major_versions. *)

open Alcotest
module BP = Octez_manager_ui.Binaries_page

let mk_version version =
  Octez_manager_lib.Binary_downloader.
    {version; release_date = None; is_rc = false}

let mk_registered alias path =
  (Octez_manager_lib.Binary_registry.{alias; path}, 0)

let mk_state ?(managed_octez = []) ?(managed_signatory = []) ?(registered = [])
    ?(available_octez = []) ?(available_signatory = [])
    ?(expanded_octez_majors = []) ?(expanded_managed_octez_items = [])
    ?(expanded_registered = []) ?(expanded_managed_octez = true)
    ?(expanded_managed_signatory = true) ?(expanded_available_octez = true)
    ?(expanded_available_signatory = true) ?(selected = 0) () =
  let items =
    BP.For_tests.build_items
      managed_octez
      managed_signatory
      registered
      available_octez
      available_signatory
      ~expanded_managed_octez
      ~expanded_managed_signatory
      ~expanded_available_octez
      ~expanded_available_signatory
      ~expanded_octez_majors
  in
  BP.
    {
      managed_octez_versions = managed_octez;
      managed_signatory_versions = managed_signatory;
      registered_dirs = registered;
      available_octez_versions = available_octez;
      available_signatory_versions = available_signatory;
      items;
      selected;
      loading_remote = false;
      expanded_managed_octez;
      expanded_managed_signatory;
      expanded_available_octez;
      expanded_available_signatory;
      expanded_octez_majors;
      expanded_managed_octez_items;
      expanded_registered;
    }

(* ── format_size ──────────────────────────────────────────────── *)

let test_format_size_zero () =
  check string "0B" "0 bytes" (BP.For_tests.format_size 0L)

let test_format_size_small () =
  check string "512B" "512 bytes" (BP.For_tests.format_size 512L)

let test_format_size_kb () =
  check string "1KB" "1 KB" (BP.For_tests.format_size 1024L)

let test_format_size_mb () =
  check string "1MB" "1 MB" (BP.For_tests.format_size 1048576L)

let test_format_size_gb () =
  check string "1GB" "1 GB" (BP.For_tests.format_size 1073741824L)

let test_format_size_500mb () =
  check
    string
    "500MB"
    "500 MB"
    (BP.For_tests.format_size (Int64.mul 500L 1048576L))

let test_format_size_integer_division () =
  (* 1500 bytes: kb = 1500/1024 = 1, mb = 0 → "1 KB" *)
  check string "rounds down" "1 KB" (BP.For_tests.format_size 1500L)

(* ── build_items ──────────────────────────────────────────────── *)

let count_item_type items pred = List.length (List.filter pred items)

let is_register_action = function BP.RegisterAction -> true | _ -> false

let is_registered_dir = function BP.RegisteredDir _ -> true | _ -> false

let is_managed_version = function BP.ManagedVersion _ -> true | _ -> false

let is_managed_group = function BP.ManagedGroup _ -> true | _ -> false

let is_available_group = function BP.AvailableGroup _ -> true | _ -> false

let is_major_group = function BP.AvailableMajorGroup _ -> true | _ -> false

let is_available_version = function BP.AvailableVersion _ -> true | _ -> false

let test_build_items_empty () =
  let items =
    BP.For_tests.build_items
      []
      []
      []
      []
      []
      ~expanded_managed_octez:true
      ~expanded_managed_signatory:true
      ~expanded_available_octez:true
      ~expanded_available_signatory:true
      ~expanded_octez_majors:[]
  in
  (* Groups + RegisterAction *)
  check int "one register action" 1 (count_item_type items is_register_action) ;
  check int "two managed groups" 2 (count_item_type items is_managed_group) ;
  check int "two available groups" 2 (count_item_type items is_available_group)

let test_build_items_with_registered () =
  let reg =
    [mk_registered "dev" "/home/dev/octez"; mk_registered "custom" "/opt/octez"]
  in
  let items =
    BP.For_tests.build_items
      []
      []
      reg
      []
      []
      ~expanded_managed_octez:true
      ~expanded_managed_signatory:true
      ~expanded_available_octez:true
      ~expanded_available_signatory:true
      ~expanded_octez_majors:[]
  in
  check int "two registered dirs" 2 (count_item_type items is_registered_dir) ;
  check int "one register action" 1 (count_item_type items is_register_action)

let test_build_items_with_managed () =
  let managed_octez = [("24.0", Some 1000L, 2); ("23.0", Some 500L, 0)] in
  let items =
    BP.For_tests.build_items
      managed_octez
      []
      []
      []
      []
      ~expanded_managed_octez:true
      ~expanded_managed_signatory:true
      ~expanded_available_octez:true
      ~expanded_available_signatory:true
      ~expanded_octez_majors:[]
  in
  check int "two managed versions" 2 (count_item_type items is_managed_version) ;
  check int "one register action" 1 (count_item_type items is_register_action)

let test_build_items_with_available () =
  let available_octez =
    [mk_version "24.0"; mk_version "24.1"; mk_version "23.0"]
  in
  let items =
    BP.For_tests.build_items
      []
      []
      []
      available_octez
      []
      ~expanded_managed_octez:true
      ~expanded_managed_signatory:true
      ~expanded_available_octez:true
      ~expanded_available_signatory:true
      ~expanded_octez_majors:[]
  in
  (* Should have major groups for 24 and 23 *)
  check int "two major groups" 2 (count_item_type items is_major_group) ;
  (* No expanded sub-items *)
  check
    int
    "no expanded versions"
    0
    (count_item_type items is_available_version)

let test_build_items_expanded_major () =
  let available_octez = [mk_version "24.0"; mk_version "24.1"] in
  let items =
    BP.For_tests.build_items
      []
      []
      []
      available_octez
      []
      ~expanded_managed_octez:true
      ~expanded_managed_signatory:true
      ~expanded_available_octez:true
      ~expanded_available_signatory:true
      ~expanded_octez_majors:[24]
  in
  check int "one major group" 1 (count_item_type items is_major_group) ;
  check
    int
    "two expanded versions"
    2
    (count_item_type items is_available_version)

let test_build_items_unexpanded_major () =
  let available_octez = [mk_version "24.0"; mk_version "24.1"] in
  let items =
    BP.For_tests.build_items
      []
      []
      []
      available_octez
      []
      ~expanded_managed_octez:true
      ~expanded_managed_signatory:true
      ~expanded_available_octez:true
      ~expanded_available_signatory:true
      ~expanded_octez_majors:[23]
  in
  (* 23 is expanded but has no versions; 24 is not expanded *)
  check
    int
    "no expanded versions"
    0
    (count_item_type items is_available_version)

(* ── move_up / move_down ──────────────────────────────────────── *)

let test_move_up_from_zero () =
  let s = mk_state () in
  let s' = BP.For_tests.move_up s in
  check int "stays at 0" 0 s'.selected

let test_move_up_from_nonzero () =
  let s = mk_state ~selected:2 () in
  let s' = BP.For_tests.move_up s in
  check int "decrements" 1 s'.selected

let test_move_down_from_zero () =
  let s = mk_state () in
  let s' = BP.For_tests.move_down s in
  (* items has 1 RegisterAction item, max_idx = 0 *)
  check int "stays at 0" 0 s'.selected

let test_move_down_at_end () =
  let s = mk_state ~selected:0 () in
  (* items = [RegisterAction], max_idx = 0 *)
  let s' = BP.For_tests.move_down s in
  check int "stays at end" 0 s'.selected

let test_move_down_with_versions () =
  let available_octez = [mk_version "24.0"] in
  let s = mk_state ~available_octez () in
  let max_idx = List.length s.items - 1 in
  let s' = BP.For_tests.move_down s in
  check int "moves down" 1 s'.selected ;
  check bool "within bounds" true (s'.selected <= max_idx)

(* ── toggle_major_expansion ───────────────────────────────────── *)

let test_toggle_major_expand () =
  let available_octez = [mk_version "24.0"; mk_version "24.1"] in
  let s = mk_state ~available_octez () in
  check
    int
    "initially no expanded versions"
    0
    (count_item_type s.items is_available_version) ;
  let s' = BP.For_tests.toggle_major_expansion s 24 in
  check bool "24 is expanded" true (List.mem 24 s'.expanded_octez_majors) ;
  check
    int
    "versions now visible"
    2
    (count_item_type s'.items is_available_version)

let test_toggle_major_collapse () =
  let available_octez = [mk_version "24.0"] in
  let s = mk_state ~available_octez ~expanded_octez_majors:[24] () in
  check
    int
    "initially expanded"
    1
    (count_item_type s.items is_available_version) ;
  let s' = BP.For_tests.toggle_major_expansion s 24 in
  check bool "24 is collapsed" false (List.mem 24 s'.expanded_octez_majors) ;
  check int "versions hidden" 0 (count_item_type s'.items is_available_version)

(* ── toggle_managed_expansion ─────────────────────────────────── *)

let test_toggle_managed_expand () =
  let s = mk_state () in
  let s' = BP.For_tests.toggle_managed_expansion s "24.0" in
  check bool "expanded" true (List.mem "24.0" s'.expanded_managed_octez_items)

let test_toggle_managed_collapse () =
  let s = mk_state ~expanded_managed_octez_items:["24.0"] () in
  let s' = BP.For_tests.toggle_managed_expansion s "24.0" in
  check bool "collapsed" false (List.mem "24.0" s'.expanded_managed_octez_items)

(* ── toggle_registered_expansion ──────────────────────────────── *)

let test_toggle_registered_expand () =
  let s = mk_state () in
  let s' = BP.For_tests.toggle_registered_expansion s "dev" in
  check bool "expanded" true (List.mem "dev" s'.expanded_registered)

let test_toggle_registered_collapse () =
  let s = mk_state ~expanded_registered:["dev"] () in
  let s' = BP.For_tests.toggle_registered_expansion s "dev" in
  check bool "collapsed" false (List.mem "dev" s'.expanded_registered)

(* ── filter_latest_n_major_versions ───────────────────────────── *)

let test_filter_empty () =
  let result = BP.For_tests.filter_latest_n_major_versions 2 [] in
  check int "empty" 0 (List.length result)

let test_filter_single_major () =
  let versions = [mk_version "24.0"; mk_version "24.1"] in
  let result = BP.For_tests.filter_latest_n_major_versions 1 versions in
  check int "keeps all v24" 2 (List.length result)

let test_filter_two_majors_keep_one () =
  let versions = [mk_version "24.0"; mk_version "24.1"; mk_version "23.0"] in
  let result = BP.For_tests.filter_latest_n_major_versions 1 versions in
  (* Should keep only v24 *)
  List.iter
    (fun (v : Octez_manager_lib.Binary_downloader.version_info) ->
      check
        bool
        "is v24"
        true
        (String.length v.version >= 2 && String.sub v.version 0 2 = "24"))
    result ;
  check int "two v24 versions" 2 (List.length result)

let test_filter_two_majors_keep_two () =
  let versions = [mk_version "24.0"; mk_version "23.0"; mk_version "22.0"] in
  let result = BP.For_tests.filter_latest_n_major_versions 2 versions in
  check int "keeps v24 and v23" 2 (List.length result) ;
  let has_22 =
    List.exists
      (fun (v : Octez_manager_lib.Binary_downloader.version_info) ->
        String.sub v.version 0 2 = "22")
      result
  in
  check bool "no v22" false has_22

let test_filter_n_larger_than_majors () =
  let versions = [mk_version "24.0"] in
  let result = BP.For_tests.filter_latest_n_major_versions 5 versions in
  check int "keeps all" 1 (List.length result)

(* ── Suite ────────────────────────────────────────────────────── *)

let () =
  run
    "Binaries_page_pure"
    [
      ( "format_size",
        [
          test_case "zero" `Quick test_format_size_zero;
          test_case "small" `Quick test_format_size_small;
          test_case "KB" `Quick test_format_size_kb;
          test_case "MB" `Quick test_format_size_mb;
          test_case "GB" `Quick test_format_size_gb;
          test_case "500MB" `Quick test_format_size_500mb;
          test_case "integer division" `Quick test_format_size_integer_division;
        ] );
      ( "build_items",
        [
          test_case "empty" `Quick test_build_items_empty;
          test_case "with managed" `Quick test_build_items_with_managed;
          test_case "with registered" `Quick test_build_items_with_registered;
          test_case "with available" `Quick test_build_items_with_available;
          test_case "expanded major" `Quick test_build_items_expanded_major;
          test_case "unexpanded major" `Quick test_build_items_unexpanded_major;
        ] );
      ( "move_up_down",
        [
          test_case "up from 0" `Quick test_move_up_from_zero;
          test_case "up from nonzero" `Quick test_move_up_from_nonzero;
          test_case "down from 0" `Quick test_move_down_from_zero;
          test_case "down at end" `Quick test_move_down_at_end;
          test_case "down with versions" `Quick test_move_down_with_versions;
        ] );
      ( "toggle_major_expansion",
        [
          test_case "expand" `Quick test_toggle_major_expand;
          test_case "collapse" `Quick test_toggle_major_collapse;
        ] );
      ( "toggle_managed_expansion",
        [
          test_case "expand" `Quick test_toggle_managed_expand;
          test_case "collapse" `Quick test_toggle_managed_collapse;
        ] );
      ( "toggle_registered_expansion",
        [
          test_case "expand" `Quick test_toggle_registered_expand;
          test_case "collapse" `Quick test_toggle_registered_collapse;
        ] );
      ( "filter_latest_n_major_versions",
        [
          test_case "empty" `Quick test_filter_empty;
          test_case "single major" `Quick test_filter_single_major;
          test_case "two majors keep one" `Quick test_filter_two_majors_keep_one;
          test_case "two majors keep two" `Quick test_filter_two_majors_keep_two;
          test_case
            "n larger than majors"
            `Quick
            test_filter_n_larger_than_majors;
        ] );
    ]
