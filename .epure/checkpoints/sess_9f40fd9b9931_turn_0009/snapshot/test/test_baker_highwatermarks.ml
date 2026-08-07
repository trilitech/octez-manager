(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Tests for Baker_highwatermarks module.

    Covers parse_highwatermark, parse_entry, parse_entries,
    read_file, max_level, format_summary, and cache operations. *)

open Alcotest
module BH = Octez_manager_ui.Baker_highwatermarks

(* ── Helpers ─────────────────────────────────────────────────── *)

let write_temp_file content =
  let path = Filename.temp_file "hwm_test_" ".json" in
  let oc = open_out path in
  output_string oc content ;
  close_out oc ;
  path

(* ── parse_highwatermark ─────────────────────────────────────── *)

let test_parse_hwm_valid () =
  match
    BH.For_tests.parse_highwatermark
      (Yojson.Safe.from_string {|{"round": 3, "level": 5000}|})
  with
  | None -> fail "should parse valid highwatermark"
  | Some hwm ->
      check int "round" 3 hwm.round ;
      check int "level" 5000 hwm.level

let test_parse_hwm_zero () =
  match
    BH.For_tests.parse_highwatermark
      (Yojson.Safe.from_string {|{"round": 0, "level": 0}|})
  with
  | None -> fail "should parse zero hwm"
  | Some hwm ->
      check int "round" 0 hwm.round ;
      check int "level" 0 hwm.level

let test_parse_hwm_missing_round () =
  check
    bool
    "None"
    true
    (BH.For_tests.parse_highwatermark
       (Yojson.Safe.from_string {|{"level": 100}|})
    = None)

let test_parse_hwm_missing_level () =
  check
    bool
    "None"
    true
    (BH.For_tests.parse_highwatermark (Yojson.Safe.from_string {|{"round": 1}|})
    = None)

let test_parse_hwm_empty () =
  check
    bool
    "None"
    true
    (BH.For_tests.parse_highwatermark (Yojson.Safe.from_string "{}") = None)

let test_parse_hwm_string () =
  check
    bool
    "None"
    true
    (BH.For_tests.parse_highwatermark
       (Yojson.Safe.from_string {|"not an object"|})
    = None)

let test_parse_hwm_null () =
  check
    bool
    "None"
    true
    (BH.For_tests.parse_highwatermark (Yojson.Safe.from_string "null") = None)

(* ── parse_entry ─────────────────────────────────────────────── *)

let test_parse_entry_valid () =
  match
    BH.For_tests.parse_entry
      (Yojson.Safe.from_string
         {|{
      "delegate": "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb",
      "highwatermark": {"round": 0, "level": 4500}
    }|})
  with
  | None -> fail "should parse valid entry"
  | Some (delegate, hwm) -> (
      check string "delegate" "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb" delegate ;
      match hwm with
      | None -> fail "should have highwatermark"
      | Some h ->
          check int "level" 4500 h.level ;
          check int "round" 0 h.round)

let test_parse_entry_invalid_hwm () =
  match
    BH.For_tests.parse_entry
      (Yojson.Safe.from_string
         {|{
      "delegate": "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb",
      "highwatermark": {}
    }|})
  with
  | None -> fail "should parse entry with invalid hwm"
  | Some (delegate, hwm) ->
      check string "delegate" "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb" delegate ;
      check bool "hwm is None" true (hwm = None)

let test_parse_entry_missing_delegate () =
  check
    bool
    "None"
    true
    (BH.For_tests.parse_entry
       (Yojson.Safe.from_string
          {|{"highwatermark": {"round": 0, "level": 100}}|})
    = None)

let test_parse_entry_non_object () =
  check
    bool
    "None"
    true
    (BH.For_tests.parse_entry (Yojson.Safe.from_string "42") = None)

(* ── parse_entries ───────────────────────────────────────────── *)

let test_parse_entries_list () =
  let entries =
    BH.For_tests.parse_entries
      (Yojson.Safe.from_string
         {|[
      {"delegate": "tz1aaa", "highwatermark": {"round": 0, "level": 1000}},
      {"delegate": "tz1bbb", "highwatermark": {"round": 1, "level": 2000}},
      {"delegate": "tz1ccc", "highwatermark": {"round": 0, "level": 3000}}
    ]|})
  in
  check int "three entries" 3 (List.length entries) ;
  let delegates = List.map fst entries in
  check bool "has tz1aaa" true (List.mem "tz1aaa" delegates) ;
  check bool "has tz1bbb" true (List.mem "tz1bbb" delegates)

let test_parse_entries_empty () =
  let entries = BH.For_tests.parse_entries (Yojson.Safe.from_string "[]") in
  check int "empty" 0 (List.length entries)

let test_parse_entries_not_list () =
  let entries =
    BH.For_tests.parse_entries (Yojson.Safe.from_string {|{"not": "a list"}|})
  in
  check int "non-list" 0 (List.length entries)

let test_parse_entries_skips_invalid () =
  let entries =
    BH.For_tests.parse_entries
      (Yojson.Safe.from_string
         {|[
      {"delegate": "tz1aaa", "highwatermark": {"round": 0, "level": 100}},
      {"invalid": "entry"},
      {"delegate": "tz1bbb", "highwatermark": {"round": 0, "level": 200}}
    ]|})
  in
  check int "skips invalid" 2 (List.length entries)

(* ── read_file ───────────────────────────────────────────────── *)

let test_read_file_full () =
  let content =
    {|{
    "blocks": [
      {"delegate": "tz1VSU", "highwatermark": {"round": 0, "level": 5000}},
      {"delegate": "tz1RJ4", "highwatermark": {"round": 1, "level": 4998}}
    ],
    "preattestations": [
      {"delegate": "tz1VSU", "highwatermark": {"round": 0, "level": 5001}}
    ],
    "attestations": [
      {"delegate": "tz1VSU", "highwatermark": {"round": 0, "level": 5001}},
      {"delegate": "tz1RJ4", "highwatermark": {"round": 0, "level": 5000}}
    ]
  }|}
  in
  let path = write_temp_file content in
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () ->
      let activities = BH.For_tests.read_file path in
      check int "two delegates" 2 (List.length activities) ;
      let vsu =
        List.find
          (fun (a : BH.delegate_activity) -> a.delegate = "tz1VSU")
          activities
      in
      (match vsu.last_block with
      | None -> fail "VSU should have last_block"
      | Some h -> check int "block level" 5000 h.level) ;
      (match vsu.last_preattestation with
      | None -> fail "VSU should have preattestation"
      | Some h -> check int "preattestation level" 5001 h.level) ;
      (match vsu.last_attestation with
      | None -> fail "VSU should have attestation"
      | Some h -> check int "attestation level" 5001 h.level) ;
      let rj4 =
        List.find
          (fun (a : BH.delegate_activity) -> a.delegate = "tz1RJ4")
          activities
      in
      (match rj4.last_block with
      | None -> fail "RJ4 should have last_block"
      | Some h -> check int "rj4 block" 4998 h.level) ;
      check bool "rj4 no preattestation" true (rj4.last_preattestation = None) ;
      match rj4.last_attestation with
      | None -> fail "RJ4 should have attestation"
      | Some h -> check int "rj4 attestation" 5000 h.level)

let test_read_file_empty_sections () =
  let content = {|{"blocks": [], "preattestations": [], "attestations": []}|} in
  let path = write_temp_file content in
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () ->
      let activities = BH.For_tests.read_file path in
      check int "empty" 0 (List.length activities))

let test_read_file_nonexistent () =
  let activities =
    BH.For_tests.read_file "/nonexistent/path/highwatermarks.json"
  in
  check int "empty" 0 (List.length activities)

let test_read_file_invalid_json () =
  let path = write_temp_file "this is not json {{{" in
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () ->
      let activities = BH.For_tests.read_file path in
      check int "empty" 0 (List.length activities))

let test_read_file_blocks_only () =
  let content =
    {|{
    "blocks": [
      {"delegate": "tz1only", "highwatermark": {"round": 0, "level": 999}}
    ],
    "preattestations": [],
    "attestations": []
  }|}
  in
  let path = write_temp_file content in
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () ->
      let activities = BH.For_tests.read_file path in
      check int "one delegate" 1 (List.length activities) ;
      let a = List.hd activities in
      check string "delegate" "tz1only" a.delegate ;
      (match a.last_block with
      | None -> fail "should have block"
      | Some h -> check int "level" 999 h.level) ;
      check bool "no preattestation" true (a.last_preattestation = None) ;
      check bool "no attestation" true (a.last_attestation = None))

(* ── max_level ───────────────────────────────────────────────── *)

let test_max_level_all_present () =
  let a : BH.delegate_activity =
    {
      delegate = "tz1test";
      last_block = Some {round = 0; level = 100};
      last_preattestation = Some {round = 0; level = 200};
      last_attestation = Some {round = 0; level = 150};
    }
  in
  check (option int) "max is 200" (Some 200) (BH.max_level a)

let test_max_level_only_block () =
  let a : BH.delegate_activity =
    {
      delegate = "tz1test";
      last_block = Some {round = 0; level = 500};
      last_preattestation = None;
      last_attestation = None;
    }
  in
  check (option int) "500" (Some 500) (BH.max_level a)

let test_max_level_only_attestation () =
  let a : BH.delegate_activity =
    {
      delegate = "tz1test";
      last_block = None;
      last_preattestation = None;
      last_attestation = Some {round = 0; level = 300};
    }
  in
  check (option int) "300" (Some 300) (BH.max_level a)

let test_max_level_all_none () =
  let a : BH.delegate_activity =
    {
      delegate = "tz1test";
      last_block = None;
      last_preattestation = None;
      last_attestation = None;
    }
  in
  check (option int) "None" None (BH.max_level a)

let test_max_level_block_highest () =
  let a : BH.delegate_activity =
    {
      delegate = "tz1test";
      last_block = Some {round = 0; level = 9999};
      last_preattestation = Some {round = 0; level = 100};
      last_attestation = Some {round = 0; level = 200};
    }
  in
  check (option int) "9999" (Some 9999) (BH.max_level a)

let test_max_level_equal () =
  let a : BH.delegate_activity =
    {
      delegate = "tz1test";
      last_block = Some {round = 0; level = 42};
      last_preattestation = Some {round = 0; level = 42};
      last_attestation = Some {round = 0; level = 42};
    }
  in
  check (option int) "42" (Some 42) (BH.max_level a)

(* ── format_summary ──────────────────────────────────────────── *)

let test_format_summary_empty () =
  check (option string) "None" None (BH.format_summary [])

let test_format_summary_single () =
  let activities : BH.delegate_activity list =
    [
      {
        delegate = "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb";
        last_block = Some {round = 0; level = 5000};
        last_preattestation = None;
        last_attestation = None;
      };
    ]
  in
  match BH.format_summary activities with
  | None -> fail "should return Some"
  | Some summary ->
      check
        bool
        "contains level"
        true
        (try
           ignore (Str.search_forward (Str.regexp_string "L5000") summary 0) ;
           true
         with Not_found -> false) ;
      (* Long delegate is truncated to 8 chars + ellipsis *)
      check
        bool
        "contains truncated delegate"
        true
        (try
           ignore (Str.search_forward (Str.regexp_string "tz1VSUr8") summary 0) ;
           true
         with Not_found -> false)

let test_format_summary_short_delegate () =
  let activities : BH.delegate_activity list =
    [
      {
        delegate = "tz1short";
        last_block = Some {round = 0; level = 100};
        last_preattestation = None;
        last_attestation = None;
      };
    ]
  in
  match BH.format_summary activities with
  | None -> fail "should return Some"
  | Some summary ->
      check
        bool
        "contains full name"
        true
        (try
           ignore (Str.search_forward (Str.regexp_string "tz1short") summary 0) ;
           true
         with Not_found -> false)

let test_format_summary_multiple () =
  let activities : BH.delegate_activity list =
    [
      {
        delegate = "tz1aaa";
        last_block = Some {round = 0; level = 100};
        last_preattestation = None;
        last_attestation = None;
      };
      {
        delegate = "tz1bbb";
        last_block = None;
        last_preattestation = None;
        last_attestation = Some {round = 0; level = 200};
      };
    ]
  in
  match BH.format_summary activities with
  | None -> fail "should return Some"
  | Some summary ->
      check
        bool
        "contains L100"
        true
        (try
           ignore (Str.search_forward (Str.regexp_string "L100") summary 0) ;
           true
         with Not_found -> false) ;
      check
        bool
        "contains L200"
        true
        (try
           ignore (Str.search_forward (Str.regexp_string "L200") summary 0) ;
           true
         with Not_found -> false) ;
      check bool "contains space" true (String.contains summary ' ')

let test_format_summary_all_no_levels () =
  let activities : BH.delegate_activity list =
    [
      {
        delegate = "tz1none";
        last_block = None;
        last_preattestation = None;
        last_attestation = None;
      };
    ]
  in
  check (option string) "None" None (BH.format_summary activities)

let test_format_summary_delegate_truncation () =
  let activities : BH.delegate_activity list =
    [
      {
        delegate = "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb";
        last_block = Some {round = 0; level = 42};
        last_preattestation = None;
        last_attestation = None;
      };
    ]
  in
  match BH.format_summary activities with
  | None -> fail "should return Some"
  | Some summary ->
      (* String.sub delegate 0 8 = "tz1VSUr8", then "…" appended *)
      check
        bool
        "truncated prefix"
        true
        (try
           ignore (Str.search_forward (Str.regexp_string "tz1VSUr8") summary 0) ;
           true
         with Not_found -> false)

(* ── Cache ───────────────────────────────────────────────────── *)

let test_cache_get_empty () =
  BH.clear ~instance:"test-cache-empty" ;
  let result = BH.get ~instance:"test-cache-empty" in
  check int "empty" 0 (List.length result)

let test_cache_clear () =
  BH.clear ~instance:"test-cache-clear" ;
  let result = BH.get ~instance:"test-cache-clear" in
  check int "cleared" 0 (List.length result)

(* ── Test suite ──────────────────────────────────────────────── *)

let () =
  Alcotest.run
    "Baker_highwatermarks"
    [
      ( "parse_highwatermark",
        [
          test_case "valid" `Quick test_parse_hwm_valid;
          test_case "zero values" `Quick test_parse_hwm_zero;
          test_case "missing round" `Quick test_parse_hwm_missing_round;
          test_case "missing level" `Quick test_parse_hwm_missing_level;
          test_case "empty object" `Quick test_parse_hwm_empty;
          test_case "string" `Quick test_parse_hwm_string;
          test_case "null" `Quick test_parse_hwm_null;
        ] );
      ( "parse_entry",
        [
          test_case "valid" `Quick test_parse_entry_valid;
          test_case "invalid hwm" `Quick test_parse_entry_invalid_hwm;
          test_case "missing delegate" `Quick test_parse_entry_missing_delegate;
          test_case "non-object" `Quick test_parse_entry_non_object;
        ] );
      ( "parse_entries",
        [
          test_case "list" `Quick test_parse_entries_list;
          test_case "empty" `Quick test_parse_entries_empty;
          test_case "not a list" `Quick test_parse_entries_not_list;
          test_case "skips invalid" `Quick test_parse_entries_skips_invalid;
        ] );
      ( "read_file",
        [
          test_case "full file" `Quick test_read_file_full;
          test_case "empty sections" `Quick test_read_file_empty_sections;
          test_case "nonexistent" `Quick test_read_file_nonexistent;
          test_case "invalid JSON" `Quick test_read_file_invalid_json;
          test_case "blocks only" `Quick test_read_file_blocks_only;
        ] );
      ( "max_level",
        [
          test_case "all present" `Quick test_max_level_all_present;
          test_case "only block" `Quick test_max_level_only_block;
          test_case "only attestation" `Quick test_max_level_only_attestation;
          test_case "all none" `Quick test_max_level_all_none;
          test_case "block highest" `Quick test_max_level_block_highest;
          test_case "equal levels" `Quick test_max_level_equal;
        ] );
      ( "format_summary",
        [
          test_case "empty" `Quick test_format_summary_empty;
          test_case "single" `Quick test_format_summary_single;
          test_case "short delegate" `Quick test_format_summary_short_delegate;
          test_case "multiple" `Quick test_format_summary_multiple;
          test_case "all no levels" `Quick test_format_summary_all_no_levels;
          test_case "truncation" `Quick test_format_summary_delegate_truncation;
        ] );
      ( "cache",
        [
          test_case "get empty" `Quick test_cache_get_empty;
          test_case "clear" `Quick test_cache_clear;
        ] );
    ]
