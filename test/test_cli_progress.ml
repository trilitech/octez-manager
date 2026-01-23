(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Unit tests for CLI progress bar rendering *)

open Cli_progress

(* Expose internal functions via For_test *)
let render_progress_bar = For_test.render_progress_bar

let render_binary_line = For_test.render_binary_line

let detect_style = For_test.detect_style

(* ========================================================================= *)
(* Test Helpers *)
(* ========================================================================= *)

let with_env_vars vars f =
  let saved = List.map (fun (k, _) -> (k, Sys.getenv_opt k)) vars in
  List.iter (fun (k, v) -> Unix.putenv k v) vars ;
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun (k, v) ->
          match v with Some v -> Unix.putenv k v | None -> Unix.putenv k "")
        saved)
    f

let string_contains ~needle haystack =
  try
    let _ = String.index haystack (String.get needle 0) in
    let len = String.length needle in
    let hlen = String.length haystack in
    let rec check i =
      if i + len > hlen then false
      else if String.sub haystack i len = needle then true
      else check (i + 1)
    in
    check 0
  with Not_found -> false

(* ========================================================================= *)
(* Progress Bar Rendering Tests *)
(* ========================================================================= *)

let test_ascii_progress_bar_empty () =
  let bar = render_progress_bar ~width:20 ~percentage:0.0 ~style:ASCII in
  Alcotest.(check string) "0% ASCII bar" "--------------------" bar

let test_ascii_progress_bar_half () =
  let bar = render_progress_bar ~width:20 ~percentage:50.0 ~style:ASCII in
  Alcotest.(check string) "50% ASCII bar" "==========----------" bar

let test_ascii_progress_bar_full () =
  let bar = render_progress_bar ~width:20 ~percentage:100.0 ~style:ASCII in
  Alcotest.(check string) "100% ASCII bar" "====================" bar

let test_ascii_progress_bar_quarter () =
  let bar = render_progress_bar ~width:20 ~percentage:25.0 ~style:ASCII in
  Alcotest.(check string) "25% ASCII bar" "=====---------------" bar

let test_progress_bar_clamps_low () =
  let bar = render_progress_bar ~width:10 ~percentage:(-10.0) ~style:ASCII in
  Alcotest.(check string) "Negative clamped to 0%" "----------" bar

let test_progress_bar_clamps_high () =
  let bar = render_progress_bar ~width:10 ~percentage:150.0 ~style:ASCII in
  Alcotest.(check string) "Over 100 clamped" "==========" bar

let test_unicode_progress_bar_length () =
  let bar = render_progress_bar ~width:20 ~percentage:50.0 ~style:Unicode in
  (* Each Unicode char is 3 bytes (UTF-8), so 20 chars = 60 bytes *)
  let byte_count = String.length bar in
  Alcotest.(check int) "Unicode bar byte length" 60 byte_count

(* ========================================================================= *)
(* File Size Formatting Tests *)
(* ========================================================================= *)

let test_format_size_bytes () =
  let formatted = format_size 512L in
  Alcotest.(check string) "Bytes" "512 bytes" formatted

let test_format_size_kb () =
  let formatted = format_size 2048L in
  Alcotest.(check string) "Kilobytes" "2 KB" formatted

let test_format_size_mb () =
  let formatted = format_size 5_242_880L in
  (* 5 * 1024 * 1024 *)
  Alcotest.(check string) "Megabytes" "5 MB" formatted

let test_format_size_gb () =
  let formatted = format_size 3_221_225_472L in
  (* 3 * 1024 * 1024 * 1024 *)
  Alcotest.(check string) "Gigabytes" "3 GB" formatted

let test_format_size_zero () =
  let formatted = format_size 0L in
  Alcotest.(check string) "Zero bytes" "0 bytes" formatted

let test_format_size_boundary_kb () =
  let formatted = format_size 1024L in
  Alcotest.(check string) "1 KB boundary" "1 KB" formatted

let test_format_size_boundary_mb () =
  let formatted = format_size 1_048_576L in
  (* 1024 * 1024 *)
  Alcotest.(check string) "1 MB boundary" "1 MB" formatted

(* ========================================================================= *)
(* Binary Line Rendering Tests *)
(* ========================================================================= *)

let test_render_pending_binary () =
  let line =
    render_binary_line
      ~name:"octez-node"
      ~status:Pending
      ~bar_width:20
      ~style:ASCII
  in
  Alcotest.(check bool)
    "Contains name"
    true
    (string_contains ~needle:"octez-node" line) ;
  Alcotest.(check bool) "Shows 0%" true (string_contains ~needle:"0%" line) ;
  Alcotest.(check bool)
    "Has empty indicator"
    true
    (string_contains ~needle:"[ ]" line)

let test_render_in_progress_with_total () =
  let status =
    InProgress {downloaded = 50_000_000L; total = Some 100_000_000L}
  in
  let line =
    render_binary_line ~name:"octez-baker" ~status ~bar_width:20 ~style:ASCII
  in
  Alcotest.(check bool) "Shows 50%" true (string_contains ~needle:"50%" line) ;
  Alcotest.(check bool)
    "Shows downloaded size"
    true
    (string_contains ~needle:"47 MB" line) ;
  Alcotest.(check bool)
    "Shows total size"
    true
    (string_contains ~needle:"95 MB" line)

let test_render_in_progress_without_total () =
  let status = InProgress {downloaded = 25_000_000L; total = None} in
  let line =
    render_binary_line ~name:"octez-client" ~status ~bar_width:20 ~style:ASCII
  in
  Alcotest.(check bool)
    "Contains name"
    true
    (string_contains ~needle:"octez-client" line) ;
  Alcotest.(check bool) "Shows size" true (string_contains ~needle:"23 MB" line) ;
  (* No percentage when total is unknown *)
  Alcotest.(check bool)
    "No percentage"
    false
    (string_contains ~needle:"50%" line)

let test_render_complete_binary () =
  let status = Complete {size = 25_000_000L} in
  let line =
    render_binary_line ~name:"octez-client" ~status ~bar_width:20 ~style:ASCII
  in
  Alcotest.(check bool) "Shows 100%" true (string_contains ~needle:"100%" line) ;
  (* Check for checkmark (✓ = U+2713 in UTF-8: \xe2\x9c\x93) *)
  Alcotest.(check bool)
    "Shows checkmark"
    true
    (string_contains ~needle:"\xe2\x9c\x93" line) ;
  Alcotest.(check bool) "Shows size" true (string_contains ~needle:"23 MB" line)

let test_render_binary_unicode_style () =
  let status =
    InProgress {downloaded = 50_000_000L; total = Some 100_000_000L}
  in
  let line =
    render_binary_line ~name:"octez-node" ~status ~bar_width:20 ~style:Unicode
  in
  (* Should contain Unicode progress characters *)
  let byte_length = String.length line in
  Alcotest.(check bool) "Has Unicode chars" true (byte_length > 80)
(* ASCII version would be shorter *)

(* ========================================================================= *)
(* Display State Tests *)
(* ========================================================================= *)

let test_init_display () =
  let state = init_display ["octez-node"; "octez-baker"] in
  Alcotest.(check int) "Two binaries" 2 (List.length state.binaries) ;
  Alcotest.(check int) "No lines printed yet" 0 state.lines_printed ;
  Alcotest.(check bool) "No checksum status" true (state.checksum_status = None) ;
  (* All binaries start as Pending *)
  List.iter
    (fun (_, status) ->
      match status with
      | Pending -> ()
      | _ -> Alcotest.fail "Initial status should be Pending")
    state.binaries

let test_update_binary_status () =
  let state = init_display ["octez-node"; "octez-baker"] in
  let updated =
    set_in_progress
      state
      ~binary:"octez-node"
      ~downloaded:1024L
      ~total:(Some 2048L)
  in
  (* Find octez-node in updated state *)
  match List.assoc_opt "octez-node" updated.binaries with
  | Some (InProgress {downloaded; total}) ->
      Alcotest.(check int64) "Downloaded bytes" 1024L downloaded ;
      Alcotest.(check (option int64)) "Total bytes" (Some 2048L) total
  | _ -> Alcotest.fail "Status not updated correctly"

let test_set_complete () =
  let state = init_display ["octez-client"] in
  let updated = set_complete state ~binary:"octez-client" ~size:5_000_000L in
  match List.assoc_opt "octez-client" updated.binaries with
  | Some (Complete {size}) -> Alcotest.(check int64) "Size" 5_000_000L size
  | _ -> Alcotest.fail "Status not set to Complete"

let test_set_checksum_status () =
  let state = init_display ["octez-node"] in
  let updated = set_checksum_status state "Verifying checksums..." in
  match updated.checksum_status with
  | Some msg ->
      Alcotest.(check string) "Checksum msg" "Verifying checksums..." msg
  | None -> Alcotest.fail "Checksum status not set"

let test_clear_checksum_status () =
  let state = init_display ["octez-node"] in
  let with_status = set_checksum_status state "Verifying..." in
  let cleared = clear_checksum_status with_status in
  Alcotest.(check bool) "Checksum cleared" true (cleared.checksum_status = None)

let test_update_preserves_other_binaries () =
  let state = init_display ["binary1"; "binary2"; "binary3"] in
  let updated =
    set_in_progress state ~binary:"binary2" ~downloaded:100L ~total:None
  in
  Alcotest.(check int) "All binaries preserved" 3 (List.length updated.binaries) ;
  (* binary1 and binary3 should still be Pending *)
  match List.assoc_opt "binary1" updated.binaries with
  | Some Pending -> ()
  | _ -> Alcotest.fail "Other binaries changed"

(* ========================================================================= *)
(* Style Detection Tests *)
(* ========================================================================= *)

let test_detect_ascii_for_dumb_term () =
  with_env_vars
    [("TERM", "dumb")]
    (fun () ->
      let style = detect_style () in
      match style with
      | ASCII -> ()
      | Unicode -> Alcotest.fail "Should detect ASCII for dumb terminal")

let test_detect_ascii_for_empty_term () =
  with_env_vars
    [("TERM", "")]
    (fun () ->
      let style = detect_style () in
      match style with
      | ASCII -> ()
      | Unicode -> Alcotest.fail "Should detect ASCII for empty TERM")

let test_detect_unicode_for_utf8 () =
  with_env_vars
    [("TERM", "xterm-256color"); ("LANG", "en_US.UTF-8")]
    (fun () ->
      let style = detect_style () in
      match style with
      | Unicode -> ()
      | ASCII -> Alcotest.fail "Should detect Unicode for UTF-8 locale")

let test_detect_ascii_without_utf8_lang () =
  with_env_vars
    [("TERM", "xterm"); ("LANG", "C")]
    (fun () ->
      let style = detect_style () in
      match style with
      | ASCII -> ()
      | Unicode -> Alcotest.fail "Should detect ASCII without UTF-8 in LANG")

(* ========================================================================= *)
(* Test Suite Registration *)
(* ========================================================================= *)

let () =
  Alcotest.run
    "CLI Progress Tests"
    [
      ( "progress_bar",
        [
          Alcotest.test_case "ascii_empty" `Quick test_ascii_progress_bar_empty;
          Alcotest.test_case "ascii_half" `Quick test_ascii_progress_bar_half;
          Alcotest.test_case "ascii_full" `Quick test_ascii_progress_bar_full;
          Alcotest.test_case
            "ascii_quarter"
            `Quick
            test_ascii_progress_bar_quarter;
          Alcotest.test_case "clamps_low" `Quick test_progress_bar_clamps_low;
          Alcotest.test_case "clamps_high" `Quick test_progress_bar_clamps_high;
          Alcotest.test_case
            "unicode_length"
            `Quick
            test_unicode_progress_bar_length;
        ] );
      ( "format_size",
        [
          Alcotest.test_case "bytes" `Quick test_format_size_bytes;
          Alcotest.test_case "kb" `Quick test_format_size_kb;
          Alcotest.test_case "mb" `Quick test_format_size_mb;
          Alcotest.test_case "gb" `Quick test_format_size_gb;
          Alcotest.test_case "zero" `Quick test_format_size_zero;
          Alcotest.test_case "boundary_kb" `Quick test_format_size_boundary_kb;
          Alcotest.test_case "boundary_mb" `Quick test_format_size_boundary_mb;
        ] );
      ( "render_binary_line",
        [
          Alcotest.test_case "pending" `Quick test_render_pending_binary;
          Alcotest.test_case
            "in_progress_with_total"
            `Quick
            test_render_in_progress_with_total;
          Alcotest.test_case
            "in_progress_without_total"
            `Quick
            test_render_in_progress_without_total;
          Alcotest.test_case "complete" `Quick test_render_complete_binary;
          Alcotest.test_case
            "unicode_style"
            `Quick
            test_render_binary_unicode_style;
        ] );
      ( "display_state",
        [
          Alcotest.test_case "init" `Quick test_init_display;
          Alcotest.test_case "update_status" `Quick test_update_binary_status;
          Alcotest.test_case "set_complete" `Quick test_set_complete;
          Alcotest.test_case "set_checksum" `Quick test_set_checksum_status;
          Alcotest.test_case "clear_checksum" `Quick test_clear_checksum_status;
          Alcotest.test_case
            "preserves_others"
            `Quick
            test_update_preserves_other_binaries;
        ] );
      ( "style_detection",
        [
          Alcotest.test_case "dumb_term" `Quick test_detect_ascii_for_dumb_term;
          Alcotest.test_case
            "empty_term"
            `Quick
            test_detect_ascii_for_empty_term;
          Alcotest.test_case "utf8" `Quick test_detect_unicode_for_utf8;
          Alcotest.test_case
            "no_utf8"
            `Quick
            test_detect_ascii_without_utf8_lang;
        ] );
    ]
