(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** QCheck properties and additional edge case tests
    for external_service_detector pure functions. *)

open Alcotest
module FT = Octez_manager_lib.External_service_detector.For_tests

(* ================================================================== *)
(* string_contains edge cases                                          *)
(* ================================================================== *)

let test_sc_needle_equals_haystack () =
  check bool "same" true (FT.string_contains ~needle:"abc" "abc")

let test_sc_needle_at_start () =
  check bool "at start" true (FT.string_contains ~needle:"abc" "abcdef")

let test_sc_needle_at_end () =
  check bool "at end" true (FT.string_contains ~needle:"def" "abcdef")

let test_sc_needle_longer () =
  check bool "longer" false (FT.string_contains ~needle:"abcdef" "abc")

let test_sc_both_empty () =
  check bool "both empty" true (FT.string_contains ~needle:"" "")

let test_sc_single_char () =
  check bool "single found" true (FT.string_contains ~needle:"b" "abc") ;
  check bool "single not found" false (FT.string_contains ~needle:"z" "abc")

(* ================================================================== *)
(* is_managed_unit_name edge cases                                     *)
(* ================================================================== *)

let test_managed_template_unit () =
  (* Template: octez-node@.service matches because split gives
     ["octez-node"; ".service"] and ".service" ends with ".service".
     This is expected behavior — callers filter templates separately. *)
  check bool "template" true (FT.is_managed_unit_name "octez-node@.service")

let test_managed_multiple_at () =
  check
    bool
    "double @"
    false
    (FT.is_managed_unit_name "octez-node@@foo.service")

let test_managed_no_service_suffix () =
  check bool "no .service" false (FT.is_managed_unit_name "octez-node@foo")

let test_managed_empty () =
  check bool "empty" false (FT.is_managed_unit_name "")

let test_managed_octez_prefix_only () =
  check bool "prefix only" false (FT.is_managed_unit_name "octez-")

let test_managed_accuser () =
  check
    bool
    "accuser"
    true
    (FT.is_managed_unit_name "octez-accuser@mainnet.service")

let test_managed_signer () =
  check
    bool
    "signer"
    true
    (FT.is_managed_unit_name "octez-signer@mainnet.service")

(* ================================================================== *)
(* extract_command_from_systemd_format edge cases                      *)
(* ================================================================== *)

let test_extract_with_multiple_args () =
  check
    (option string)
    "multiple args"
    (Some
       "/usr/bin/octez-node run --data-dir /var/lib --rpc-addr 127.0.0.1:8732")
    (FT.extract_command_from_systemd_format
       "{ path=/usr/bin/octez-node ; argv[]=/usr/bin/octez-node run --data-dir \
        /var/lib --rpc-addr 127.0.0.1:8732 ; ignore_errors=no }")

let test_extract_empty () =
  check (option string) "empty" None (FT.extract_command_from_systemd_format "")

let test_extract_no_trailing_semicolon () =
  (* No semicolon after argv content — extracts to end of string including } *)
  check
    (option string)
    "no trailing semicolon"
    (Some "/bin/foo --arg }")
    (FT.extract_command_from_systemd_format
       "{ path=/bin/foo ; argv[]=/bin/foo --arg }")

let test_extract_just_brackets () =
  check
    (option string)
    "just brackets"
    None
    (FT.extract_command_from_systemd_format "[]")

let test_extract_argv_at_start () =
  (* argv[] without preceding "argv" (4 chars before '[') *)
  check
    (option string)
    "short prefix"
    None
    (FT.extract_command_from_systemd_format "v[]=/bin/foo ;")

(* ================================================================== *)
(* chain_id_to_network edge cases                                      *)
(* ================================================================== *)

let test_chain_shadownet () =
  check
    (option string)
    "shadownet"
    (Some "shadownet")
    (FT.chain_id_to_network "NetXsqzbfFenSTS")

let test_chain_tallinnnet () =
  check
    (option string)
    "tallinnnet"
    (Some "tallinnnet")
    (FT.chain_id_to_network "NetXe8DbhW9A1eS")

let test_chain_empty () =
  check (option string) "empty" None (FT.chain_id_to_network "")

let test_chain_partial () =
  check (option string) "partial" None (FT.chain_id_to_network "NetX")

(* ================================================================== *)
(* contains_octez_binary edge cases                                    *)
(* ================================================================== *)

let test_cob_with_path_prefix () =
  check
    bool
    "full path"
    true
    (FT.contains_octez_binary "/home/user/.opam/bin/octez-node run")

let test_cob_mixed_case () =
  check
    bool
    "mixed case"
    true
    (FT.contains_octez_binary "Octez-Baker run with-dal")

let test_cob_partial_match () =
  check
    bool
    "octez- alone"
    false
    (FT.contains_octez_binary "octez-client transfer")

let test_cob_tezos_accuser () =
  check
    bool
    "tezos-accuser"
    true
    (FT.contains_octez_binary "tezos-accuser-PsBoreas")

(* ================================================================== *)
(* systemctl_cmd                                                       *)
(* ================================================================== *)

let test_systemctl_cmd () =
  let cmd = FT.systemctl_cmd () in
  check bool "starts with systemctl" true (List.hd cmd = "systemctl")

(* ================================================================== *)
(* QCheck properties                                                   *)
(* ================================================================== *)

let prop_sc_never_crashes =
  QCheck_alcotest.to_alcotest
    (QCheck.Test.make
       ~count:500
       ~name:"string_contains never crashes"
       QCheck.(pair string string)
       (fun (needle, haystack) ->
         let _ = FT.string_contains ~needle haystack in
         true))

let prop_sc_reflexive =
  QCheck_alcotest.to_alcotest
    (QCheck.Test.make
       ~count:300
       ~name:"string_contains is reflexive"
       QCheck.string
       (fun s -> FT.string_contains ~needle:s s))

let prop_sc_empty_needle_always_true =
  QCheck_alcotest.to_alcotest
    (QCheck.Test.make
       ~count:300
       ~name:"empty needle always found"
       QCheck.string
       (fun s -> FT.string_contains ~needle:"" s))

let prop_managed_never_crashes =
  QCheck_alcotest.to_alcotest
    (QCheck.Test.make
       ~count:500
       ~name:"is_managed_unit_name never crashes"
       QCheck.string
       (fun s ->
         let _ = FT.is_managed_unit_name s in
         true))

let prop_managed_implies_prefix_and_at =
  QCheck_alcotest.to_alcotest
    (QCheck.Test.make
       ~count:500
       ~name:"managed implies octez- prefix and @ sign"
       QCheck.string
       (fun s ->
         if FT.is_managed_unit_name s then
           String.starts_with ~prefix:"octez-" s
           && String.contains s '@'
           && String.ends_with ~suffix:".service" s
         else true))

let prop_extract_never_crashes =
  QCheck_alcotest.to_alcotest
    (QCheck.Test.make
       ~count:500
       ~name:"extract_command never crashes"
       QCheck.string
       (fun s ->
         let _ = FT.extract_command_from_systemd_format s in
         true))

let prop_chain_id_never_crashes =
  QCheck_alcotest.to_alcotest
    (QCheck.Test.make
       ~count:500
       ~name:"chain_id_to_network never crashes"
       QCheck.string
       (fun s ->
         let _ = FT.chain_id_to_network s in
         true))

let prop_chain_id_known_only =
  QCheck_alcotest.to_alcotest
    (QCheck.Test.make
       ~count:500
       ~name:"chain_id returns Some only for Net* prefixed IDs"
       QCheck.string
       (fun s ->
         match FT.chain_id_to_network s with
         | Some _ -> String.starts_with ~prefix:"Net" s
         | None -> true))

let prop_cob_never_crashes =
  QCheck_alcotest.to_alcotest
    (QCheck.Test.make
       ~count:500
       ~name:"contains_octez_binary never crashes"
       QCheck.string
       (fun s ->
         let _ = FT.contains_octez_binary s in
         true))

let prop_cob_true_implies_octez_or_tezos =
  QCheck_alcotest.to_alcotest
    (QCheck.Test.make
       ~count:500
       ~name:"contains_octez_binary true implies octez or tezos in input"
       QCheck.string
       (fun s ->
         if FT.contains_octez_binary s then
           let lower = String.lowercase_ascii s in
           FT.string_contains ~needle:"octez-" lower
           || FT.string_contains ~needle:"tezos-" lower
         else true))

(* ================================================================== *)
(* TEST SUITE                                                          *)
(* ================================================================== *)

let () =
  run
    "External Detector Props"
    [
      ( "string_contains edges",
        [
          test_case "equals" `Quick test_sc_needle_equals_haystack;
          test_case "at start" `Quick test_sc_needle_at_start;
          test_case "at end" `Quick test_sc_needle_at_end;
          test_case "longer" `Quick test_sc_needle_longer;
          test_case "both empty" `Quick test_sc_both_empty;
          test_case "single char" `Quick test_sc_single_char;
        ] );
      ( "is_managed edges",
        [
          test_case "template" `Quick test_managed_template_unit;
          test_case "double @" `Quick test_managed_multiple_at;
          test_case "no .service" `Quick test_managed_no_service_suffix;
          test_case "empty" `Quick test_managed_empty;
          test_case "prefix only" `Quick test_managed_octez_prefix_only;
          test_case "accuser" `Quick test_managed_accuser;
          test_case "signer" `Quick test_managed_signer;
        ] );
      ( "extract_command edges",
        [
          test_case "multiple args" `Quick test_extract_with_multiple_args;
          test_case "empty" `Quick test_extract_empty;
          test_case
            "no trailing semicolon"
            `Quick
            test_extract_no_trailing_semicolon;
          test_case "just brackets" `Quick test_extract_just_brackets;
          test_case "short prefix" `Quick test_extract_argv_at_start;
        ] );
      ( "chain_id edges",
        [
          test_case "shadownet" `Quick test_chain_shadownet;
          test_case "tallinnnet" `Quick test_chain_tallinnnet;
          test_case "empty" `Quick test_chain_empty;
          test_case "partial" `Quick test_chain_partial;
        ] );
      ( "contains_octez edges",
        [
          test_case "full path" `Quick test_cob_with_path_prefix;
          test_case "mixed case" `Quick test_cob_mixed_case;
          test_case "partial match" `Quick test_cob_partial_match;
          test_case "tezos-accuser" `Quick test_cob_tezos_accuser;
        ] );
      ("systemctl_cmd", [test_case "starts with" `Quick test_systemctl_cmd]);
      ( "QCheck properties",
        [
          prop_sc_never_crashes;
          prop_sc_reflexive;
          prop_sc_empty_needle_always_true;
          prop_managed_never_crashes;
          prop_managed_implies_prefix_and_at;
          prop_extract_never_crashes;
          prop_chain_id_never_crashes;
          prop_chain_id_known_only;
          prop_cob_never_crashes;
          prop_cob_true_implies_octez_or_tezos;
        ] );
    ]
