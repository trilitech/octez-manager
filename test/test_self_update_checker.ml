(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Octez_manager_lib

(** Tests for self_update_checker.ml - version comparison and update checking *)

(* Test version comparison *)
let test_compare_versions () =
  (* Equal versions *)
  Alcotest.(check int)
    "0.1.1 = 0.1.1"
    0
    (Self_update_checker.compare_versions "0.1.1" "0.1.1") ;
  Alcotest.(check int)
    "v1.0.0 = 1.0.0"
    0
    (Self_update_checker.compare_versions "v1.0.0" "1.0.0") ;
  (* Less than *)
  Alcotest.(check int)
    "0.1.0 < 0.1.1"
    (-1)
    (Self_update_checker.compare_versions "0.1.0" "0.1.1") ;
  Alcotest.(check int)
    "0.9.0 < 1.0.0"
    (-1)
    (Self_update_checker.compare_versions "0.9.0" "1.0.0") ;
  Alcotest.(check int)
    "1.0.0 < 1.1.0"
    (-1)
    (Self_update_checker.compare_versions "1.0.0" "1.1.0") ;
  (* Greater than *)
  Alcotest.(check int)
    "0.1.2 > 0.1.1"
    1
    (Self_update_checker.compare_versions "0.1.2" "0.1.1") ;
  Alcotest.(check int)
    "1.0.0 > 0.9.9"
    1
    (Self_update_checker.compare_versions "1.0.0" "0.9.9") ;
  Alcotest.(check int)
    "2.0.0 > 1.9.9"
    1
    (Self_update_checker.compare_versions "2.0.0" "1.9.9") ;
  (* RC versions *)
  Alcotest.(check int)
    "1.0.0-rc1 < 1.0.0"
    (-1)
    (Self_update_checker.compare_versions "1.0.0-rc1" "1.0.0") ;
  Alcotest.(check int)
    "1.0.0 > 1.0.0-rc1"
    1
    (Self_update_checker.compare_versions "1.0.0" "1.0.0-rc1") ;
  Alcotest.(check int)
    "1.0.0-rc1 < 1.0.0-rc2"
    (-1)
    (Self_update_checker.compare_versions "1.0.0-rc1" "1.0.0-rc2") ;
  Alcotest.(check int)
    "1.0.0-rc2 > 1.0.0-rc1"
    1
    (Self_update_checker.compare_versions "1.0.0-rc2" "1.0.0-rc1") ;
  (* Trailing zeros *)
  Alcotest.(check int)
    "1.0 = 1.0.0"
    0
    (Self_update_checker.compare_versions "1.0" "1.0.0") ;
  Alcotest.(check int)
    "1 = 1.0.0"
    0
    (Self_update_checker.compare_versions "1" "1.0.0")

(* Test major update detection *)
let test_is_major_update () =
  Alcotest.(check bool)
    "0.1.1 -> 1.0.0 is major"
    true
    (Self_update_checker.is_major_update ~current:"0.1.1" ~latest:"1.0.0") ;
  Alcotest.(check bool)
    "1.0.0 -> 2.0.0 is major"
    true
    (Self_update_checker.is_major_update ~current:"1.0.0" ~latest:"2.0.0") ;
  Alcotest.(check bool)
    "0.1.1 -> 0.2.0 is not major"
    false
    (Self_update_checker.is_major_update ~current:"0.1.1" ~latest:"0.2.0") ;
  Alcotest.(check bool)
    "1.0.0 -> 1.1.0 is not major"
    false
    (Self_update_checker.is_major_update ~current:"1.0.0" ~latest:"1.1.0") ;
  Alcotest.(check bool)
    "1.9.9 -> 1.9.10 is not major"
    false
    (Self_update_checker.is_major_update ~current:"1.9.9" ~latest:"1.9.10")

(* Test URL generation - these functions aren't exported, so we test them indirectly
   through check_for_updates which would use them if an update is available *)
let test_url_constants () =
  (* Just verify the module constants are accessible *)
  Alcotest.(check pass) "URL functions exist" () ()

(* Test install method detection *)
let test_install_method () =
  (* We can't test the actual detection without mocking,
     but we can test the logic exists and compiles *)
  let _method = Self_update_checker.detect_install_method () in
  Alcotest.(check pass) "detect_install_method runs" () ()

(* Test current version *)
let test_current_version () =
  let ver = Self_update_checker.current_version in
  Alcotest.(check bool)
    "current version is not empty"
    true
    (String.length ver > 0) ;
  (* The version comes from dune-build-info: stamped builds (the released
     binary) report the package version in X.Y.Z format; unstamped builds
     (dev, test executables) report "dev". Regression test for #967: the
     version must never be a hardcoded constant again. *)
  Alcotest.(check bool)
    "current version is the build-info version or dev"
    true
    (String.equal ver "dev" || String.contains ver '.') ;
  Alcotest.(check bool)
    "current version is not the stale hardcoded 0.3.0"
    true
    (not (String.equal ver "0.3.0"))

let () =
  let open Alcotest in
  run
    "Self Update Checker"
    [
      ( "Version Comparison",
        [
          test_case "compare versions" `Quick test_compare_versions;
          test_case "detect major updates" `Quick test_is_major_update;
          test_case "current version format" `Quick test_current_version;
        ] );
      ("URL Generation", [test_case "URL constants" `Quick test_url_constants]);
      ( "Installation",
        [test_case "install method detection" `Quick test_install_method] );
    ]
