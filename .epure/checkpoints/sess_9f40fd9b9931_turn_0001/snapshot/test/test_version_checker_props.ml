(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Property-based tests for Version_checker module.

    Properties tested:
    - No-crash: parse_version/compare_versions never raise on random input
    - Reflexivity: compare_versions v v = 0
    - Antisymmetry: compare_versions a b = -(compare_versions b a)
    - Transitivity: if a <= b and b <= c then a <= c
    - RC ordering: "X.Y-rc1" < "X.Y" for any valid version
*)

open Octez_manager_lib

(* ============================================================ *)
(* Generators *)
(* ============================================================ *)

(** Generate version-like strings *)
let gen_version_string =
  let open QCheck.Gen in
  oneof_weighted
    [
      (* Realistic versions like "24.1", "v25.0" *)
      ( 5,
        let* major = int_range 1 99 in
        let* minor = int_range 0 9 in
        let* has_v = bool in
        let prefix = if has_v then "v" else "" in
        return (Printf.sprintf "%s%d.%d" prefix major minor) );
      (* Versions with RC *)
      ( 2,
        let* major = int_range 1 99 in
        let* minor = int_range 0 9 in
        let* rc = int_range 1 5 in
        return (Printf.sprintf "%d.%d-rc%d" major minor rc) );
      (* Simple numbers *)
      ( 1,
        let* n = int_range 0 99 in
        return (string_of_int n) );
      (* Random strings *)
      (2, string_size (int_range 0 20));
    ]

let arb_version = QCheck.make gen_version_string ~print:Fun.id

(* ============================================================ *)
(* No-crash properties *)
(* ============================================================ *)

let prop_parse_version_no_crash =
  QCheck.Test.make
    ~name:"parse_version never crashes on random input"
    ~count:300
    QCheck.string
    (fun s ->
      let _result = Version_checker.For_tests.parse_version s in
      true)

let prop_compare_versions_no_crash =
  QCheck.Test.make
    ~name:"compare_versions never crashes on random input"
    ~count:300
    QCheck.(pair string string)
    (fun (a, b) ->
      let _result = Version_checker.compare_versions a b in
      true)

(* ============================================================ *)
(* Ordering properties *)
(* ============================================================ *)

let prop_reflexivity =
  QCheck.Test.make
    ~name:"compare_versions is reflexive: v = v"
    ~count:300
    arb_version
    (fun v -> Version_checker.compare_versions v v = 0)

let prop_antisymmetry =
  QCheck.Test.make
    ~name:"compare_versions is antisymmetric: cmp(a,b) = -cmp(b,a)"
    ~count:300
    QCheck.(pair arb_version arb_version)
    (fun (a, b) ->
      let ab = Version_checker.compare_versions a b in
      let ba = Version_checker.compare_versions b a in
      (* Signs are opposite *)
      (ab > 0 && ba < 0) || (ab < 0 && ba > 0) || (ab = 0 && ba = 0))

let prop_transitivity =
  QCheck.Test.make
    ~name:"compare_versions is transitive: a<=b and b<=c implies a<=c"
    ~count:300
    QCheck.(triple arb_version arb_version arb_version)
    (fun (a, b, c) ->
      let ab = Version_checker.compare_versions a b in
      let bc = Version_checker.compare_versions b c in
      let ac = Version_checker.compare_versions a c in
      if ab <= 0 && bc <= 0 then ac <= 0 else true)

(* ============================================================ *)
(* RC ordering properties *)
(* ============================================================ *)

let prop_rc_less_than_release =
  QCheck.Test.make
    ~name:"RC versions are less than their release"
    ~count:100
    QCheck.(pair (int_range 1 99) (int_range 0 9))
    (fun (major, minor) ->
      let release = Printf.sprintf "%d.%d" major minor in
      let rc = Printf.sprintf "%d.%d-rc1" major minor in
      Version_checker.compare_versions rc release < 0)

let prop_rc_ordering =
  QCheck.Test.make
    ~name:"RC1 < RC2 < ... for same base version"
    ~count:100
    QCheck.(triple (int_range 1 99) (int_range 0 9) (int_range 1 4))
    (fun (major, minor, rc) ->
      let rc1 = Printf.sprintf "%d.%d-rc%d" major minor rc in
      let rc2 = Printf.sprintf "%d.%d-rc%d" major minor (rc + 1) in
      Version_checker.compare_versions rc1 rc2 < 0)

(* ============================================================ *)
(* Test Suite *)
(* ============================================================ *)

let props =
  List.map
    QCheck_alcotest.to_alcotest
    [
      prop_parse_version_no_crash;
      prop_compare_versions_no_crash;
      prop_reflexivity;
      prop_antisymmetry;
      prop_transitivity;
      prop_rc_less_than_release;
      prop_rc_ordering;
    ]

let () = Alcotest.run "Version_checker_props" [("properties", props)]
