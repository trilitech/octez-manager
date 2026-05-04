(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Octez_manager_lib
open Octez_manager_ui
module FT = Instances_actions.For_tests
module UFT = Instances_update.For_tests

(* ============================================================ *)
(* Helper: create minimal Service.t                              *)
(* ============================================================ *)

let make_svc ?(instance = "test") ?(role = "node") ?(network = "mainnet") () =
  Service.make
    ~instance
    ~role
    ~network
    ~history_mode:History_mode.default
    ~data_dir:"/tmp"
    ~rpc_addr:(Rpc_addr.of_string "")
    ~net_addr:""
    ~service_user:"tezos"
    ~app_bin_dir:"/usr/bin"
    ~logging_mode:Logging_mode.default
    ()

(* ============================================================ *)
(* extract_version_string Tests                                  *)
(* ============================================================ *)

let test_extract_version_simple () =
  Alcotest.(check (option string))
    "24.0"
    (Some "24.0")
    (UFT.extract_version_string "24.0 (abc123)")

let test_extract_version_with_prefix () =
  Alcotest.(check (option string))
    "Octez 24.0"
    (Some "24.0")
    (UFT.extract_version_string "Octez 24.0")

let test_extract_version_three_part () =
  Alcotest.(check (option string))
    "24.0.1"
    (Some "24.0.1")
    (UFT.extract_version_string "v24.0.1-beta")

let test_extract_version_no_match () =
  Alcotest.(check (option string))
    "no match"
    None
    (UFT.extract_version_string "no version here")

let test_extract_version_empty () =
  Alcotest.(check (option string)) "empty" None (UFT.extract_version_string "")

let test_extract_version_just_number () =
  Alcotest.(check (option string))
    "just number"
    None
    (UFT.extract_version_string "42")

let test_extract_version_multi_line () =
  Alcotest.(check (option string))
    "multi line"
    (Some "24.0")
    (UFT.extract_version_string "Version\n24.0 (build abc)")

(* ============================================================ *)
(* role_to_binary_name Tests                                     *)
(* ============================================================ *)

let test_role_node () =
  Alcotest.(check string) "node" "octez-node" (UFT.role_to_binary_name "node")

let test_role_baker () =
  Alcotest.(check string)
    "baker"
    "octez-baker"
    (UFT.role_to_binary_name "baker")

let test_role_accuser () =
  Alcotest.(check string)
    "accuser"
    "octez-baker"
    (UFT.role_to_binary_name "accuser")

let test_role_dal_node () =
  Alcotest.(check string)
    "dal-node"
    "octez-dal-node"
    (UFT.role_to_binary_name "dal-node")

let test_role_dal () =
  Alcotest.(check string) "dal" "octez-dal-node" (UFT.role_to_binary_name "dal")

let test_role_unknown () =
  Alcotest.(check string)
    "unknown prefixes with octez-"
    "octez-signer"
    (UFT.role_to_binary_name "signer")

(* ============================================================ *)
(* collect_dependents Tests                                      *)
(* ============================================================ *)

(* Graph: A -> [B, C], B -> [D], C -> [], D -> [] *)
let linear_deps =
  let graph =
    [
      ("A", [make_svc ~instance:"B" (); make_svc ~instance:"C" ()]);
      ("B", [make_svc ~instance:"D" ()]);
      ("C", []);
      ("D", []);
    ]
  in
  fun inst ->
    match List.assoc_opt inst graph with Some deps -> deps | None -> []

let test_collect_linear () =
  let deps = UFT.collect_dependents ~get_deps:linear_deps "A" in
  let names = List.map (fun s -> s.Service.instance) deps in
  Alcotest.(check int) "3 dependents" 3 (List.length names) ;
  Alcotest.(check bool) "has B" true (List.mem "B" names) ;
  Alcotest.(check bool) "has C" true (List.mem "C" names) ;
  Alcotest.(check bool) "has D" true (List.mem "D" names)

let test_collect_leaf () =
  let deps = UFT.collect_dependents ~get_deps:linear_deps "D" in
  Alcotest.(check int) "leaf has no deps" 0 (List.length deps)

let test_collect_no_such_node () =
  let deps = UFT.collect_dependents ~get_deps:linear_deps "NONEXISTENT" in
  Alcotest.(check int) "unknown node has no deps" 0 (List.length deps)

(* Graph with cycle: X -> [Y], Y -> [X] *)
let cyclic_deps =
  let graph =
    [("X", [make_svc ~instance:"Y" ()]); ("Y", [make_svc ~instance:"X" ()])]
  in
  fun inst ->
    match List.assoc_opt inst graph with Some deps -> deps | None -> []

let test_collect_cycle () =
  let deps = UFT.collect_dependents ~get_deps:cyclic_deps "X" in
  (* Should not loop forever; should find Y (and then stop since X is visited) *)
  let names = List.map (fun s -> s.Service.instance) deps in
  Alcotest.(check bool) "has Y" true (List.mem "Y" names) ;
  (* X shouldn't be in deps because it's visited as root *)
  Alcotest.(check bool) "terminates" true (List.length names <= 2)

(* Diamond: root -> [A, B], A -> [C], B -> [C] *)
let diamond_deps =
  let graph =
    [
      ("root", [make_svc ~instance:"A" (); make_svc ~instance:"B" ()]);
      ("A", [make_svc ~instance:"C" ()]);
      ("B", [make_svc ~instance:"C" ()]);
      ("C", []);
    ]
  in
  fun inst ->
    match List.assoc_opt inst graph with Some deps -> deps | None -> []

let test_collect_diamond () =
  let deps = UFT.collect_dependents ~get_deps:diamond_deps "root" in
  let names = List.map (fun s -> s.Service.instance) deps in
  Alcotest.(check bool) "has A" true (List.mem "A" names) ;
  Alcotest.(check bool) "has B" true (List.mem "B" names) ;
  Alcotest.(check bool) "has C" true (List.mem "C" names)

let test_collect_empty_graph () =
  let get_deps _ = [] in
  let deps = UFT.collect_dependents ~get_deps "any" in
  Alcotest.(check int) "empty graph" 0 (List.length deps)

(* ============================================================ *)
(* dedup_services Tests                                          *)
(* ============================================================ *)

let test_dedup_no_duplicates () =
  let svcs = [make_svc ~instance:"A" (); make_svc ~instance:"B" ()] in
  let result = UFT.dedup_services svcs in
  Alcotest.(check int) "no change" 2 (List.length result)

let test_dedup_with_duplicates () =
  let svcs =
    [
      make_svc ~instance:"A" ();
      make_svc ~instance:"B" ();
      make_svc ~instance:"A" ();
      make_svc ~instance:"C" ();
      make_svc ~instance:"B" ();
    ]
  in
  let result = UFT.dedup_services svcs in
  let names = List.map (fun s -> s.Service.instance) result in
  Alcotest.(check int) "deduped" 3 (List.length result) ;
  Alcotest.(check (list string)) "order preserved" ["A"; "B"; "C"] names

let test_dedup_empty () =
  let result = UFT.dedup_services [] in
  Alcotest.(check int) "empty" 0 (List.length result)

let test_dedup_single () =
  let result = UFT.dedup_services [make_svc ~instance:"X" ()] in
  Alcotest.(check int) "single" 1 (List.length result)

let test_dedup_all_same () =
  let svcs =
    [
      make_svc ~instance:"A" ();
      make_svc ~instance:"A" ();
      make_svc ~instance:"A" ();
    ]
  in
  let result = UFT.dedup_services svcs in
  Alcotest.(check int) "all same -> 1" 1 (List.length result)

(* ============================================================ *)
(* journalctl_args Tests                                         *)
(* ============================================================ *)

let test_journalctl_root () =
  (* We can't easily test is_root in a unit test, so just check the format *)
  let args = FT.journalctl_args "my-unit.service" in
  Alcotest.(check bool)
    "contains unit name"
    true
    (List.mem "my-unit.service" args) ;
  Alcotest.(check bool) "contains journalctl" true (List.mem "journalctl" args)

let test_journalctl_has_no_pager () =
  let args = FT.journalctl_args "x.service" in
  Alcotest.(check bool) "has --no-pager" true (List.mem "--no-pager" args)

(* ============================================================ *)
(* PBT: extract_version_string never crashes                     *)
(* ============================================================ *)

let test_extract_version_no_crash =
  QCheck.Test.make
    ~name:"extract_version_string never crashes"
    ~count:500
    QCheck.string
    (fun s ->
      let _ = UFT.extract_version_string s in
      true)

(* PBT: collect_dependents terminates on any graph *)
let test_collect_terminates =
  let gen =
    QCheck.Gen.(
      let* n_nodes = int_range 0 8 in
      let nodes = List.init n_nodes (fun i -> Printf.sprintf "n%d" i) in
      let* edges =
        list_size
          (return n_nodes)
          (list_size (int_range 0 3) (int_range 0 (max 1 (n_nodes - 1))))
      in
      let graph =
        List.mapi
          (fun i targets ->
            let deps =
              List.filter_map
                (fun t ->
                  let name = Printf.sprintf "n%d" t in
                  if name <> Printf.sprintf "n%d" i then
                    Some (make_svc ~instance:name ())
                  else None)
                targets
            in
            (Printf.sprintf "n%d" i, deps))
          edges
      in
      let root = if nodes = [] then "x" else List.hd nodes in
      return (graph, root))
  in
  QCheck.Test.make
    ~name:"collect_dependents always terminates"
    ~count:200
    (QCheck.make gen)
    (fun (graph, root) ->
      let get_deps inst =
        match List.assoc_opt inst graph with Some d -> d | None -> []
      in
      let _ = UFT.collect_dependents ~get_deps root in
      true)

(* ============================================================ *)
(* Test Runner                                                   *)
(* ============================================================ *)

let () =
  Alcotest.run
    "Instances_actions (pure)"
    [
      ( "extract_version_string",
        [
          Alcotest.test_case "simple" `Quick test_extract_version_simple;
          Alcotest.test_case
            "with prefix"
            `Quick
            test_extract_version_with_prefix;
          Alcotest.test_case "three part" `Quick test_extract_version_three_part;
          Alcotest.test_case "no match" `Quick test_extract_version_no_match;
          Alcotest.test_case "empty" `Quick test_extract_version_empty;
          Alcotest.test_case
            "just number"
            `Quick
            test_extract_version_just_number;
          Alcotest.test_case "multi line" `Quick test_extract_version_multi_line;
        ] );
      ( "role_to_binary_name",
        [
          Alcotest.test_case "node" `Quick test_role_node;
          Alcotest.test_case "baker" `Quick test_role_baker;
          Alcotest.test_case "accuser" `Quick test_role_accuser;
          Alcotest.test_case "dal-node" `Quick test_role_dal_node;
          Alcotest.test_case "dal" `Quick test_role_dal;
          Alcotest.test_case "unknown" `Quick test_role_unknown;
        ] );
      ( "collect_dependents",
        [
          Alcotest.test_case "linear chain" `Quick test_collect_linear;
          Alcotest.test_case "leaf" `Quick test_collect_leaf;
          Alcotest.test_case "nonexistent" `Quick test_collect_no_such_node;
          Alcotest.test_case "cycle" `Quick test_collect_cycle;
          Alcotest.test_case "diamond" `Quick test_collect_diamond;
          Alcotest.test_case "empty graph" `Quick test_collect_empty_graph;
        ] );
      ( "dedup_services",
        [
          Alcotest.test_case "no duplicates" `Quick test_dedup_no_duplicates;
          Alcotest.test_case "with duplicates" `Quick test_dedup_with_duplicates;
          Alcotest.test_case "empty" `Quick test_dedup_empty;
          Alcotest.test_case "single" `Quick test_dedup_single;
          Alcotest.test_case "all same" `Quick test_dedup_all_same;
        ] );
      ( "journalctl_args",
        [
          Alcotest.test_case "contains unit" `Quick test_journalctl_root;
          Alcotest.test_case "has no-pager" `Quick test_journalctl_has_no_pager;
        ] );
      ( "PBT",
        List.map
          QCheck_alcotest.to_alcotest
          [test_extract_version_no_crash; test_collect_terminates] );
    ]
