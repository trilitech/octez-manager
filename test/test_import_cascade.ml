(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

open Octez_manager_lib

(* ── Helpers ──────────────────────────────────────────────────────── *)

let default_unit_state : External_service.unit_state =
  {active_state = "active"; sub_state = "running"; enabled = Some true}

(** Create a mock external service with optional RPC addr and node endpoint.
    Dependencies are resolved by matching node_endpoint to rpc_addr. *)
let make_service ~unit_name ?role ?rpc_addr ?node_endpoint ?dal_endpoint () :
    External_service.t =
  let config =
    External_service.empty_config
      ~unit_name
      ~exec_start:("/usr/bin/" ^ unit_name)
      ~unit_state:default_unit_state
  in
  let config =
    {
      config with
      role =
        (match role with
        | Some r -> External_service.detected ~source:"test" r
        | None -> config.role);
      rpc_addr =
        (match rpc_addr with
        | Some addr -> External_service.detected ~source:"test" addr
        | None -> config.rpc_addr);
      node_endpoint =
        (match node_endpoint with
        | Some ep -> External_service.detected ~source:"test" ep
        | None -> config.node_endpoint);
      dal_endpoint =
        (match dal_endpoint with
        | Some ep -> External_service.detected ~source:"test" ep
        | None -> config.dal_endpoint);
    }
  in
  {config; suggested_instance_name = unit_name}

(* ── Mock service graphs ─────────────────────────────────────────── *)

(** Simple chain: node -> baker -> accuser *)
let simple_chain () =
  let node =
    make_service
      ~unit_name:"octez-node"
      ~role:Node
      ~rpc_addr:"127.0.0.1:8732"
      ()
  in
  let baker =
    make_service
      ~unit_name:"octez-baker"
      ~role:Baker
      ~node_endpoint:"http://127.0.0.1:8732"
      ()
  in
  let accuser =
    make_service
      ~unit_name:"octez-accuser"
      ~role:Accuser
      ~node_endpoint:"http://127.0.0.1:8732"
      ()
  in
  (node, baker, accuser)

(** Diamond dependency: node -> {baker, dal} -> (both depend on node) *)
let diamond_graph () =
  let node =
    make_service
      ~unit_name:"octez-node"
      ~role:Node
      ~rpc_addr:"127.0.0.1:8732"
      ()
  in
  let dal =
    make_service
      ~unit_name:"octez-dal"
      ~role:Dal_node
      ~node_endpoint:"http://127.0.0.1:8732"
      ~rpc_addr:"127.0.0.1:10732"
      ()
  in
  let baker =
    make_service
      ~unit_name:"octez-baker"
      ~role:Baker
      ~node_endpoint:"http://127.0.0.1:8732"
      ~dal_endpoint:"http://127.0.0.1:10732"
      ()
  in
  (node, dal, baker)

(* ── Alcotest testables ──────────────────────────────────────────── *)

let list_string = Alcotest.(list string)

let list_list_string = Alcotest.(list (list string))

(* ── Tests: topological_sort ─────────────────────────────────────── *)

let test_topo_sort_single_node () =
  let node = make_service ~unit_name:"node" ~rpc_addr:"127.0.0.1:8732" () in
  let all = [node] in
  let nodes =
    Import_cascade.For_tests.build_graph_nodes
      ~all_services:all
      ~target_services:all
  in
  let sorted, cycles = Import_cascade.For_tests.topological_sort nodes in
  Alcotest.(check list_string) "single node" ["node"] sorted ;
  Alcotest.(check list_list_string) "no cycles" [] cycles

let test_topo_sort_linear_chain () =
  let node, baker, accuser = simple_chain () in
  let all = [node; baker; accuser] in
  let nodes =
    Import_cascade.For_tests.build_graph_nodes
      ~all_services:all
      ~target_services:all
  in
  let sorted, cycles = Import_cascade.For_tests.topological_sort nodes in
  (* Node must come before baker and accuser *)
  let node_idx =
    List.find_index (fun s -> s = "octez-node") sorted |> Option.get
  in
  let baker_idx =
    List.find_index (fun s -> s = "octez-baker") sorted |> Option.get
  in
  let accuser_idx =
    List.find_index (fun s -> s = "octez-accuser") sorted |> Option.get
  in
  Alcotest.(check bool) "node before baker" true (node_idx < baker_idx) ;
  Alcotest.(check bool) "node before accuser" true (node_idx < accuser_idx) ;
  Alcotest.(check list_list_string) "no cycles" [] cycles

let test_topo_sort_diamond () =
  let node, dal, baker = diamond_graph () in
  let all = [node; dal; baker] in
  let nodes =
    Import_cascade.For_tests.build_graph_nodes
      ~all_services:all
      ~target_services:all
  in
  let sorted, cycles = Import_cascade.For_tests.topological_sort nodes in
  let idx name = List.find_index (fun s -> s = name) sorted |> Option.get in
  (* node must come first, then dal, then baker (baker depends on both node and dal) *)
  Alcotest.(check bool)
    "node before dal"
    true
    (idx "octez-node" < idx "octez-dal") ;
  Alcotest.(check bool)
    "node before baker"
    true
    (idx "octez-node" < idx "octez-baker") ;
  Alcotest.(check bool)
    "dal before baker"
    true
    (idx "octez-dal" < idx "octez-baker") ;
  Alcotest.(check list_list_string) "no cycles" [] cycles

let test_topo_sort_independent_services () =
  let node1 =
    make_service ~unit_name:"node1" ~role:Node ~rpc_addr:"127.0.0.1:8732" ()
  in
  let node2 =
    make_service ~unit_name:"node2" ~role:Node ~rpc_addr:"127.0.0.1:8733" ()
  in
  let all = [node1; node2] in
  let nodes =
    Import_cascade.For_tests.build_graph_nodes
      ~all_services:all
      ~target_services:all
  in
  let sorted, cycles = Import_cascade.For_tests.topological_sort nodes in
  Alcotest.(check int) "both present" 2 (List.length sorted) ;
  Alcotest.(check list_list_string) "no cycles" [] cycles

(* ── Tests: analyze_dependencies ─────────────────────────────────── *)

let test_analyze_simple_chain () =
  let node, baker, accuser = simple_chain () in
  let all = [node; baker; accuser] in
  let analysis =
    Import_cascade.analyze_dependencies ~services:all ~target_services:all
  in
  Alcotest.(check int) "3 nodes" 3 (List.length analysis.nodes) ;
  Alcotest.(check list_list_string) "no cycles" [] analysis.cycles ;
  (* import_order should put node first *)
  match analysis.import_order with
  | first :: _ ->
      Alcotest.(check string) "node first in import order" "octez-node" first
  | [] -> Alcotest.fail "empty import order"

let test_analyze_external_dependents () =
  let node, baker, _accuser = simple_chain () in
  (* Only import the node, baker is external *)
  let all = [node; baker] in
  let analysis =
    Import_cascade.analyze_dependencies ~services:all ~target_services:[node]
  in
  (* Baker depends on node but is not in target set -> external dependent *)
  Alcotest.(check int)
    "one external dependent"
    1
    (List.length analysis.external_dependents) ;
  let dep_name, dep_target = List.hd analysis.external_dependents in
  Alcotest.(check string) "baker is the dependent" "octez-baker" dep_name ;
  Alcotest.(check string) "depends on node" "octez-node" dep_target

let test_analyze_no_external_dependents_when_all_imported () =
  let node, baker, accuser = simple_chain () in
  let all = [node; baker; accuser] in
  let analysis =
    Import_cascade.analyze_dependencies ~services:all ~target_services:all
  in
  Alcotest.(check int)
    "no external dependents"
    0
    (List.length analysis.external_dependents)

(* ── Tests: validate_cascade ─────────────────────────────────────── *)

let test_validate_ok_clone () =
  let node, baker, accuser = simple_chain () in
  let all = [node; baker; accuser] in
  let result =
    Import_cascade.validate_cascade
      ~services:all
      ~target_services:all
      ~strategy:Installer_types.Clone
  in
  Alcotest.(check bool) "clone succeeds" true (Result.is_ok result)

let test_validate_ok_takeover_all () =
  let node, baker, accuser = simple_chain () in
  let all = [node; baker; accuser] in
  let result =
    Import_cascade.validate_cascade
      ~services:all
      ~target_services:all
      ~strategy:Installer_types.Takeover
  in
  Alcotest.(check bool) "takeover all succeeds" true (Result.is_ok result)

let test_validate_takeover_external_dependents_fails () =
  let node, baker, _accuser = simple_chain () in
  let all = [node; baker] in
  let result =
    Import_cascade.validate_cascade
      ~services:all
      ~target_services:[node]
      ~strategy:Installer_types.Takeover
  in
  match result with
  | Error (Import_cascade.External_dependents_exist deps) ->
      Alcotest.(check int) "one dependent" 1 (List.length deps)
  | Error _ -> Alcotest.fail "wrong error type"
  | Ok () -> Alcotest.fail "should have failed"

let test_validate_clone_allows_external_dependents () =
  let node, baker, _accuser = simple_chain () in
  let all = [node; baker] in
  let result =
    Import_cascade.validate_cascade
      ~services:all
      ~target_services:[node]
      ~strategy:Installer_types.Clone
  in
  Alcotest.(check bool)
    "clone allows external dependents"
    true
    (Result.is_ok result)

let test_validate_missing_dependencies () =
  (* Baker without its node in the service list *)
  let baker =
    make_service
      ~unit_name:"octez-baker"
      ~role:Baker
      ~node_endpoint:"http://127.0.0.1:9999"
      ()
  in
  (* The baker's endpoint doesn't match any service's rpc_addr,
     so there are no dependencies to be "missing" from the target set.
     Missing dependencies = deps found in the graph but not in target_services. *)
  (* To test missing deps, we need a service whose dependency IS in all_services
     but NOT in target_services *)
  let node =
    make_service
      ~unit_name:"octez-node"
      ~role:Node
      ~rpc_addr:"127.0.0.1:8732"
      ()
  in
  let baker2 =
    make_service
      ~unit_name:"octez-baker"
      ~role:Baker
      ~node_endpoint:"http://127.0.0.1:8732"
      ()
  in
  let all = [node; baker2] in
  let result =
    Import_cascade.validate_cascade
      ~services:all
      ~target_services:[baker; baker2]
      ~strategy:Installer_types.Clone
  in
  match result with
  | Error (Import_cascade.Missing_dependencies missing) ->
      Alcotest.(check bool) "has missing deps" true (List.length missing > 0)
  | Error _ -> Alcotest.fail "wrong error type"
  | Ok () ->
      (* If baker2's dep (node) is in all_services but not in target_services,
         it should be flagged as missing *)
      Alcotest.fail "should have reported missing dependencies"

(* ── Tests: get_dependency_chain ─────────────────────────────────── *)

let test_dependency_chain_node_has_no_deps () =
  let node, baker, accuser = simple_chain () in
  let all = [node; baker; accuser] in
  let chain =
    Import_cascade.get_dependency_chain ~service:node ~all_services:all
  in
  Alcotest.(check int) "node has only itself" 1 (List.length chain) ;
  let name = (List.hd chain).config.unit_name in
  Alcotest.(check string) "is the node" "octez-node" name

let test_dependency_chain_baker_includes_node () =
  let node, baker, accuser = simple_chain () in
  let all = [node; baker; accuser] in
  let chain =
    Import_cascade.get_dependency_chain ~service:baker ~all_services:all
  in
  let names =
    List.map (fun (s : External_service.t) -> s.config.unit_name) chain
  in
  Alcotest.(check bool) "includes node" true (List.mem "octez-node" names) ;
  Alcotest.(check bool) "includes baker" true (List.mem "octez-baker" names) ;
  (* Node should come before baker in the chain *)
  let node_idx =
    List.find_index (fun n -> n = "octez-node") names |> Option.get
  in
  let baker_idx =
    List.find_index (fun n -> n = "octez-baker") names |> Option.get
  in
  Alcotest.(check bool) "node before baker" true (node_idx < baker_idx)

let test_dependency_chain_diamond () =
  let node, dal, baker = diamond_graph () in
  let all = [node; dal; baker] in
  let chain =
    Import_cascade.get_dependency_chain ~service:baker ~all_services:all
  in
  let names =
    List.map (fun (s : External_service.t) -> s.config.unit_name) chain
  in
  Alcotest.(check bool) "includes node" true (List.mem "octez-node" names) ;
  Alcotest.(check bool) "includes dal" true (List.mem "octez-dal" names) ;
  Alcotest.(check bool) "includes baker" true (List.mem "octez-baker" names)

(* ── Tests: get_full_cascade ─────────────────────────────────────── *)

let test_full_cascade_from_node () =
  let node, baker, accuser = simple_chain () in
  let all = [node; baker; accuser] in
  let cascade =
    Import_cascade.get_full_cascade ~service:node ~all_services:all
  in
  let names =
    List.map (fun (s : External_service.t) -> s.config.unit_name) cascade
  in
  (* Full cascade from node should include everything that depends on it *)
  Alcotest.(check bool) "includes node" true (List.mem "octez-node" names) ;
  Alcotest.(check bool) "includes baker" true (List.mem "octez-baker" names) ;
  Alcotest.(check bool) "includes accuser" true (List.mem "octez-accuser" names) ;
  Alcotest.(check int) "all 3 services" 3 (List.length cascade)

let test_full_cascade_from_baker () =
  let node, baker, accuser = simple_chain () in
  let all = [node; baker; accuser] in
  let cascade =
    Import_cascade.get_full_cascade ~service:baker ~all_services:all
  in
  let names =
    List.map (fun (s : External_service.t) -> s.config.unit_name) cascade
  in
  (* Baker depends on node, and accuser also depends on node,
     so the full cascade should include all three *)
  Alcotest.(check bool) "includes node" true (List.mem "octez-node" names) ;
  Alcotest.(check bool) "includes baker" true (List.mem "octez-baker" names)

let test_full_cascade_independent_service_excluded () =
  let node, baker, _accuser = simple_chain () in
  let independent =
    make_service
      ~unit_name:"other-node"
      ~role:Node
      ~rpc_addr:"127.0.0.1:9999"
      ()
  in
  let all = [node; baker; independent] in
  let cascade =
    Import_cascade.get_full_cascade ~service:baker ~all_services:all
  in
  let names =
    List.map (fun (s : External_service.t) -> s.config.unit_name) cascade
  in
  Alcotest.(check bool)
    "independent not included"
    false
    (List.mem "other-node" names)

(* ── Tests: list_drop helper ─────────────────────────────────────── *)

let test_list_drop_zero () =
  Alcotest.(check (list int))
    "drop 0"
    [1; 2; 3]
    (Import_cascade.For_tests.list_drop 0 [1; 2; 3])

let test_list_drop_some () =
  Alcotest.(check (list int))
    "drop 2"
    [3]
    (Import_cascade.For_tests.list_drop 2 [1; 2; 3])

let test_list_drop_all () =
  Alcotest.(check (list int))
    "drop all"
    []
    (Import_cascade.For_tests.list_drop 5 [1; 2; 3])

let test_list_drop_empty () =
  Alcotest.(check (list int))
    "drop from empty"
    []
    (Import_cascade.For_tests.list_drop 3 [])

(* ── Tests: pp functions ─────────────────────────────────────────── *)

let test_pp_cycle_error () =
  let err = Import_cascade.Cycle_detected ["a"; "b"; "c"; "a"] in
  let s = Format.asprintf "%a" Import_cascade.pp_validation_error err in
  let has_prefix prefix str =
    String.length str >= String.length prefix
    && String.sub str 0 (String.length prefix) = prefix
  in
  Alcotest.(check bool) "contains cycle" true (has_prefix "Dependency cycle" s)

let test_pp_external_dependents_error () =
  let err = Import_cascade.External_dependents_exist [("baker", "node")] in
  let s = Format.asprintf "%a" Import_cascade.pp_validation_error err in
  let has_prefix prefix str =
    String.length str >= String.length prefix
    && String.sub str 0 (String.length prefix) = prefix
  in
  Alcotest.(check bool)
    "contains external"
    true
    (has_prefix "External services" s)

let test_pp_analysis () =
  let node, baker, accuser = simple_chain () in
  let all = [node; baker; accuser] in
  let analysis =
    Import_cascade.analyze_dependencies ~services:all ~target_services:all
  in
  let s = Format.asprintf "%a" Import_cascade.pp_analysis analysis in
  Alcotest.(check bool) "non-empty output" true (String.length s > 0)

(* ── Test suite ──────────────────────────────────────────────────── *)

let () =
  Alcotest.run
    "Import_cascade"
    [
      ( "topological_sort",
        [
          Alcotest.test_case "single node" `Quick test_topo_sort_single_node;
          Alcotest.test_case "linear chain" `Quick test_topo_sort_linear_chain;
          Alcotest.test_case "diamond" `Quick test_topo_sort_diamond;
          Alcotest.test_case
            "independent"
            `Quick
            test_topo_sort_independent_services;
        ] );
      ( "analyze_dependencies",
        [
          Alcotest.test_case "simple chain" `Quick test_analyze_simple_chain;
          Alcotest.test_case
            "external dependents"
            `Quick
            test_analyze_external_dependents;
          Alcotest.test_case
            "no external when all imported"
            `Quick
            test_analyze_no_external_dependents_when_all_imported;
        ] );
      ( "validate_cascade",
        [
          Alcotest.test_case "clone ok" `Quick test_validate_ok_clone;
          Alcotest.test_case
            "takeover all ok"
            `Quick
            test_validate_ok_takeover_all;
          Alcotest.test_case
            "takeover external deps fails"
            `Quick
            test_validate_takeover_external_dependents_fails;
          Alcotest.test_case
            "clone allows external deps"
            `Quick
            test_validate_clone_allows_external_dependents;
          Alcotest.test_case
            "missing dependencies"
            `Quick
            test_validate_missing_dependencies;
        ] );
      ( "get_dependency_chain",
        [
          Alcotest.test_case
            "node has no deps"
            `Quick
            test_dependency_chain_node_has_no_deps;
          Alcotest.test_case
            "baker includes node"
            `Quick
            test_dependency_chain_baker_includes_node;
          Alcotest.test_case "diamond" `Quick test_dependency_chain_diamond;
        ] );
      ( "get_full_cascade",
        [
          Alcotest.test_case "from node" `Quick test_full_cascade_from_node;
          Alcotest.test_case "from baker" `Quick test_full_cascade_from_baker;
          Alcotest.test_case
            "independent excluded"
            `Quick
            test_full_cascade_independent_service_excluded;
        ] );
      ( "list_drop",
        [
          Alcotest.test_case "drop zero" `Quick test_list_drop_zero;
          Alcotest.test_case "drop some" `Quick test_list_drop_some;
          Alcotest.test_case "drop all" `Quick test_list_drop_all;
          Alcotest.test_case "drop empty" `Quick test_list_drop_empty;
        ] );
      ( "pretty_printing",
        [
          Alcotest.test_case "pp cycle error" `Quick test_pp_cycle_error;
          Alcotest.test_case
            "pp external deps error"
            `Quick
            test_pp_external_dependents_error;
          Alcotest.test_case "pp analysis" `Quick test_pp_analysis;
        ] );
    ]
