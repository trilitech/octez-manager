(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Alcotest
open Octez_manager_lib
module Data = Octez_manager_ui.Data
module Service_state = Data.Service_state
module Topology = Octez_manager_ui.Topology_page

let make_service_state ?(role = "node") ?(depends_on = None) ?(dependents = [])
    instance =
  let service =
    {
      Service.instance;
      role;
      network = "mainnet";
      history_mode = History_mode.Rolling;
      data_dir = "/tmp/" ^ instance;
      rpc_addr = Rpc_addr.of_string "127.0.0.1:8732";
      net_addr = "0.0.0.0:9732";
      service_user = "octez";
      app_bin_dir = "/usr/bin";
      bin_source = None;
      created_at = "2026-01-15 00:00:00";
      logging_mode = Logging_mode.Journald;
      snapshot_auto = false;
      snapshot_uri = None;
      snapshot_network_slug = None;
      snapshot_no_check = false;
      extra_args = [];
      depends_on;
      dependents;
      signer_mode = None;
      signer_uri = None;
      group = None;
    }
  in
  {
    Service_state.service;
    enabled = Some true;
    active = Some true;
    status = Service_state.Running;
    status_text = None;
  }

let node_names nodes =
  List.map (fun (n : Topology.tree_node) -> n.svc.Service.instance) nodes

let test_build_tree_depends_on_hierarchy () =
  let node = make_service_state "node-a" in
  let baker =
    make_service_state ~role:"baker" ~depends_on:(Some "node-a") "baker-a"
  in
  let trees = Topology.build_tree [node; baker] in
  check (list string) "single root" ["node-a"] (node_names trees) ;
  match trees with
  | [root] ->
      check (list string) "child baker" ["baker-a"] (node_names root.children)
  | _ -> fail "unexpected tree shape"

let test_build_tree_includes_dependents_edge () =
  let primary = make_service_state "node-primary" in
  let extra = make_service_state ~dependents:["baker-a"] "node-extra" in
  let baker =
    make_service_state ~role:"baker" ~depends_on:(Some "node-primary") "baker-a"
  in
  let trees = Topology.build_tree [primary; extra; baker] in
  check
    (list string)
    "both nodes remain roots"
    ["node-primary"; "node-extra"]
    (node_names trees) ;
  let find_root name =
    List.find_opt
      (fun (node : Topology.tree_node) ->
        String.equal node.svc.Service.instance name)
      trees
  in
  let child_names name =
    match find_root name with
    | None -> []
    | Some root -> node_names root.children
  in
  check
    (list string)
    "depends_on child"
    ["baker-a"]
    (child_names "node-primary") ;
  check (list string) "dependents child" ["baker-a"] (child_names "node-extra")

let test_build_tree_deduplicates_same_child_for_same_parent () =
  let node = make_service_state ~dependents:["baker-a"] "node-a" in
  let baker =
    make_service_state ~role:"baker" ~depends_on:(Some "node-a") "baker-a"
  in
  let trees = Topology.build_tree [node; baker] in
  match trees with
  | [root] ->
      check int "child only once" 1 (List.length root.children) ;
      check (list string) "same child" ["baker-a"] (node_names root.children)
  | _ -> fail "unexpected tree shape"

let () =
  run
    "Topology Page"
    [
      ( "build_tree",
        [
          test_case
            "depends_on hierarchy"
            `Quick
            test_build_tree_depends_on_hierarchy;
          test_case
            "dependents edge"
            `Quick
            test_build_tree_includes_dependents_edge;
          test_case
            "deduplicates same child"
            `Quick
            test_build_tree_deduplicates_same_child_for_same_parent;
        ] );
    ]
