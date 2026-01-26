(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Unit tests for form_builder_bundles.ml - pre-built field bundles.
    
    Tests the reusable field bundles for common Octez configurations.
    Since we cannot access field internals, we test by counting fields
    and verifying bundle composition works correctly. *)

open Alcotest
module FBB = Octez_manager_ui.Form_builder_bundles
module FBC = Octez_manager_ui.Form_builder_common

(******************************************************************************)
(*                             TEST MODELS                                   *)
(******************************************************************************)

(** Test model with client config *)
type client_model = {core : FBC.core_service_config; client : FBC.client_config}

(** Test model with node config *)
type node_model = {core : FBC.core_service_config; node : FBC.node_config}

(******************************************************************************)
(*                      CORE SERVICE BUNDLE TESTS                            *)
(******************************************************************************)

(** Test: Core service bundle generates fields *)
let test_core_service_bundle_basic () =
  let fields =
    FBB.core_service_fields
      ~get_core:(fun m -> m.core)
      ~set_core:(fun c m -> {m with core = c})
      ~binary:"octez-node"
      ~subcommand:["run"]
      ()
  in

  (* Should have multiple fields (exact count depends on options) *)
  (* Typically: Instance Name, Service User, App Bin Dir, Logging Mode, Enable on Boot, Start Now, Extra Args *)
  check bool "has fields" true (List.length fields >= 5)

(** Test: Core service bundle with skip_instance_name has fewer fields *)
let test_core_service_skip_instance_name () =
  let fields_full =
    FBB.core_service_fields
      ~get_core:(fun m -> m.core)
      ~set_core:(fun c m -> {m with core = c})
      ~binary:"octez-node"
      ~subcommand:["run"]
      ()
  in

  let fields_skip =
    FBB.core_service_fields
      ~get_core:(fun m -> m.core)
      ~set_core:(fun c m -> {m with core = c})
      ~binary:"octez-node"
      ~subcommand:["run"]
      ~skip_instance_name:true
      ()
  in

  (* Skipping instance name should reduce field count by 1 *)
  check
    int
    "skip_instance_name reduces fields"
    (List.length fields_full - 1)
    (List.length fields_skip)

(** Test: Core service bundle with skip_app_bin_dir has fewer fields *)
let test_core_service_skip_app_bin_dir () =
  let fields_full =
    FBB.core_service_fields
      ~get_core:(fun m -> m.core)
      ~set_core:(fun c m -> {m with core = c})
      ~binary:"octez-node"
      ~subcommand:["run"]
      ()
  in

  let fields_skip =
    FBB.core_service_fields
      ~get_core:(fun m -> m.core)
      ~set_core:(fun c m -> {m with core = c})
      ~binary:"octez-node"
      ~subcommand:["run"]
      ~skip_app_bin_dir:true
      ()
  in

  (* Skipping app_bin_dir should reduce field count *)
  check
    bool
    "skip_app_bin_dir reduces fields"
    true
    (List.length fields_skip < List.length fields_full)

(** Test: Core service bundle with skip_extra_args has fewer fields *)
let test_core_service_skip_extra_args () =
  let fields_full =
    FBB.core_service_fields
      ~get_core:(fun m -> m.core)
      ~set_core:(fun c m -> {m with core = c})
      ~binary:"octez-node"
      ~subcommand:["run"]
      ()
  in

  let fields_skip =
    FBB.core_service_fields
      ~get_core:(fun m -> m.core)
      ~set_core:(fun c m -> {m with core = c})
      ~binary:"octez-node"
      ~subcommand:["run"]
      ~skip_extra_args:true
      ()
  in

  check
    bool
    "skip_extra_args reduces fields"
    true
    (List.length fields_skip < List.length fields_full)

(** Test: Core service bundle with skip_service_fields has fewer fields *)
let test_core_service_skip_service_fields () =
  let fields_full =
    FBB.core_service_fields
      ~get_core:(fun m -> m.core)
      ~set_core:(fun c m -> {m with core = c})
      ~binary:"octez-node"
      ~subcommand:["run"]
      ()
  in

  let fields_skip =
    FBB.core_service_fields
      ~get_core:(fun m -> m.core)
      ~set_core:(fun c m -> {m with core = c})
      ~binary:"octez-node"
      ~subcommand:["run"]
      ~skip_service_fields:true
      ()
  in

  (* Skipping service fields should significantly reduce count *)
  check
    bool
    "skip_service_fields reduces fields significantly"
    true
    (List.length fields_skip < List.length fields_full - 2)

(** Test: Core service bundle in edit mode generates fields *)
let test_core_service_edit_mode () =
  let fields =
    FBB.core_service_fields
      ~get_core:(fun m -> m.core)
      ~set_core:(fun c m -> {m with core = c})
      ~binary:"octez-node"
      ~subcommand:["run"]
      ~edit_mode:true
      ~original_instance:(Some "test-node")
      ()
  in

  (* Edit mode should still have fields *)
  check bool "edit mode has fields" true (List.length fields > 0)

(******************************************************************************)
(*                      CLIENT BUNDLE TESTS                                  *)
(******************************************************************************)

(** Test: Client fields bundle generates fields *)
let test_client_fields_basic () =
  let binary_validator _dir = true in

  let fields : client_model Octez_manager_ui.Form_builder.field list =
    FBB.client_fields_with_autoname
      ~role:"baker"
      ~binary:"octez-baker"
      ~binary_validator
      ~get_core:(fun (m : client_model) -> m.core)
      ~set_core:(fun c (m : client_model) -> {m with core = c})
      ~get_client:(fun (m : client_model) -> m.client)
      ~set_client:(fun cl (m : client_model) -> {m with client = cl})
      ()
  in

  (* Should have at least Node and Base Dir fields *)
  check bool "has client fields" true (List.length fields >= 2)

(** Test: Client fields with skip_base_dir has fewer fields *)
let test_client_fields_skip_base_dir () =
  let binary_validator _dir = true in

  let fields_full : client_model Octez_manager_ui.Form_builder.field list =
    FBB.client_fields_with_autoname
      ~role:"baker"
      ~binary:"octez-baker"
      ~binary_validator
      ~get_core:(fun (m : client_model) -> m.core)
      ~set_core:(fun c (m : client_model) -> {m with core = c})
      ~get_client:(fun (m : client_model) -> m.client)
      ~set_client:(fun cl (m : client_model) -> {m with client = cl})
      ()
  in

  let fields_skip : client_model Octez_manager_ui.Form_builder.field list =
    FBB.client_fields_with_autoname
      ~role:"baker"
      ~binary:"octez-baker"
      ~binary_validator
      ~get_core:(fun (m : client_model) -> m.core)
      ~set_core:(fun c (m : client_model) -> {m with core = c})
      ~get_client:(fun (m : client_model) -> m.client)
      ~set_client:(fun cl (m : client_model) -> {m with client = cl})
      ~skip_base_dir:true
      ()
  in

  check
    bool
    "skip_base_dir reduces fields"
    true
    (List.length fields_skip < List.length fields_full)

(******************************************************************************)
(*                         NODE BUNDLE TESTS                                 *)
(******************************************************************************)

(** Test: Node fields bundle generates expected number of fields *)
let test_node_fields_basic () =
  let fields =
    FBB.node_fields
      ~get_node:(fun m -> m.node)
      ~set_node:(fun n m -> {m with node = n})
      ()
  in

  (* Should have Network, History Mode, Data Dir, RPC Address, P2P Address *)
  check bool "has node fields" true (List.length fields >= 5)

(** Test: Node fields with skip options reduce field count *)
let test_node_fields_skip_network () =
  let fields_full =
    FBB.node_fields
      ~get_node:(fun m -> m.node)
      ~set_node:(fun n m -> {m with node = n})
      ()
  in

  let fields_skip =
    FBB.node_fields
      ~get_node:(fun m -> m.node)
      ~set_node:(fun n m -> {m with node = n})
      ~skip_network:true
      ()
  in

  check
    bool
    "skip_network reduces fields"
    true
    (List.length fields_skip < List.length fields_full)

(** Test: Node fields with skip_data_dir reduces fields *)
let test_node_fields_skip_data_dir () =
  let fields_full =
    FBB.node_fields
      ~get_node:(fun m -> m.node)
      ~set_node:(fun n m -> {m with node = n})
      ()
  in

  let fields_skip =
    FBB.node_fields
      ~get_node:(fun m -> m.node)
      ~set_node:(fun n m -> {m with node = n})
      ~skip_data_dir:true
      ()
  in

  check
    bool
    "skip_data_dir reduces fields"
    true
    (List.length fields_skip < List.length fields_full)

(** Test: Node fields with skip_addresses reduces fields *)
let test_node_fields_skip_addresses () =
  let fields_full =
    FBB.node_fields
      ~get_node:(fun m -> m.node)
      ~set_node:(fun n m -> {m with node = n})
      ()
  in

  let fields_skip =
    FBB.node_fields
      ~get_node:(fun m -> m.node)
      ~set_node:(fun n m -> {m with node = n})
      ~skip_addresses:true
      ()
  in

  (* Skipping addresses removes 2 fields (RPC + P2P) *)
  check
    bool
    "skip_addresses reduces fields significantly"
    true
    (List.length fields_skip <= List.length fields_full - 2)

(******************************************************************************)
(*                      BUNDLE COMPOSITION TESTS                             *)
(******************************************************************************)

(** Test: Combining core + node fields *)
let test_combined_core_and_node_fields () =
  (* Use node_model for node tests *)
  let core_fields : node_model Octez_manager_ui.Form_builder.field list =
    FBB.core_service_fields
      ~get_core:(fun (m : node_model) -> m.core)
      ~set_core:(fun c (m : node_model) -> {m with core = c})
      ~binary:"octez-node"
      ~subcommand:["run"]
      ()
  in

  let node_fields : node_model Octez_manager_ui.Form_builder.field list =
    FBB.node_fields
      ~get_node:(fun (m : node_model) -> m.node)
      ~set_node:(fun n (m : node_model) -> {m with node = n})
      ()
  in

  let combined = core_fields @ node_fields in

  (* Combined should have more fields than either alone *)
  check
    bool
    "combined has more than core"
    true
    (List.length combined > List.length core_fields) ;

  check
    bool
    "combined has more than node"
    true
    (List.length combined > List.length node_fields) ;

  (* Combined should equal sum of both *)
  check
    int
    "combined equals sum"
    (List.length core_fields + List.length node_fields)
    (List.length combined)

(** Test: Combining core + client fields *)
let test_combined_core_and_client_fields () =
  let binary_validator _dir = true in

  (* Use client_model for client tests *)
  let core_fields : client_model Octez_manager_ui.Form_builder.field list =
    FBB.core_service_fields
      ~get_core:(fun (m : client_model) -> m.core)
      ~set_core:(fun c (m : client_model) -> {m with core = c})
      ~binary:"octez-baker"
      ~subcommand:["run"]
      ()
  in

  let client_fields : client_model Octez_manager_ui.Form_builder.field list =
    FBB.client_fields_with_autoname
      ~role:"baker"
      ~binary:"octez-baker"
      ~binary_validator
      ~get_core:(fun (m : client_model) -> m.core)
      ~set_core:(fun c (m : client_model) -> {m with core = c})
      ~get_client:(fun (m : client_model) -> m.client)
      ~set_client:(fun cl (m : client_model) -> {m with client = cl})
      ()
  in

  let combined = core_fields @ client_fields in

  check bool "combined has fields" true (List.length combined > 0) ;

  check
    int
    "combined equals sum"
    (List.length core_fields + List.length client_fields)
    (List.length combined)

(******************************************************************************)
(*                              TEST SUITE                                   *)
(******************************************************************************)

let () =
  run
    "Form Builder Bundles"
    [
      ( "Core Service Bundle",
        [
          test_case "basic fields" `Quick test_core_service_bundle_basic;
          test_case
            "skip_instance_name"
            `Quick
            test_core_service_skip_instance_name;
          test_case "skip_app_bin_dir" `Quick test_core_service_skip_app_bin_dir;
          test_case "skip_extra_args" `Quick test_core_service_skip_extra_args;
          test_case
            "skip_service_fields"
            `Quick
            test_core_service_skip_service_fields;
          test_case "edit_mode" `Quick test_core_service_edit_mode;
        ] );
      ( "Client Bundle",
        [
          test_case "basic fields" `Quick test_client_fields_basic;
          test_case "skip_base_dir" `Quick test_client_fields_skip_base_dir;
        ] );
      ( "Node Bundle",
        [
          test_case "basic fields" `Quick test_node_fields_basic;
          test_case "skip_network" `Quick test_node_fields_skip_network;
          test_case "skip_data_dir" `Quick test_node_fields_skip_data_dir;
          test_case "skip_addresses" `Quick test_node_fields_skip_addresses;
        ] );
      ( "Bundle Composition",
        [
          test_case "core + node" `Quick test_combined_core_and_node_fields;
          test_case "core + client" `Quick test_combined_core_and_client_fields;
        ] );
    ]
