(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Octez_manager_lib
open Octez_manager_ui
module Navigation = Miaou.Core.Navigation

(* ============================================================ *)
(* Helper: create minimal import_wizard state                    *)
(* ============================================================ *)

let make_state ?(step = Import_wizard.SelectService) ?(external_services = [])
    ?(selected_idx = 0) ?selected_service ?(strategy = Import.Takeover)
    ?custom_name ?network_override ?error ?(cascade = false)
    ?(cascade_chain = []) ?cascade_analysis () : Import_wizard.state =
  {
    step;
    external_services;
    selected_idx;
    selected_service;
    strategy;
    custom_name;
    network_override;
    error;
    cascade;
    cascade_chain;
    cascade_analysis;
  }

let wrap_state = Navigation.make

(* ============================================================ *)
(* move_selection Tests (wrapping mod behavior)                  *)
(* ============================================================ *)

let make_ext_svc name : External_service.t =
  let unknown () : _ External_service.field =
    {value = None; confidence = External_service.Unknown; source = "test"}
  in
  let detected v : _ External_service.field =
    {value = Some v; confidence = External_service.Detected; source = "test"}
  in
  {
    config =
      {
        unit_name = name;
        unit_file_path = None;
        exec_start = "/usr/bin/octez-node run";
        unit_state =
          {active_state = "active"; sub_state = "running"; enabled = Some true};
        user = None;
        group = None;
        working_dir = None;
        environment_files = [];
        role = detected External_service.Node;
        binary_path = unknown ();
        binary_version = unknown ();
        data_dir = detected "/tmp";
        rpc_addr = detected "127.0.0.1:8732";
        net_addr = unknown ();
        network = detected "mainnet";
        history_mode = unknown ();
        node_endpoint = unknown ();
        base_dir = unknown ();
        delegates =
          {
            value = Some [];
            confidence = External_service.Unknown;
            source = "test";
          };
        dal_endpoint = unknown ();
        daily_logs_dir = None;
        extra_args = [];
        parse_warnings = [];
      };
    suggested_instance_name = name;
  }

let test_move_down () =
  let svcs = [make_ext_svc "a"; make_ext_svc "b"; make_ext_svc "c"] in
  let ps = wrap_state (make_state ~external_services:svcs ~selected_idx:0 ()) in
  let ps' = Import_wizard.move_selection ps 1 in
  Alcotest.(check int) "moved to 1" 1 ps'.Navigation.s.selected_idx

let test_move_up () =
  let svcs = [make_ext_svc "a"; make_ext_svc "b"; make_ext_svc "c"] in
  let ps = wrap_state (make_state ~external_services:svcs ~selected_idx:1 ()) in
  let ps' = Import_wizard.move_selection ps (-1) in
  Alcotest.(check int) "moved to 0" 0 ps'.Navigation.s.selected_idx

let test_move_wrap_bottom () =
  let svcs = [make_ext_svc "a"; make_ext_svc "b"; make_ext_svc "c"] in
  let ps = wrap_state (make_state ~external_services:svcs ~selected_idx:2 ()) in
  let ps' = Import_wizard.move_selection ps 1 in
  Alcotest.(check int) "wraps to 0" 0 ps'.Navigation.s.selected_idx

let test_move_wrap_top () =
  let svcs = [make_ext_svc "a"; make_ext_svc "b"; make_ext_svc "c"] in
  let ps = wrap_state (make_state ~external_services:svcs ~selected_idx:0 ()) in
  let ps' = Import_wizard.move_selection ps (-1) in
  Alcotest.(check int) "wraps to 2" 2 ps'.Navigation.s.selected_idx

let test_move_empty () =
  let ps = wrap_state (make_state ~external_services:[] ~selected_idx:0 ()) in
  let ps' = Import_wizard.move_selection ps 1 in
  Alcotest.(check int) "stays at 0" 0 ps'.Navigation.s.selected_idx

let test_move_single () =
  let svcs = [make_ext_svc "only"] in
  let ps = wrap_state (make_state ~external_services:svcs ~selected_idx:0 ()) in
  let ps' = Import_wizard.move_selection ps 1 in
  Alcotest.(check int) "stays at 0" 0 ps'.Navigation.s.selected_idx

(* ============================================================ *)
(* toggle_strategy Tests                                         *)
(* ============================================================ *)

let strategy_to_string = function
  | Import.Takeover -> "Takeover"
  | Import.Clone -> "Clone"

let test_toggle_takeover_to_clone () =
  let ps = wrap_state (make_state ~strategy:Import.Takeover ()) in
  let ps' = Import_wizard.toggle_strategy ps in
  Alcotest.(check string)
    "becomes Clone"
    "Clone"
    (strategy_to_string ps'.Navigation.s.strategy)

let test_toggle_clone_to_takeover () =
  let ps = wrap_state (make_state ~strategy:Import.Clone ()) in
  let ps' = Import_wizard.toggle_strategy ps in
  Alcotest.(check string)
    "becomes Takeover"
    "Takeover"
    (strategy_to_string ps'.Navigation.s.strategy)

let test_toggle_round_trip () =
  let ps = wrap_state (make_state ~strategy:Import.Takeover ()) in
  let ps' = Import_wizard.toggle_strategy ps in
  let ps'' = Import_wizard.toggle_strategy ps' in
  Alcotest.(check string)
    "back to Takeover"
    "Takeover"
    (strategy_to_string ps''.Navigation.s.strategy)

(* ============================================================ *)
(* back Tests (issue #999 - stale overrides on service reselect) *)
(* ============================================================ *)

(* Regression test for #999: selecting service A, setting a network
   override / custom name / cascade while configuring it, then going back
   to the service list must clear that per-service state. Otherwise picking
   a different service B inherits A's stale overrides (e.g. B gets imported
   with A's manually-chosen network). *)
let test_back_from_configure_resets_overrides () =
  let svc_a = make_ext_svc "service-a" in
  let analysis =
    Import_cascade.analyze_dependencies
      ~services:[svc_a]
      ~target_services:[svc_a]
  in
  let ps =
    wrap_state
      (make_state
         ~step:Import_wizard.ConfigureImport
         ~external_services:[svc_a]
         ~selected_service:svc_a
         ~network_override:"mainnet"
         ~custom_name:"my-custom-name"
         ~cascade:true
         ~cascade_chain:[svc_a]
         ~cascade_analysis:analysis
         ())
  in
  let ps' = Import_wizard.back ps in
  let s = ps'.Navigation.s in
  Alcotest.(check bool)
    "step is SelectService"
    true
    (match s.step with Import_wizard.SelectService -> true | _ -> false) ;
  Alcotest.(check bool)
    "selected_service reset"
    true
    (Option.is_none s.selected_service) ;
  Alcotest.(check (option string))
    "network_override reset"
    None
    s.network_override ;
  Alcotest.(check (option string)) "custom_name reset" None s.custom_name ;
  Alcotest.(check bool) "cascade reset" false s.cascade ;
  Alcotest.(check int) "cascade_chain reset" 0 (List.length s.cascade_chain) ;
  Alcotest.(check bool)
    "cascade_analysis reset"
    true
    (Option.is_none s.cascade_analysis)

let test_back_from_review_preserves_configuration () =
  let svc_a = make_ext_svc "service-a" in
  let ps =
    wrap_state
      (make_state
         ~step:Import_wizard.ReviewImport
         ~external_services:[svc_a]
         ~selected_service:svc_a
         ~network_override:"mainnet"
         ~custom_name:"my-custom-name"
         ~cascade:true
         ~cascade_chain:[svc_a]
         ())
  in
  let ps' = Import_wizard.back ps in
  let s = ps'.Navigation.s in
  Alcotest.(check bool)
    "step is ConfigureImport"
    true
    (match s.step with Import_wizard.ConfigureImport -> true | _ -> false) ;
  Alcotest.(check bool)
    "selected_service preserved"
    true
    (Option.is_some s.selected_service) ;
  Alcotest.(check (option string))
    "network_override preserved"
    (Some "mainnet")
    s.network_override ;
  Alcotest.(check (option string))
    "custom_name preserved"
    (Some "my-custom-name")
    s.custom_name ;
  Alcotest.(check bool) "cascade preserved" true s.cascade

(* ============================================================ *)
(* header Tests                                                  *)
(* ============================================================ *)

let contains_substring = Test_string_helpers.contains_substring

let test_header_select () =
  let s = make_state ~step:Import_wizard.SelectService () in
  let lines = Import_wizard.header s in
  let joined = String.concat " " lines in
  Alcotest.(check bool) "mentions Step 1" true (contains_substring joined "1/3")

let test_header_configure () =
  let s = make_state ~step:Import_wizard.ConfigureImport () in
  let lines = Import_wizard.header s in
  let joined = String.concat " " lines in
  Alcotest.(check bool) "mentions Step 2" true (contains_substring joined "2/3")

let test_header_review () =
  let s = make_state ~step:Import_wizard.ReviewImport () in
  let lines = Import_wizard.header s in
  let joined = String.concat " " lines in
  Alcotest.(check bool) "mentions Step 3" true (contains_substring joined "3/3")

let test_header_importing () =
  let s = make_state ~step:Import_wizard.Importing () in
  let lines = Import_wizard.header s in
  let joined = String.concat " " lines in
  Alcotest.(check bool)
    "mentions Importing"
    true
    (contains_substring joined "Importing")

(* ============================================================ *)
(* handled_keys Tests                                            *)
(* ============================================================ *)

let test_handled_keys_has_escape () =
  let keys = Import_wizard.handled_keys () in
  Alcotest.(check bool) "has Escape" true (List.mem Miaou.Core.Keys.Escape keys)

let test_handled_keys_has_enter () =
  let keys = Import_wizard.handled_keys () in
  Alcotest.(check bool) "has Enter" true (List.mem Miaou.Core.Keys.Enter keys)

let test_handled_keys_has_arrows () =
  let keys = Import_wizard.handled_keys () in
  Alcotest.(check bool) "has Up" true (List.mem Miaou.Core.Keys.Up keys) ;
  Alcotest.(check bool) "has Down" true (List.mem Miaou.Core.Keys.Down keys)

(* ============================================================ *)
(* keymap Tests                                                  *)
(* ============================================================ *)

let test_keymap_not_empty () =
  let s = make_state () in
  let km = Import_wizard.keymap s in
  Alcotest.(check bool) "not empty" true (List.length km >= 1)

let test_keymap_has_esc () =
  let s = make_state () in
  let km = Import_wizard.keymap s in
  let keys = List.map (fun kb -> kb.Miaou.Core.Tui_page.key) km in
  Alcotest.(check bool) "has Esc" true (List.mem "Esc" keys)

let test_keymap_has_enter () =
  let s = make_state () in
  let km = Import_wizard.keymap s in
  let keys = List.map (fun kb -> kb.Miaou.Core.Tui_page.key) km in
  Alcotest.(check bool) "has Enter" true (List.mem "Enter" keys)

(* ============================================================ *)
(* toggle_cascade Tests                                         *)
(* ============================================================ *)

let test_toggle_cascade_off_to_on () =
  let svc = make_ext_svc "test-node" in
  let ps =
    wrap_state
      (make_state
         ~external_services:[svc]
         ~selected_service:svc
         ~cascade:false
         ())
  in
  let ps' = Import_wizard.toggle_cascade ps in
  Alcotest.(check bool) "cascade enabled" true ps'.Navigation.s.cascade

let test_toggle_cascade_on_to_off () =
  let svc = make_ext_svc "test-node" in
  let ps =
    wrap_state
      (make_state
         ~external_services:[svc]
         ~selected_service:svc
         ~cascade:true
         ())
  in
  let ps' = Import_wizard.toggle_cascade ps in
  Alcotest.(check bool) "cascade disabled" false ps'.Navigation.s.cascade

let test_toggle_cascade_clears_chain_when_disabled () =
  let svc = make_ext_svc "test-node" in
  let ps =
    wrap_state
      (make_state
         ~external_services:[svc]
         ~selected_service:svc
         ~cascade:true
         ~cascade_chain:[svc]
         ())
  in
  let ps' = Import_wizard.toggle_cascade ps in
  Alcotest.(check int)
    "cascade chain cleared"
    0
    (List.length ps'.Navigation.s.cascade_chain)

let test_toggle_cascade_no_selected_service () =
  let ps = wrap_state (make_state ~cascade:false ()) in
  let ps' = Import_wizard.toggle_cascade ps in
  Alcotest.(check bool) "cascade enabled" true ps'.Navigation.s.cascade ;
  Alcotest.(check int)
    "no chain computed"
    0
    (List.length ps'.Navigation.s.cascade_chain)

(* ============================================================ *)
(* Test Runner                                                   *)
(* ============================================================ *)

let () =
  Alcotest.run
    "Import_wizard (pure)"
    [
      ( "move_selection",
        [
          Alcotest.test_case "down" `Quick test_move_down;
          Alcotest.test_case "up" `Quick test_move_up;
          Alcotest.test_case "wrap bottom" `Quick test_move_wrap_bottom;
          Alcotest.test_case "wrap top" `Quick test_move_wrap_top;
          Alcotest.test_case "empty" `Quick test_move_empty;
          Alcotest.test_case "single" `Quick test_move_single;
        ] );
      ( "toggle_strategy",
        [
          Alcotest.test_case
            "takeover to clone"
            `Quick
            test_toggle_takeover_to_clone;
          Alcotest.test_case
            "clone to takeover"
            `Quick
            test_toggle_clone_to_takeover;
          Alcotest.test_case "round trip" `Quick test_toggle_round_trip;
        ] );
      ( "toggle_cascade",
        [
          Alcotest.test_case "off to on" `Quick test_toggle_cascade_off_to_on;
          Alcotest.test_case "on to off" `Quick test_toggle_cascade_on_to_off;
          Alcotest.test_case
            "clears chain when disabled"
            `Quick
            test_toggle_cascade_clears_chain_when_disabled;
          Alcotest.test_case
            "no selected service"
            `Quick
            test_toggle_cascade_no_selected_service;
        ] );
      ( "back",
        [
          Alcotest.test_case
            "configure -> select resets overrides"
            `Quick
            test_back_from_configure_resets_overrides;
          Alcotest.test_case
            "review -> configure preserves configuration"
            `Quick
            test_back_from_review_preserves_configuration;
        ] );
      ( "header",
        [
          Alcotest.test_case "select step" `Quick test_header_select;
          Alcotest.test_case "configure step" `Quick test_header_configure;
          Alcotest.test_case "review step" `Quick test_header_review;
          Alcotest.test_case "importing step" `Quick test_header_importing;
        ] );
      ( "handled_keys",
        [
          Alcotest.test_case "has Escape" `Quick test_handled_keys_has_escape;
          Alcotest.test_case "has Enter" `Quick test_handled_keys_has_enter;
          Alcotest.test_case "has arrows" `Quick test_handled_keys_has_arrows;
        ] );
      ( "keymap",
        [
          Alcotest.test_case "not empty" `Quick test_keymap_not_empty;
          Alcotest.test_case "has Esc" `Quick test_keymap_has_esc;
          Alcotest.test_case "has Enter" `Quick test_keymap_has_enter;
        ] );
    ]
