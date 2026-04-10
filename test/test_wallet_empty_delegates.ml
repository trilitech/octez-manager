(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Regression test for wallet modal with 0 delegates.
    
    Bug: When a baker instance has 0 delegates configured in its base dir,
    the wallet modal shows "Unable to fetch wallet data — node may be unreachable"
    which is misleading (the node may be fine, there are just no keys).
    
    Expected: The modal should show a clear "No delegates found" message instead. *)

open Alcotest
module HD = Lib_miaou_internal.Headless_driver
module TH = Tui_test_helpers_lib.Tui_test_helpers
module Mock = Mock_service_helpers_lib.Mock_service_helpers
module Delegate_scheduler = Octez_manager_ui.Delegate_scheduler
module Instances_wallet = Octez_manager_ui.Instances_wallet

(* ============================================================ *)
(* Test: render_wallet_header with empty delegates *)
(* ============================================================ *)

let test_render_wallet_header_empty_delegates () =
  TH.with_test_env (fun () ->
      (* Inject test data: baker with empty delegates *)
      Delegate_scheduler.Internal_for_tests.set_config
        ~instance:"test-baker"
        ~delegates:[]
        ~node_endpoint:(Some "http://localhost:8732")
        ~has_dal:false ;

      (* Call render_wallet_header with empty delegates list *)
      let header =
        Instances_wallet.Internal_for_tests.render_wallet_header
          ~pkh:""
          ~delegates:[]
          ~cols:80
      in

      (* The header should NOT show the misleading error *)
      check
        bool
        "does not show misleading node unreachable error"
        false
        (TH.contains_substring header "node may be unreachable"
        || TH.contains_substring header "Unable to fetch wallet data") ;

      (* Instead, it should show a helpful message about no delegates *)
      check
        bool
        "shows no delegates message"
        true
        (TH.contains_substring header "No delegate"
        || TH.contains_substring header "no delegate") ;

      (* Check for exact message *)
      check
        bool
        "shows exact empty wallet message"
        true
        (TH.contains_substring header "No delegates found in wallet"))

(* ============================================================ *)
(* Test: render_wallet_header with delegates but no cache *)
(* ============================================================ *)

let test_render_wallet_header_with_delegates_no_cache () =
  TH.with_test_env (fun () ->
      (* Set up baker with delegates but no wallet data in cache *)
      Delegate_scheduler.Internal_for_tests.set_config
        ~instance:"test-baker-2"
        ~delegates:["tz1abc123"]
        ~node_endpoint:(Some "http://localhost:8732")
        ~has_dal:false ;

      (* Call render_wallet_header with a delegate but no cached wallet data *)
      let header =
        Instances_wallet.Internal_for_tests.render_wallet_header
          ~pkh:"tz1abc123"
          ~delegates:["tz1abc123"]
          ~cols:80
      in

      (* This SHOULD show the "node may be unreachable" error
         because we have a delegate but can't fetch its data *)
      check
        bool
        "shows node unreachable error when delegate exists but data missing"
        true
        (TH.contains_substring header "node may be unreachable"
        || TH.contains_substring header "Unable to fetch wallet data"))

(* ============================================================ *)
(* Test Suite *)
(* ============================================================ *)

let () =
  run
    "Wallet Modal - Empty Delegates"
    [
      ( "render_wallet_header",
        [
          test_case
            "shows correct message with empty delegates"
            `Quick
            test_render_wallet_header_empty_delegates;
          test_case
            "shows node error when delegate exists but no cache"
            `Quick
            test_render_wallet_header_with_delegates_no_cache;
        ] );
    ]
