(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Tests for Baker_ops.build_command and describe_operation.

    Validates that each wallet_operation variant produces the correct
    octez-client command-line argv and human-readable description. *)

open Alcotest
module BO = Octez_manager_ui.Baker_ops
module BWD = Octez_manager_ui.Baker_wallet_data

let bin = "/usr/bin/octez-client"

let endpoint = "http://localhost:8732"

let alias = "baker1"

(* ── build_command ───────────────────────────────────────── *)

let test_build_register () =
  let cmd =
    BO.build_command
      ~octez_client_bin:bin
      ~endpoint
      ~base_dir:None
      ~password_file:None
      ~alias
      ~op:Register
  in
  check
    (list string)
    "register"
    [
      bin;
      "--endpoint";
      endpoint;
      "register";
      "key";
      alias;
      "as";
      "delegate";
      "--burn-cap";
      "1";
    ]
    cmd

let test_build_stake () =
  let cmd =
    BO.build_command
      ~octez_client_bin:bin
      ~endpoint
      ~base_dir:None
      ~password_file:None
      ~alias
      ~op:(Stake {amount = "1000"})
  in
  check
    (list string)
    "stake"
    [
      bin;
      "--endpoint";
      endpoint;
      "stake";
      "1000";
      "for";
      alias;
      "--burn-cap";
      "1";
    ]
    cmd

let test_build_unstake () =
  let cmd =
    BO.build_command
      ~octez_client_bin:bin
      ~endpoint
      ~base_dir:None
      ~password_file:None
      ~alias
      ~op:(Unstake {amount = "500"})
  in
  check
    (list string)
    "unstake"
    [
      bin;
      "--endpoint";
      endpoint;
      "unstake";
      "500";
      "for";
      alias;
      "--burn-cap";
      "1";
    ]
    cmd

let test_build_finalize_unstake () =
  let cmd =
    BO.build_command
      ~octez_client_bin:bin
      ~endpoint
      ~base_dir:None
      ~password_file:None
      ~alias
      ~op:Finalize_unstake
  in
  check
    (list string)
    "finalize"
    [
      bin;
      "--endpoint";
      endpoint;
      "finalize";
      "unstake";
      "for";
      alias;
      "--burn-cap";
      "1";
    ]
    cmd

let test_build_transfer () =
  let cmd =
    BO.build_command
      ~octez_client_bin:bin
      ~endpoint
      ~base_dir:None
      ~password_file:None
      ~alias
      ~op:(Transfer {amount = "100"; destination = "tz1dest"})
  in
  check
    (list string)
    "transfer"
    [
      bin;
      "--endpoint";
      endpoint;
      "transfer";
      "100";
      "from";
      alias;
      "to";
      "tz1dest";
      "--burn-cap";
      "1";
    ]
    cmd

let test_build_set_delegate_params () =
  let cmd =
    BO.build_command
      ~octez_client_bin:bin
      ~endpoint
      ~base_dir:None
      ~password_file:None
      ~alias
      ~op:(Set_delegate_params {limit = 5; edge = 10})
  in
  check
    (list string)
    "set delegate params"
    [
      bin;
      "--endpoint";
      endpoint;
      "set";
      "delegate";
      "parameters";
      "for";
      alias;
      "--limit-of-staking-over-baking";
      "5";
      "--edge-of-baking-over-staking";
      "0.1";
      "--burn-cap";
      "1";
    ]
    cmd

let test_build_update_consensus_key () =
  let cmd =
    BO.build_command
      ~octez_client_bin:bin
      ~endpoint
      ~base_dir:None
      ~password_file:None
      ~alias
      ~op:
        (Update_consensus_key
           {delegate_alias = alias; key_alias = "consensus-key"})
  in
  check
    (list string)
    "update consensus key"
    [
      bin;
      "--endpoint";
      endpoint;
      "set";
      "consensus";
      "key";
      "for";
      alias;
      "to";
      "consensus-key";
      "--burn-cap";
      "1";
    ]
    cmd

let test_build_submit_proposals () =
  let cmd =
    BO.build_command
      ~octez_client_bin:bin
      ~endpoint
      ~base_dir:None
      ~password_file:None
      ~alias
      ~op:(Submit_proposals {proposals = ["PtProto1"; "PtProto2"]})
  in
  check
    (list string)
    "submit proposals"
    [
      bin;
      "--endpoint";
      endpoint;
      "submit";
      "proposals";
      "for";
      alias;
      "PtProto1";
      "PtProto2";
      "--burn-cap";
      "1";
    ]
    cmd

let test_build_submit_ballot () =
  let cmd =
    BO.build_command
      ~octez_client_bin:bin
      ~endpoint
      ~base_dir:None
      ~password_file:None
      ~alias
      ~op:(Submit_ballot {proposal = "PtProto1"; ballot = BWD.Yay})
  in
  check
    (list string)
    "submit ballot"
    [
      bin;
      "--endpoint";
      endpoint;
      "submit";
      "ballot";
      "for";
      alias;
      "PtProto1";
      "yay";
      "--burn-cap";
      "1";
    ]
    cmd

let test_build_submit_ballot_nay () =
  let cmd =
    BO.build_command
      ~octez_client_bin:bin
      ~endpoint
      ~base_dir:None
      ~password_file:None
      ~alias
      ~op:(Submit_ballot {proposal = "PtProto1"; ballot = BWD.Nay})
  in
  (* Find the ballot value (before --burn-cap suffix) *)
  let has_nay = List.mem "nay" cmd in
  check bool "nay ballot" true has_nay

let test_build_with_base_dir () =
  let cmd =
    BO.build_command
      ~octez_client_bin:bin
      ~endpoint
      ~base_dir:(Some "/home/tezos/.tezos-client")
      ~password_file:None
      ~alias
      ~op:Register
  in
  check
    (list string)
    "register with base_dir"
    [
      bin;
      "--base-dir";
      "/home/tezos/.tezos-client";
      "--endpoint";
      endpoint;
      "register";
      "key";
      alias;
      "as";
      "delegate";
      "--burn-cap";
      "1";
    ]
    cmd

let test_build_with_password_file () =
  let cmd =
    BO.build_command
      ~octez_client_bin:bin
      ~endpoint
      ~base_dir:None
      ~password_file:(Some "/home/mathias/passwd")
      ~alias
      ~op:Register
  in
  check
    (list string)
    "register with password_file"
    [
      bin;
      "--password-filename";
      "/home/mathias/passwd";
      "--endpoint";
      endpoint;
      "register";
      "key";
      alias;
      "as";
      "delegate";
      "--burn-cap";
      "1";
    ]
    cmd

let test_build_with_base_dir_and_password_file () =
  let cmd =
    BO.build_command
      ~octez_client_bin:bin
      ~endpoint
      ~base_dir:(Some "/home/tezos/.tezos-client")
      ~password_file:(Some "/home/mathias/passwd")
      ~alias
      ~op:(Stake {amount = "500"})
  in
  check
    (list string)
    "stake with base_dir and password_file"
    [
      bin;
      "--base-dir";
      "/home/tezos/.tezos-client";
      "--password-filename";
      "/home/mathias/passwd";
      "--endpoint";
      endpoint;
      "stake";
      "500";
      "for";
      alias;
      "--burn-cap";
      "1";
    ]
    cmd

(* ── describe_operation ──────────────────────────────────── *)

let test_describe_register () =
  check
    string
    "register"
    "Register as Delegate"
    (BO.describe_operation Register)

let test_describe_finalize () =
  check
    string
    "finalize"
    "Finalize Unstake"
    (BO.describe_operation Finalize_unstake)

let test_describe_unstake_everything () =
  check
    string
    "unstake everything"
    "Unstake everything"
    (BO.describe_operation (Unstake {amount = "everything"}))

let test_describe_submit_proposals_plural () =
  let desc =
    BO.describe_operation (Submit_proposals {proposals = ["PtA"; "PtB"]})
  in
  check bool "contains 'Proposals'" true (String.length desc > 0) ;
  check
    bool
    "plural s"
    true
    (try
       let _ = Str.search_forward (Str.regexp_string "Proposals") desc 0 in
       true
     with Not_found -> false)

let test_describe_submit_ballot () =
  let desc =
    BO.describe_operation
      (Submit_ballot {proposal = "PtProto1"; ballot = BWD.Pass})
  in
  check
    bool
    "contains 'pass'"
    true
    (try
       let _ = Str.search_forward (Str.regexp_string "pass") desc 0 in
       true
     with Not_found -> false)

(* ── Test suite ──────────────────────────────────────────── *)

let () =
  Alcotest.run
    "Baker_ops"
    [
      ( "build_command",
        [
          test_case "register" `Quick test_build_register;
          test_case "stake" `Quick test_build_stake;
          test_case "unstake" `Quick test_build_unstake;
          test_case "finalize_unstake" `Quick test_build_finalize_unstake;
          test_case "transfer" `Quick test_build_transfer;
          test_case "set_delegate_params" `Quick test_build_set_delegate_params;
          test_case
            "update_consensus_key"
            `Quick
            test_build_update_consensus_key;
          test_case "submit_proposals" `Quick test_build_submit_proposals;
          test_case "submit_ballot yay" `Quick test_build_submit_ballot;
          test_case "submit_ballot nay" `Quick test_build_submit_ballot_nay;
          test_case "with base_dir" `Quick test_build_with_base_dir;
          test_case "with password_file" `Quick test_build_with_password_file;
          test_case
            "with base_dir and password_file"
            `Quick
            test_build_with_base_dir_and_password_file;
        ] );
      ( "describe_operation",
        [
          test_case "register" `Quick test_describe_register;
          test_case "finalize" `Quick test_describe_finalize;
          test_case "unstake everything" `Quick test_describe_unstake_everything;
          test_case
            "proposals plural"
            `Quick
            test_describe_submit_proposals_plural;
          test_case "ballot" `Quick test_describe_submit_ballot;
        ] );
    ]
