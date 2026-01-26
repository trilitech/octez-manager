(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Helper functions for creating mock service data in tests.
    
    These helpers create realistic Service.t and Service_state.t instances
    for testing rendering logic without requiring actual systemd services. *)

open Octez_manager_lib
open Octez_manager_ui

(** Create a mock Service.t with sensible defaults *)
let mock_service ?(instance = "test-node") ?(role = "node")
    ?(network = "mainnet") ?(history_mode = History_mode.Rolling)
    ?(data_dir = "/var/lib/octez/test-node") ?(rpc_addr = "127.0.0.1:8732")
    ?(net_addr = "0.0.0.0:9732") ?(service_user = "octez")
    ?(app_bin_dir = "/usr/bin") ?(logging_mode = Logging_mode.Journald) () =
  Service.make
    ~instance
    ~role
    ~network
    ~history_mode
    ~data_dir
    ~rpc_addr
    ~net_addr
    ~service_user
    ~app_bin_dir
    ~logging_mode
    ()

(** Create a mock Service_state.t with a given status *)
let mock_service_state ?(instance = "test-node") ?(role = "node")
    ?(network = "mainnet") ?(enabled = Some true) ?(active = Some true)
    ?(status = Data.Service_state.Running) ?(status_text = None) () =
  let service = mock_service ~instance ~role ~network () in
  {Data.Service_state.service; enabled; active; status; status_text}

(** Create a running service state *)
let running_service ?(instance = "test-node") ?(role = "node")
    ?(network = "mainnet") () =
  mock_service_state
    ~instance
    ~role
    ~network
    ~status:Data.Service_state.Running
    ~active:(Some true)
    ()

(** Create a stopped service state *)
let stopped_service ?(instance = "test-node") ?(role = "node")
    ?(network = "mainnet") () =
  mock_service_state
    ~instance
    ~role
    ~network
    ~status:Data.Service_state.Stopped
    ~active:(Some false)
    ()

(** Create a failed service state *)
let failed_service ?(instance = "test-node") ?(role = "node")
    ?(network = "mainnet") ?(error = "connection refused") () =
  mock_service_state
    ~instance
    ~role
    ~network
    ~status:(Data.Service_state.Unknown error)
    ~active:(Some false)
    ()

(** Create a baker service *)
let baker_service ?(instance = "test-baker") ?(network = "mainnet")
    ?(status = Data.Service_state.Running) () =
  mock_service_state ~instance ~role:"baker" ~network ~status ()

(** Create an accuser service *)
let accuser_service ?(instance = "test-accuser") ?(network = "mainnet")
    ?(status = Data.Service_state.Running) () =
  mock_service_state ~instance ~role:"accuser" ~network ~status ()

(** Create a DAL node service *)
let dal_service ?(instance = "test-dal") ?(network = "mainnet")
    ?(status = Data.Service_state.Running) () =
  mock_service_state ~instance ~role:"dal-node" ~network ~status ()

(** Create N mock services with sequential naming *)
let create_services n ~prefix ~role ~status =
  List.init n (fun i ->
      let instance = Printf.sprintf "%s-%d" prefix i in
      mock_service_state ~instance ~role ~status ())

(** Create a mix of services with different states *)
let mixed_services ?(running = 3) ?(stopped = 2) ?(failed = 1) () =
  let running_svcs =
    create_services
      running
      ~prefix:"running-node"
      ~role:"node"
      ~status:Data.Service_state.Running
  in
  let stopped_svcs =
    create_services
      stopped
      ~prefix:"stopped-node"
      ~role:"node"
      ~status:Data.Service_state.Stopped
  in
  let failed_svcs =
    List.init failed (fun i ->
        failed_service
          ~instance:(Printf.sprintf "failed-node-%d" i)
          ~error:"startup error"
          ())
  in
  running_svcs @ stopped_svcs @ failed_svcs

(** Create services of different types *)
let multi_role_services () =
  [
    running_service ~instance:"node-1" ~role:"node" ~network:"mainnet" ();
    running_service ~instance:"node-2" ~role:"node" ~network:"ghostnet" ();
    baker_service ~instance:"baker-1" ~network:"mainnet" ();
    accuser_service ~instance:"accuser-1" ~network:"mainnet" ();
    dal_service ~instance:"dal-1" ~network:"mainnet" ();
  ]
