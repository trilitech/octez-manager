(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Rresult
open Installer_types
include Helpers
include Config
include Dal_node

let install_accuser ?(quiet = false) (request : accuser_request) =
  let* node_mode : Installer_types.resolved_baker_node_mode =
    match request.node_mode with
    | Remote_endpoint endpoint -> Ok (Remote endpoint)
    | Local_datadir (endpoint, data_dir) ->
        Ok (Local_unmanaged (endpoint, data_dir))
    | Local_instance inst ->
        let* svc = lookup_node_service inst in
        Ok (Local svc)
  in
  let node_data_dir =
    match node_mode with
    | Remote _ -> ""
    | Local_unmanaged (_, data_dir) -> data_dir
    | Local svc -> svc.Service.data_dir
  in
  let history_mode =
    match node_mode with
    | Local svc -> svc.Service.history_mode
    | Local_unmanaged _ | Remote _ -> History_mode.default
  in
  let node_endpoint =
    match node_mode with
    | Remote endpoint -> endpoint_of_rpc endpoint
    | Local_unmanaged (endpoint, _) -> endpoint_of_rpc endpoint
    | Local svc -> endpoint_of_rpc svc.Service.rpc_addr
  in
  let* network =
    match node_mode with
    | Local svc -> Ok svc.Service.network
    | Local_unmanaged _ | Remote _ ->
        Teztnets.resolve_octez_node_chain ~endpoint:node_endpoint
  in
  let base_dir =
    match request.base_dir with
    | Some dir when String.trim dir <> "" -> dir
    | _ -> Common.default_role_dir "accuser" request.instance
  in
  (* Split extra args into global (before subcommand) and command (after) *)
  let global_args, command_args =
    split_baker_extra_args ~app_bin_dir:request.app_bin_dir request.extra_args
  in
  let global_args_str = String.concat " " global_args |> String.trim in
  let command_args_str = String.concat " " command_args |> String.trim in
  let depends_on =
    match node_mode with
    | Local svc -> Some svc.Service.instance
    | Local_unmanaged _ | Remote _ -> None
  in
  let* service =
    install_daemon
      ~quiet
      {
        role = "accuser";
        instance = request.instance;
        network;
        history_mode;
        data_dir = node_data_dir;
        rpc_addr = node_endpoint;
        net_addr = "";
        service_user = request.service_user;
        app_bin_dir = request.app_bin_dir;
        bin_source = request.bin_source;
        logging_mode = request.logging_mode;
        service_args = [];
        (* Accusers use OCTEZ_BAKER_COMMAND_ARGS, not OCTEZ_SERVICE_ARGS *)
        extra_env =
          [
            ("OCTEZ_CLIENT_BASE_DIR", base_dir);
            ("OCTEZ_NODE_ENDPOINT", node_endpoint);
            ( "OCTEZ_NODE_INSTANCE",
              match node_mode with
              | Local svc -> svc.Service.instance
              | Local_unmanaged _ | Remote _ -> "" );
            ("OCTEZ_BAKER_GLOBAL_ARGS", global_args_str);
            ("OCTEZ_BAKER_COMMAND_ARGS", command_args_str);
          ];
        extra_paths = [base_dir];
        auto_enable = request.auto_enable;
        depends_on;
        preserve_data = request.preserve_data;
      }
  in
  (* Register as dependent on parent node (avoid duplicates) *)
  let* () =
    match node_mode with
    | Local parent_svc ->
        if List.mem request.instance parent_svc.dependents then Ok ()
        else
          let updated_parent =
            {
              parent_svc with
              dependents = request.instance :: parent_svc.dependents;
            }
          in
          Service_registry.write updated_parent
    | Local_unmanaged _ | Remote _ -> Ok ()
  in
  Ok service
