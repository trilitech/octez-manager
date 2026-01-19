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

let install_baker ?(quiet = false) (request : baker_request) =
  let* node_mode : Installer_types.resolved_baker_node_mode =
    match request.node_mode with
    | Remote_endpoint endpoint -> Ok (Remote endpoint)
    | Local_instance inst ->
        let* svc = lookup_node_service inst in
        Ok (Local svc)
  in
  let node_data_dir =
    match node_mode with Remote _ -> "" | Local svc -> svc.Service.data_dir
  in
  let history_mode =
    match node_mode with
    | Local svc -> svc.Service.history_mode
    | Remote _ -> History_mode.default
  in
  let node_endpoint =
    match node_mode with
    | Remote endpoint -> endpoint_of_rpc endpoint
    | Local svc -> endpoint_of_rpc svc.Service.rpc_addr
  in
  let* network =
    match node_mode with
    | Local svc -> Ok svc.Service.network
    | Remote _ -> Teztnets.resolve_octez_node_chain ~endpoint:node_endpoint
  in
  let base_dir =
    match request.base_dir with
    | Some dir when String.trim dir <> "" -> dir
    | _ -> Common.default_role_dir "baker" request.instance
  in
  let dal_config =
    match request.dal_config with
    | Dal_endpoint ep when String.trim ep <> "" ->
        Dal_endpoint (endpoint_of_rpc ep)
    | Dal_disabled -> Dal_disabled
    | _ -> Dal_auto
  in
  let* liquidity_baking_vote =
    match request.liquidity_baking_vote with
    | Some vote when String.trim vote <> "" ->
        let normalized = String.lowercase_ascii (String.trim vote) in
        if normalized = "on" || normalized = "off" || normalized = "pass" then
          Ok normalized
        else
          R.error_msg
            (Printf.sprintf
               "Invalid liquidity baking vote '%s'. Must be 'on', 'off', or \
                'pass'."
               vote)
    | _ ->
        R.error_msg
          "Liquidity baking vote is required. Use --liquidity-baking-vote with \
           'on', 'off', or 'pass'."
  in
  let node_mode_env =
    match node_mode with Local _ -> "local" | Remote _ -> "remote"
  in
  (* Delegates are positional arguments, not --delegate flags *)
  let delegate_args = String.concat " " request.delegates |> String.trim in
  (* Split extra args into global (before subcommand) and command (after) *)
  let global_args, command_args =
    split_baker_extra_args ~app_bin_dir:request.app_bin_dir request.extra_args
  in
  let global_args_str = String.concat " " global_args |> String.trim in
  let command_args_str = String.concat " " command_args |> String.trim in
  let depends_on =
    match node_mode with
    | Local svc -> Some svc.Service.instance
    | Remote _ -> None
  in
  let* service =
    install_daemon
      ~quiet
      {
        role = "baker";
        instance = request.instance;
        network;
        history_mode;
        data_dir = node_data_dir;
        rpc_addr = node_endpoint;
        net_addr = "";
        service_user = request.service_user;
        app_bin_dir = request.app_bin_dir;
        logging_mode = request.logging_mode;
        service_args = [];
        extra_env =
          [
            ("OCTEZ_BAKER_BASE_DIR", base_dir);
            ("OCTEZ_NODE_ENDPOINT", node_endpoint);
            ( "OCTEZ_NODE_INSTANCE",
              match node_mode with
              | Local svc -> svc.Service.instance
              | Remote _ -> "" );
            ("OCTEZ_BAKER_NODE_MODE", node_mode_env);
            ( "OCTEZ_DAL_CONFIG",
              match dal_config with
              | Dal_disabled -> "disabled"
              | Dal_endpoint ep -> ep
              | Dal_auto -> "" );
            ("OCTEZ_DAL_INSTANCE", Option.value ~default:"" request.dal_node);
            ("OCTEZ_BAKER_DELEGATES_ARGS", delegate_args);
            ("OCTEZ_BAKER_DELEGATES_CSV", String.concat "," request.delegates);
            ("OCTEZ_BAKER_LB_VOTE", liquidity_baking_vote);
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
    | Remote _ -> Ok ()
  in
  (* Register as dependent on DAL node if using local DAL (avoid duplicates) *)
  let* () =
    match request.dal_node with
    | Some dal_inst -> (
        match Service_registry.find ~instance:dal_inst with
        | Ok (Some dal_svc) ->
            if List.mem request.instance dal_svc.dependents then Ok ()
            else
              let updated_dal =
                {
                  dal_svc with
                  dependents = request.instance :: dal_svc.dependents;
                }
              in
              Service_registry.write updated_dal
        | _ -> Ok ())
    | None -> Ok ()
  in
  Ok service
