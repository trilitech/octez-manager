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
    | Local svc -> Rpc_addr.to_endpoint svc.Service.rpc_addr
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
    | _ -> Paths.default_role_dir "baker" request.instance
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
  (* Resolve extra nodes to endpoints and collect systemd dependencies *)
  let extra_node_endpoints, extra_node_dependencies =
    List.fold_left
      (fun (endpoints, deps) spec ->
        match spec with
        | Installer_types.Extra_instance inst -> (
            (* Instance was already validated in CLI, but double-check *)
            match Service_registry.find ~instance:inst with
            | Ok (Some svc) when String.equal svc.Service.role "node" ->
                let endpoint = Rpc_addr.to_endpoint svc.Service.rpc_addr in
                (endpoint :: endpoints, ("node", inst) :: deps)
            | _ ->
                (* This shouldn't happen due to CLI validation, but handle it *)
                (endpoints, deps))
        | Installer_types.Extra_endpoint ep -> (ep :: endpoints, deps))
      ([], [])
      request.extra_nodes
    |> fun (eps, deps) -> (List.rev eps, List.rev deps)
  in
  (* When extra nodes are present, baker MUST run in remote mode *)
  let node_mode_env =
    match (node_mode, extra_node_endpoints) with
    | _, _ :: _ ->
        "remote" (* Force remote mode when extra nodes are configured *)
    | Local _, [] | Local_unmanaged _, [] -> "local"
    | Remote _, [] -> "remote"
  in
  (* Validate and resolve remote signer configuration *)
  let* signer_uri_opt =
    Signer_validation.validate_and_resolve request.signer_mode
  in
  let signatory_instance =
    match request.signer_mode with
    | Signer_types.Remote_signer {instance = Some name; _} -> Some name
    | _ -> None
  in
  (* Delegates are positional arguments, not --delegate flags *)
  let delegate_args = String.concat " " request.delegates |> String.trim in
  (* Split extra args into global (before subcommand) and command (after) *)
  let global_args, command_args =
    split_baker_extra_args ~app_bin_dir:request.app_bin_dir request.extra_args
  in
  (* Add -R flag to global args if using remote signer *)
  let global_args_with_signer =
    match signer_uri_opt with
    | Some uri -> global_args @ ["-R"; uri]
    | None -> global_args
  in
  let global_args_str =
    String.concat " " global_args_with_signer |> String.trim
  in
  let command_args_str = String.concat " " command_args |> String.trim in
  (* Resolve DAL endpoint for Dal_auto mode *)
  let dal_config_env =
    match dal_config with
    | Dal_disabled -> "disabled"
    | Dal_endpoint ep -> ep
    | Dal_auto -> (
        (* Look up the DAL instance to get its RPC endpoint *)
        match request.dal_node with
        | Some dal_instance_name -> (
            match Service_registry.find ~instance:dal_instance_name with
            | Ok (Some dal_svc) ->
                (* Convert RPC addr to http://host:port format *)
                let rpc_str = Rpc_addr.to_string dal_svc.Service.rpc_addr in
                if
                  String.starts_with ~prefix:"http://" rpc_str
                  || String.starts_with ~prefix:"https://" rpc_str
                then rpc_str
                else "http://" ^ rpc_str
            | _ ->
                (* DAL instance not found - this shouldn't happen in cascade import *)
                "")
        | None -> "")
  in
  let depends_on =
    match node_mode with
    | Local svc -> Some svc.Service.instance
    | Local_unmanaged _ | Remote _ -> None
  in
  (* Collect all dependencies for systemd dropin (node + signatory if applicable) *)
  let all_dependencies_for_systemd =
    let node_deps =
      match node_mode with
      | Local svc -> [(svc.Service.role, svc.Service.instance)]
      | Local_unmanaged _ | Remote _ -> []
    in
    let signatory_deps =
      match signatory_instance with
      | Some sig_inst -> (
          match Service_registry.find ~instance:sig_inst with
          | Ok (Some sig_svc) -> [(sig_svc.Service.role, sig_inst)]
          | _ -> [])
      | None -> []
    in
    let extra_node_deps = extra_node_dependencies in
    match node_deps @ signatory_deps @ extra_node_deps with
    | [] -> None
    | deps -> Some deps
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
        rpc_addr = Rpc_addr.of_string node_endpoint;
        net_addr = "";
        service_user = request.service_user;
        app_bin_dir = request.app_bin_dir;
        bin_source = request.bin_source;
        logging_mode = request.logging_mode;
        service_args = request.extra_args;
        extra_env =
          [
            ("OCTEZ_BAKER_BASE_DIR", base_dir);
            ("OCTEZ_NODE_ENDPOINT", node_endpoint);
            ( "OCTEZ_EXTRA_NODE_ENDPOINTS",
              String.concat "," extra_node_endpoints );
            ( "OCTEZ_NODE_INSTANCE",
              match node_mode with
              | Local svc -> svc.Service.instance
              | Local_unmanaged _ | Remote _ -> "" );
            ("OCTEZ_BAKER_NODE_MODE", node_mode_env);
            ("OCTEZ_DAL_CONFIG", dal_config_env);
            ("OCTEZ_DAL_INSTANCE", Option.value ~default:"" request.dal_node);
            ("OCTEZ_BAKER_DELEGATES_ARGS", delegate_args);
            ("OCTEZ_BAKER_DELEGATES_CSV", String.concat "," request.delegates);
            ("OCTEZ_BAKER_LB_VOTE", liquidity_baking_vote);
            ("OCTEZ_BAKER_GLOBAL_ARGS", global_args_str);
            ("OCTEZ_BAKER_COMMAND_ARGS", command_args_str);
            ("OCTEZ_REMOTE_SIGNER_URI", Option.value ~default:"" signer_uri_opt);
            ( "OCTEZ_SIGNATORY_INSTANCE",
              Option.value ~default:"" signatory_instance );
          ]
          @ request.extra_env;
        extra_paths = [base_dir];
        auto_enable = request.auto_enable;
        depends_on;
        preserve_data = request.preserve_data;
      }
  in
  (* Update service with signer configuration *)
  let service_with_signer =
    {
      service with
      signer_mode = Some request.signer_mode;
      signer_uri = signer_uri_opt;
    }
  in
  let* () = Service_registry.write service_with_signer in
  (* Rewrite dropin with all dependencies (node + signatory if applicable) *)
  let* () =
    match all_dependencies_for_systemd with
    | Some deps when List.length deps > 1 || signatory_instance <> None ->
        (* Only rewrite if we have signatory dependency (install_daemon already wrote node-only) *)
        Systemd.write_dropin
          ~quiet
          ~role:"baker"
          ~inst:request.instance
          ~data_dir:service.Service.data_dir
          ~logging_mode:service.Service.logging_mode
          ~extra_paths:[base_dir]
          ~app_bin_dir:request.app_bin_dir
          ~depends_on:deps
          ()
    | _ ->
        (* No signatory dependency, install_daemon already wrote correct dropin *)
        Ok ()
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
  (* Register as dependent on Signatory instance if using managed signer (avoid duplicates) *)
  let* () =
    match signatory_instance with
    | Some sig_inst -> (
        match Service_registry.find ~instance:sig_inst with
        | Ok (Some sig_svc) ->
            if List.mem request.instance sig_svc.dependents then Ok ()
            else
              let updated_sig =
                {
                  sig_svc with
                  dependents = request.instance :: sig_svc.dependents;
                }
              in
              Service_registry.write updated_sig
        | _ -> Ok ())
    | None -> Ok ()
  in
  (* Register as dependent on extra node instances (avoid duplicates) *)
  let* () =
    List.fold_left
      (fun acc (_role, inst) ->
        let* () = acc in
        match Service_registry.find ~instance:inst with
        | Ok (Some extra_node_svc) ->
            if List.mem request.instance extra_node_svc.dependents then Ok ()
            else
              let updated_extra_node =
                {
                  extra_node_svc with
                  dependents = request.instance :: extra_node_svc.dependents;
                }
              in
              Service_registry.write updated_extra_node
        | _ -> Ok ())
      (Ok ())
      extra_node_dependencies
  in
  Ok service_with_signer
