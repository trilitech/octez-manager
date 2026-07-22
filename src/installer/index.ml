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

let install ?(quiet = false) (request : index_request) =
  let* () =
    validate_instance_name
      ~allow_existing:request.preserve_data
      ~instance:request.instance
      ()
  in
  let logging_mode =
    prepare_logging
      ~instance:request.instance
      ~role:"index"
      ~logging_mode:request.logging_mode
  in
  let* () =
    System_user.ensure_service_account ~quiet ~name:request.service_user ()
  in
  (* Fail before any state is written: a failure after the service registry
     entry is created (e.g. inside Systemd.install_unit) would leave a stale
     record occupying the instance name and RPC port. *)
  let* () =
    Systemd.validate_bin_dir
      ~user:request.service_user
      ~app_bin_dir:request.app_bin_dir
      ~role:"index"
  in
  let* () =
    if Paths.is_root () then
      System_user.ensure_system_directories
        ~user:request.service_user
        ~group:request.service_user
        ()
    else Ok ()
  in
  let* () =
    ensure_logging_destination ~service_user:request.service_user logging_mode
  in
  let* () = System_user.validate_user_for_service ~user:request.service_user in
  let owner, group =
    if Paths.is_root () then (request.service_user, request.service_user)
    else Paths.current_user_group_names ()
  in
  let directories = [request.base_dir] in
  let* () = ensure_directories ~owner ~group directories in
  let* () = ensure_logging_base_directory ~owner ~group logging_mode in
  let* () = ensure_runtime_log_directory ~owner ~group logging_mode in
  (* Build service_args: --watched-address flags and --db-name *)
  let watched_args =
    List.concat_map
      (fun addr -> ["--watched-address"; addr])
      request.watched_addresses
  in
  let db_args =
    match request.db_name with
    | Some name when String.trim name <> "" ->
        let normalized =
          if String.contains name ':' then name else "sqlite3:" ^ name
        in
        ["--db-name"; normalized]
    | _ -> []
  in
  let all_service_args = watched_args @ db_args @ request.extra_args in
  (* Resolve parent service for network and dependencies *)
  let parent_svc_opt =
    match request.depends_on with
    | None -> None
    | Some parent_instance -> (
        match Service_registry.find ~instance:parent_instance with
        | Ok (Some parent_svc) -> Some parent_svc
        | _ -> None)
  in
  let network =
    match parent_svc_opt with
    | Some parent_svc -> parent_svc.Service.network
    | None -> ""
  in
  let existing_dependents =
    if request.preserve_data then
      match Service_registry.find ~instance:request.instance with
      | Ok (Some existing) -> existing.Service.dependents
      | _ -> []
    else []
  in
  let service =
    Service.make
      ~instance:request.instance
      ~role:"index"
      ~network
      ~history_mode:History_mode.default
      ~data_dir:request.base_dir
      ~rpc_addr:request.rpc_addr
      ~net_addr:""
      ~service_user:request.service_user
      ~app_bin_dir:request.app_bin_dir
      ?bin_source:request.bin_source
      ~logging_mode
      ~extra_args:all_service_args
      ~depends_on:request.depends_on
      ~dependents:existing_dependents
      ()
  in
  let* () = Service_registry.write service in
  let service_args_str = String.concat " " all_service_args |> String.trim in
  (* Convert node_endpoint host:port to full URI *)
  let node_endpoint = endpoint_of_rpc request.node_endpoint in
  let extra_env =
    let rpc_addr_str = Rpc_addr.to_string request.rpc_addr in
    let rpc_addr_entry =
      if rpc_addr_str = "" then [] else [("OCTEZ_INDEX_RPC_ADDR", rpc_addr_str)]
    in
    let args_entry =
      if service_args_str = "" then []
      else [("OCTEZ_SERVICE_ARGS", service_args_str)]
    in
    [
      ("OCTEZ_INDEXER_DIR", request.base_dir);
      ("OCTEZ_NODE_ENDPOINT", node_endpoint);
    ]
    @ rpc_addr_entry @ args_entry @ request.extra_env
  in
  let* () =
    Node_env.write_pairs ~with_comments:true ~inst:request.instance extra_env
  in
  let* () =
    Systemd.install_unit
      ~quiet
      ~role:"index"
      ~app_bin_dir:request.app_bin_dir
      ~user:request.service_user
      ()
  in
  let depends_on_for_systemd =
    match parent_svc_opt with
    | Some parent_svc ->
        Some [(parent_svc.Service.role, parent_svc.Service.instance)]
    | None -> None
  in
  let* () =
    Systemd.write_dropin
      ~role:"index"
      ~inst:request.instance
      ~data_dir:request.base_dir
      ~logging_mode
      ~extra_paths:[]
      ~app_bin_dir:request.app_bin_dir
      ?depends_on:depends_on_for_systemd
      ()
  in
  let* () =
    if request.preserve_data then Ok ()
    else reown_runtime_paths ~owner ~group ~paths:directories ~logging_mode
  in
  (* Register as dependent on parent if depends_on is set *)
  let* () =
    match request.depends_on with
    | Some parent_instance -> (
        match Service_registry.find ~instance:parent_instance with
        | Ok (Some parent_svc) ->
            if List.mem request.instance parent_svc.dependents then Ok ()
            else
              let updated_parent =
                {
                  parent_svc with
                  dependents = request.instance :: parent_svc.dependents;
                }
              in
              Service_registry.write updated_parent
        | Ok None -> Ok ()
        | Error _ -> Ok ())
    | None -> Ok ()
  in
  let* () =
    if request.auto_enable then
      Systemd.enable
        ~quiet
        ~role:"index"
        ~instance:request.instance
        ~start_now:true
        ()
    else Ok ()
  in
  Ok service
