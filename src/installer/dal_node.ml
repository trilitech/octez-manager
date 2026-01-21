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

let install_daemon ?(quiet = false) (request : daemon_request) =
  let* () =
    validate_instance_name
      ~allow_existing:request.preserve_data
      ~instance:request.instance
      ()
  in
  let logging_mode =
    prepare_logging
      ~instance:request.instance
      ~role:request.role
      ~logging_mode:request.logging_mode
  in
  let* () =
    System_user.ensure_service_account ~quiet ~name:request.service_user ()
  in
  let* () =
    if Common.is_root () then
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
    if Common.is_root () then (request.service_user, request.service_user)
    else Common.current_user_group_names ()
  in
  let directories = request.data_dir :: request.extra_paths in
  let* () = ensure_directories ~owner ~group directories in
  let* () = ensure_logging_base_directory ~owner ~group logging_mode in
  let* () = ensure_runtime_log_directory ~owner ~group logging_mode in
  let extra_env =
    let service_args = String.concat " " request.service_args |> String.trim in
    let args_entry =
      if service_args = "" then [] else [("OCTEZ_SERVICE_ARGS", service_args)]
    in
    [
      ("OCTEZ_DAL_DATA_DIR", request.data_dir);
      ("OCTEZ_DAL_RPC_ADDR", request.rpc_addr);
      ("OCTEZ_DAL_NET_ADDR", request.net_addr);
      ("OCTEZ_NETWORK", request.network);
    ]
    @ args_entry @ request.extra_env
  in
  let* () =
    Node_env.write_pairs ~with_comments:true ~inst:request.instance extra_env
  in
  let* () =
    Systemd.install_unit
      ~quiet
      ~role:request.role
      ~app_bin_dir:request.app_bin_dir
      ~user:request.service_user
      ()
  in
  (* Resolve depends_on to (parent_role, parent_instance) tuple for systemd *)
  let depends_on_for_systemd =
    match request.depends_on with
    | None -> None
    | Some parent_instance -> (
        match Service_registry.find ~instance:parent_instance with
        | Ok (Some parent_svc) -> Some (parent_svc.Service.role, parent_instance)
        | _ -> None)
  in
  let* () =
    Systemd.write_dropin
      ~role:request.role
      ~inst:request.instance
      ~data_dir:request.data_dir
      ~logging_mode
      ~extra_paths:request.extra_paths
      ?depends_on:depends_on_for_systemd
      ()
  in
  let* () =
    if request.preserve_data then Ok ()
    else reown_runtime_paths ~owner ~group ~paths:directories ~logging_mode
  in
  (* In edit mode, preserve existing dependents list *)
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
      ~role:request.role
      ~network:request.network
      ~history_mode:request.history_mode
      ~data_dir:request.data_dir
      ~rpc_addr:request.rpc_addr
      ~net_addr:request.net_addr
      ~service_user:request.service_user
      ~app_bin_dir:request.app_bin_dir
      ~logging_mode
      ~extra_args:request.service_args
      ~depends_on:request.depends_on
      ~dependents:existing_dependents
      ()
  in
  let* () = Service_registry.write service in
  (* In edit mode, update dependent endpoints if RPC address changed (for DAL nodes) *)
  let* () =
    if
      request.preserve_data
      && (request.role = "dal-node" || request.role = "dal")
    then
      update_dependent_endpoints
        ~instance:request.instance
        ~role:request.role
        ~new_rpc_addr:request.rpc_addr
        ()
    else Ok ()
  in
  (* Register as dependent on parent if depends_on is set *)
  let* () =
    match request.depends_on with
    | Some parent_instance -> (
        match Service_registry.find ~instance:parent_instance with
        | Ok (Some parent_svc) ->
            (* Only add if not already in dependents list *)
            if List.mem request.instance parent_svc.dependents then Ok ()
            else
              let updated_parent =
                {
                  parent_svc with
                  dependents = request.instance :: parent_svc.dependents;
                }
              in
              Service_registry.write updated_parent
        | Ok None -> Ok () (* Parent not found, skip *)
        | Error _ -> Ok () (* Error finding parent, skip *))
    | None -> Ok ()
  in
  let* services = Service_registry.list () in
  let* () = Systemd.sync_logrotate (logrotate_specs_of services) in
  let* () =
    if request.auto_enable then
      Systemd.enable
        ~quiet
        ~role:request.role
        ~instance:request.instance
        ~start_now:true
        ()
    else Ok ()
  in
  Ok service
