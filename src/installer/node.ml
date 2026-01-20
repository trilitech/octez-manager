(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Rresult
open Installer_types
include Helpers
include Snapshot
include Config

let install_node ?(quiet = false) ?on_log (request : node_request) =
  let log msg = match on_log with Some f -> f msg | None -> () in
  log "Validating instance name...\n" ;
  let* () =
    validate_instance_name
      ~allow_existing:request.preserve_data
      ~instance:request.instance
      ()
  in
  log "Resolving network...\n" ;
  let* resolved_network =
    Teztnets.resolve_network_for_octez_node request.network
  in
  let data_dir = normalize_data_dir request.instance request.data_dir in
  let logging_mode =
    prepare_logging
      ~instance:request.instance
      ~role:"node"
      ~logging_mode:request.logging_mode
  in
  let data_dir_nonempty =
    let trimmed = String.trim data_dir in
    if trimmed = "" || not (Sys.file_exists trimmed) then false
    else
      try
        let st = Unix.stat trimmed in
        st.Unix.st_kind = Unix.S_DIR
        &&
        let entries = Sys.readdir trimmed in
        Array.exists (fun e -> e <> "." && e <> "..") entries
      with Unix.Unix_error _ | Sys_error _ -> false
  in
  log "Computing snapshot plan...\n" ;
  let* snapshot_plan = snapshot_plan_of_request request in
  let snapshot_meta =
    snapshot_metadata_of_plan ~no_check:request.snapshot_no_check snapshot_plan
  in
  log "Ensuring service account...\n" ;
  let* () =
    System_user.ensure_service_account ~quiet ~name:request.service_user ()
  in
  log "Ensuring system directories...\n" ;
  let* () =
    if Common.is_root () then
      System_user.ensure_system_directories
        ~user:request.service_user
        ~group:request.service_user
        ()
    else Ok ()
  in
  log "Ensuring logging destination...\n" ;
  let* () =
    ensure_logging_destination ~service_user:request.service_user logging_mode
  in
  let run_args =
    build_run_args
      ~network:resolved_network
      ~history_mode:request.history_mode
      ~rpc_addr:request.rpc_addr
      ~net_addr:request.net_addr
      ~extra_args:request.extra_args
      ~logging_mode
  in
  log "Validating user for service...\n" ;
  let* () = System_user.validate_user_for_service ~user:request.service_user in
  let owner, group =
    if Common.is_root () then (request.service_user, request.service_user)
    else Common.current_user_group_names ()
  in
  log (Printf.sprintf "Owner: %s, Group: %s\n" owner group) ;
  let* () =
    if data_dir_nonempty && not request.preserve_data then (
      log "Removing existing data directory...\n" ;
      Common.remove_tree data_dir)
    else Ok ()
  in
  log "Ensuring directories...\n" ;
  let* () = ensure_directories ~owner ~group [data_dir] in
  log "Ensuring logging base directory...\n" ;
  let* () = ensure_logging_base_directory ~owner ~group logging_mode in
  log "Ensuring runtime log directory...\n" ;
  let* () = ensure_runtime_log_directory ~owner ~group logging_mode in
  log "Ensuring node config...\n" ;
  let* () =
    ensure_node_config
      ~quiet
      ~app_bin_dir:request.app_bin_dir
      ~data_dir
      ~network:resolved_network
      ~history_mode:request.history_mode
      ()
  in
  log "Performing bootstrap...\n" ;
  let* () =
    if request.preserve_data then (
      log "Skipping bootstrap (preserve_data=true)\n" ;
      Ok ())
    else
      perform_bootstrap
        ~quiet
        ?on_log
        ?tmp_dir:request.tmp_dir
        ~plan:snapshot_plan
        ~request
        ~data_dir
        ()
  in
  log "Reowning runtime paths...\n" ;
  let* () =
    if request.preserve_data then (
      log "Skipping reown (preserve_data=true)\n" ;
      Ok ())
    else reown_runtime_paths ~owner ~group ~paths:[data_dir] ~logging_mode
  in
  log "Creating service record...\n" ;
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
      ~role:"node"
      ~network:request.network
      ~history_mode:request.history_mode
      ~data_dir
      ~rpc_addr:request.rpc_addr
      ~net_addr:request.net_addr
      ~service_user:request.service_user
      ~app_bin_dir:request.app_bin_dir
      ~logging_mode
      ~snapshot_auto:snapshot_meta.auto
      ~snapshot_uri:snapshot_meta.uri
      ~snapshot_network_slug:snapshot_meta.network_slug
      ~snapshot_no_check:snapshot_meta.no_check
      ~extra_args:request.extra_args
      ~dependents:existing_dependents
      ()
  in
  log "Installing systemd unit...\n" ;
  let* () =
    Systemd.install_unit
      ~quiet
      ~role:"node"
      ~app_bin_dir:request.app_bin_dir
      ~user:request.service_user
      ()
  in
  log "Building extra env...\n" ;
  let extra_env =
    [
      ("OCTEZ_NETWORK", request.network);
      ("OCTEZ_HISTORY_MODE", History_mode.to_string request.history_mode);
      ("OCTEZ_SNAPSHOT_AUTO", if snapshot_meta.auto then "1" else "0");
      ("OCTEZ_SNAPSHOT_URI", Option.value ~default:"" snapshot_meta.uri);
      ( "OCTEZ_SNAPSHOT_NETWORK_SLUG",
        Option.value ~default:"" snapshot_meta.network_slug );
      ("OCTEZ_SNAPSHOT_KIND", Option.value ~default:"" snapshot_meta.kind_slug);
      ("OCTEZ_SNAPSHOT_NO_CHECK", if snapshot_meta.no_check then "1" else "0");
    ]
  in
  log "Writing node env...\n" ;
  let* () =
    Node_env.write ~inst:request.instance ~data_dir ~run_args ~extra_env
  in
  log "Writing systemd dropin...\n" ;
  let* () =
    Systemd.write_dropin_node
      ~quiet
      ~inst:request.instance
      ~data_dir
      ~logging_mode
      ()
  in
  log "Writing service registry...\n" ;
  let* () = Service_registry.write service in
  (* In edit mode, update dependent endpoints if RPC address changed *)
  let* () =
    if request.preserve_data then (
      log "Updating dependent endpoints...\n" ;
      update_dependent_endpoints
        ~instance:request.instance
        ~role:"node"
        ~new_rpc_addr:request.rpc_addr
        ())
    else Ok ()
  in
  log "Listing services for logrotate...\n" ;
  let* services = Service_registry.list () in
  log "Syncing logrotate...\n" ;
  let* () = Systemd.sync_logrotate (logrotate_specs_of services) in
  log "Enabling service...\n" ;
  let* () =
    if request.auto_enable then
      Systemd.enable
        ~quiet
        ~role:"node"
        ~instance:request.instance
        ~start_now:true
        ()
    else Ok ()
  in
  log "Install complete!\n" ;
  Ok service
