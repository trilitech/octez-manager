(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Rresult
open Installer_types

let ( let* ) = Result.bind

(** {1 Types} *)

type import_strategy = Takeover | Clone

type field_overrides = {
  network : string option;
  data_dir : string option;
  rpc_addr : string option;
  net_addr : string option;
  base_dir : string option;
  delegates : string list option;
}

let empty_overrides =
  {
    network = None;
    data_dir = None;
    rpc_addr = None;
    net_addr = None;
    base_dir = None;
    delegates = None;
  }

type import_options = {
  strategy : import_strategy;
  new_instance_name : string option;
  overrides : field_overrides;
  dry_run : bool;
  preserve_data : bool;
  quiet : bool;
}

type import_result = {
  original_unit : string;
  new_instance : string;
  preserved_paths : string list;
  warnings : string list;
}

(** {1 Validation} *)

let validate_importable external_svc =
  (* 1. Check it's systemd-managed, not a standalone process *)
  let* () =
    if
      String.starts_with
        ~prefix:"process-"
        external_svc.External_service.config.unit_name
    then
      Error
        (`Msg
           "Cannot import standalone processes, only systemd services. Please \
            create a systemd unit first.")
    else Ok ()
  in
  (* 2. Check not already managed by octez-manager *)
  let* all_services = Service_registry.list () in
  let suggested_name = external_svc.External_service.suggested_instance_name in
  let* () =
    if List.exists (fun s -> s.Service.instance = suggested_name) all_services
    then
      Error
        (`Msg
           (Printf.sprintf
              "Instance '%s' already exists as a managed service"
              suggested_name))
    else Ok ()
  in
  (* 3. Check network is known or can be inferred for nodes *)
  let role_field = external_svc.External_service.config.role in
  let network_field = external_svc.External_service.config.network in
  let* () =
    match
      ( role_field.External_service.value,
        External_service.is_known network_field )
    with
    | Some External_service.Node, false ->
        Error
          (`Msg
             "Network could not be detected (RPC not accessible). Please \
              specify --network")
    | _, _ -> Ok ()
  in
  Ok ()

let missing_required_fields external_svc =
  let config = external_svc.External_service.config in
  let role_value = config.External_service.role.value in
  let required =
    match role_value with
    | Some External_service.Node -> ["network"; "data_dir"]
    | Some External_service.Baker -> ["network"; "base_dir"; "node_endpoint"]
    | Some External_service.Accuser -> ["network"; "base_dir"; "node_endpoint"]
    | Some External_service.Dal_node -> ["network"; "data_dir"; "node_endpoint"]
    | Some (External_service.Unknown _) | None -> []
  in
  List.filter
    (fun field ->
      match field with
      | "network" -> not (External_service.is_known config.network)
      | "data_dir" -> not (External_service.is_known config.data_dir)
      | "base_dir" -> not (External_service.is_known config.base_dir)
      | "node_endpoint" -> not (External_service.is_known config.node_endpoint)
      | _ -> false)
    required

(** {1 Field Resolution} *)

let resolve_field ~override ~detected ~field_name =
  match override with
  | Some value -> Ok value
  | None -> (
      match detected.External_service.value with
      | Some value -> Ok value
      | None ->
          Error
            (`Msg
               (Printf.sprintf
                  "Required field '%s' is missing and no override provided"
                  field_name)))

let resolve_network ~overrides ~external_svc =
  let config = external_svc.External_service.config in
  resolve_field
    ~override:overrides.network
    ~detected:config.network
    ~field_name:"network"

let resolve_data_dir ~overrides ~external_svc =
  let config = external_svc.External_service.config in
  match config.role.value with
  | Some External_service.Node | Some External_service.Dal_node ->
      resolve_field
        ~override:overrides.data_dir
        ~detected:config.data_dir
        ~field_name:"data_dir"
  | Some External_service.Baker
  | Some External_service.Accuser
  | Some (External_service.Unknown _)
  | None ->
      Ok "" (* Not required for baker/accuser *)

let resolve_base_dir ~overrides ~external_svc =
  let config = external_svc.External_service.config in
  match config.role.value with
  | Some External_service.Baker | Some External_service.Accuser ->
      resolve_field
        ~override:overrides.base_dir
        ~detected:config.base_dir
        ~field_name:"base_dir"
  | Some External_service.Node
  | Some External_service.Dal_node
  | Some (External_service.Unknown _)
  | None ->
      Ok "" (* Not required for node/dal *)

let resolve_rpc_addr ~overrides ~external_svc =
  let config = external_svc.External_service.config in
  let default_rpc = "127.0.0.1:8732" in
  match overrides.rpc_addr with
  | Some addr -> addr
  | None -> External_service.value_or ~default:default_rpc config.rpc_addr

let resolve_net_addr ~overrides ~external_svc =
  let config = external_svc.External_service.config in
  let default_net = "0.0.0.0:9732" in
  match overrides.net_addr with
  | Some addr -> addr
  | None -> External_service.value_or ~default:default_net config.net_addr

(** {1 Service Creation from External} *)

(** Get service user from external service, with fallback to current user.
    CRITICAL: We must preserve the original service user to avoid breaking
    permissions on existing data directories. *)
let get_service_user external_svc =
  match external_svc.External_service.config.user with
  | Some user -> user
  | None ->
      (* Fallback to current user if not detected *)
      let current_user, _ = Common.current_user_group_names () in
      current_user

let create_node_from_external ~instance ~external_svc ~network ~data_dir
    ~rpc_addr ~net_addr ~bin_dir =
  let service_user = get_service_user external_svc in
  let request : node_request =
    {
      instance;
      network;
      history_mode = Rolling;
      (* Default, can't detect from external *)
      data_dir = Some data_dir;
      rpc_addr;
      net_addr;
      service_user;
      app_bin_dir = bin_dir;
      logging_mode = Logging_mode.Journald;
      extra_args = [];
      auto_enable = true;
      bootstrap = Genesis;
      (* No bootstrap - data already exists *)
      preserve_data = true;
      (* Critical: don't touch existing data *)
      snapshot_no_check = false;
      tmp_dir = None;
      keep_snapshot = false;
    }
  in
  match Node.install_node ~quiet:true request with
  | Ok svc -> Ok svc
  | Error e ->
      Error
        (`Msg
           (Printf.sprintf
              "Failed to create node: %s"
              (match e with `Msg m -> m)))

let create_baker_from_external ~instance ~external_svc ~network:_ ~base_dir
    ~node_endpoint ~bin_dir =
  let config = external_svc.External_service.config in
  let service_user = get_service_user external_svc in
  (* Extract delegates if detected *)
  let delegates =
    match config.delegates.value with Some d -> d | None -> []
  in
  let request : baker_request =
    {
      instance;
      node_mode = Remote_endpoint node_endpoint;
      base_dir = Some base_dir;
      delegates;
      dal_config = Dal_disabled;
      dal_node = None;
      liquidity_baking_vote = None;
      extra_args = [];
      service_user;
      app_bin_dir = bin_dir;
      logging_mode = Logging_mode.Journald;
      auto_enable = true;
      preserve_data = true;
    }
  in
  match Baker.install_baker ~quiet:true request with
  | Ok svc -> Ok svc
  | Error e ->
      Error
        (`Msg
           (Printf.sprintf
              "Failed to create baker: %s"
              (match e with `Msg m -> m)))

let create_accuser_from_external ~instance ~external_svc ~network:_ ~base_dir
    ~node_endpoint ~bin_dir =
  let service_user = get_service_user external_svc in
  let request : accuser_request =
    {
      instance;
      node_mode = Remote_endpoint node_endpoint;
      base_dir = Some base_dir;
      extra_args = [];
      service_user;
      app_bin_dir = bin_dir;
      logging_mode = Logging_mode.Journald;
      auto_enable = true;
      preserve_data = true;
    }
  in
  match Accuser.install_accuser ~quiet:true request with
  | Ok svc -> Ok svc
  | Error e ->
      Error
        (`Msg
           (Printf.sprintf
              "Failed to create accuser: %s"
              (match e with `Msg m -> m)))

let create_dal_from_external ~instance ~external_svc ~network ~data_dir
    ~rpc_addr ~net_addr ~node_endpoint ~bin_dir ~strategy =
  let service_user = get_service_user external_svc in
  (* For Clone strategy, increment ports to avoid conflicts *)
  let rpc_addr, net_addr =
    if strategy = Clone then
      let increment_port addr =
        match String.split_on_char ':' addr with
        | [host; port] -> (
            try
              let port_num = int_of_string port in
              Printf.sprintf "%s:%d" host (port_num + 1)
            with _ -> addr)
        | _ -> addr
      in
      (increment_port rpc_addr, increment_port net_addr)
    else (rpc_addr, net_addr)
  in
  let request : daemon_request =
    {
      role = "dal-node";
      instance;
      network;
      history_mode = Rolling;
      data_dir;
      rpc_addr;
      net_addr;
      service_user;
      app_bin_dir = bin_dir;
      logging_mode = Logging_mode.Journald;
      service_args = [];
      extra_env = [("OCTEZ_NODE_ENDPOINT", node_endpoint)];
      extra_paths = [];
      auto_enable = true;
      depends_on = None;
      preserve_data = true;
    }
  in
  match Dal_node.install_daemon ~quiet:true request with
  | Ok svc -> Ok svc
  | Error e ->
      Error
        (`Msg
           (Printf.sprintf
              "Failed to create DAL node: %s"
              (match e with `Msg m -> m)))

(** {1 Rollback} *)

let rollback_import ~original_unit ~new_instance =
  (* 1. Re-enable original unit *)
  let* () =
    match Systemd.enable_unit original_unit with
    | Ok () -> Ok ()
    | Error _ ->
        (* Best effort, log but continue *)
        Ok ()
  in
  (* 2. Start original service *)
  let* () =
    match Systemd.start_unit ~unit_name:original_unit with
    | Ok () -> Ok ()
    | Error e ->
        (* Failed to restart original - this is bad *)
        Error
          (`Msg
             (Printf.sprintf
                "Rollback failed: could not restart original service (%s)"
                (match e with `Msg m -> m)))
  in
  (* 3. Remove partial managed service *)
  let* () =
    match new_instance with
    | Some inst -> (
        match Service_registry.remove ~instance:inst with
        | Ok () -> Ok ()
        | Error _ -> Ok () (* Best effort *))
    | None -> Ok ()
  in
  Ok ()

(** {1 Main Import Function} *)

let import_service ?(on_log = fun _ -> ()) ~options ~external_svc () =
  let log msg = on_log msg in
  let config = external_svc.External_service.config in
  (* 1. Validate *)
  log "Validating service is importable..." ;
  let* () = validate_importable external_svc in
  (* 2. Determine instance name *)
  let instance_name =
    match options.new_instance_name with
    | Some name -> name
    | None -> external_svc.External_service.suggested_instance_name
  in
  (* 3. Check for name conflicts *)
  let* all_services = Service_registry.list () in
  let* () =
    if List.exists (fun s -> s.Service.instance = instance_name) all_services
    then
      Error
        (`Msg
           (Printf.sprintf
              "Instance name '%s' already exists. Use --as to specify a \
               different name."
              instance_name))
    else Ok ()
  in
  (* 4. Resolve required fields *)
  let* network = resolve_network ~overrides:options.overrides ~external_svc in
  let* data_dir = resolve_data_dir ~overrides:options.overrides ~external_svc in
  let* base_dir = resolve_base_dir ~overrides:options.overrides ~external_svc in
  let rpc_addr = resolve_rpc_addr ~overrides:options.overrides ~external_svc in
  let net_addr = resolve_net_addr ~overrides:options.overrides ~external_svc in
  (* 5. Get node endpoint for baker/accuser/dal *)
  let node_endpoint =
    match config.node_endpoint.value with
    | Some ep -> ep
    | None -> "http://127.0.0.1:8732"
  in
  (* 6. Get bin_dir - extract directory from binary path *)
  let bin_dir =
    match config.binary_path.value with
    | Some binary_path -> Filename.dirname binary_path
    | None -> "/usr/bin"
  in
  (* DRY RUN: Stop here and show what would happen *)
  if options.dry_run then (
    log "DRY RUN - Would import:" ;
    log (Printf.sprintf "  Original: %s" config.unit_name) ;
    log (Printf.sprintf "  New name: %s" instance_name) ;
    log
      (Printf.sprintf
         "  Strategy: %s"
         (match options.strategy with
         | Takeover -> "takeover"
         | Clone -> "clone")) ;
    log "" ;
    log "Configuration:" ;
    log (Printf.sprintf "  Network: %s (detected)" network) ;
    let role_str =
      match config.role.value with
      | Some r -> External_service.role_to_string r
      | None -> "unknown"
    in
    log (Printf.sprintf "  Role: %s" role_str) ;
    if data_dir <> "" then log (Printf.sprintf "  Data dir: %s" data_dir) ;
    if base_dir <> "" then log (Printf.sprintf "  Base dir: %s" base_dir) ;
    log (Printf.sprintf "  RPC addr: %s" rpc_addr) ;
    log "" ;
    log "Actions (not executed):" ;
    log (Printf.sprintf "  1. Stop %s" config.unit_name) ;
    log (Printf.sprintf "  2. Create managed service: %s" instance_name) ;
    if options.strategy = Takeover then
      log (Printf.sprintf "  3. Disable %s" config.unit_name) ;
    log (Printf.sprintf "  4. Start %s" instance_name) ;
    Ok
      {
        original_unit = config.unit_name;
        new_instance = instance_name;
        preserved_paths = [];
        warnings = [];
      })
  else
    (* ACTUAL IMPORT *)
    try
      (* 5. Stop external service (for Takeover) *)
      log
        (Printf.sprintf
           "[1/6] Strategy: %s"
           (match options.strategy with
           | Takeover -> "Takeover"
           | Clone -> "Clone")) ;
      let* () =
        if options.strategy = Takeover then (
          log (Printf.sprintf "Stopping external service: %s" config.unit_name) ;
          let result = Systemd.stop_unit ~unit_name:config.unit_name in
          log "Stop completed." ;
          result)
        else (
          log "Clone strategy - keeping original running" ;
          Ok ())
      in
      (* 6. Create managed service based on role *)
      log "[2/6] Creating managed service configuration..." ;
      let* created_svc =
        match config.role.value with
        | Some External_service.Node ->
            create_node_from_external
              ~instance:instance_name
              ~external_svc
              ~network
              ~data_dir
              ~rpc_addr
              ~net_addr
              ~bin_dir
        | Some External_service.Baker ->
            create_baker_from_external
              ~instance:instance_name
              ~external_svc
              ~network
              ~base_dir
              ~node_endpoint
              ~bin_dir
        | Some External_service.Accuser ->
            create_accuser_from_external
              ~instance:instance_name
              ~external_svc
              ~network
              ~base_dir
              ~node_endpoint
              ~bin_dir
        | Some External_service.Dal_node ->
            create_dal_from_external
              ~instance:instance_name
              ~external_svc
              ~network
              ~data_dir
              ~rpc_addr
              ~net_addr
              ~node_endpoint
              ~bin_dir
              ~strategy:options.strategy
        | Some (External_service.Unknown role_str) ->
            Error (`Msg (Printf.sprintf "Unknown role: %s" role_str))
        | None -> Error (`Msg "Role not detected for external service")
      in
      log
        (Printf.sprintf
           "[3/6] Service created: %s (role: %s, user: %s)"
           instance_name
           created_svc.Service.role
           created_svc.Service.service_user) ;
      (* 7. Disable original unit (for Takeover) *)
      let* () =
        if options.strategy = Takeover then (
          log
            (Printf.sprintf
               "[4/6] Disabling original systemd unit: %s"
               config.unit_name) ;
          let result = Systemd.disable_unit config.unit_name in
          log "Disable completed." ;
          result)
        else (
          log "[4/6] Skipping disable (Clone strategy)" ;
          Ok ())
      in
      (* 8. Start managed service *)
      log (Printf.sprintf "[5/6] Starting managed service: %s" instance_name) ;
      let* () =
        Lifecycle.start_service ~quiet:true ~instance:instance_name ()
      in
      log "Start completed." ;
      (* 9. Verify running - systemd start is synchronous, no sleep needed *)
      log "[6/6] Verifying service is active..." ;
      let* unit_state =
        Systemd.get_unit_state
          ~role:created_svc.Service.role
          ~instance:instance_name
      in
      let* () =
        if unit_state.active_state = "active" then Ok ()
        else
          Error
            (`Msg
               (Printf.sprintf
                  "Service failed to start (state: %s)"
                  unit_state.active_state))
      in
      log "Import successful!" ;
      (* Build preserved paths list *)
      let preserved =
        [
          (if data_dir <> "" then Some data_dir else None);
          (if base_dir <> "" then Some base_dir else None);
        ]
        |> List.filter_map Fun.id
      in
      Ok
        {
          original_unit = config.unit_name;
          new_instance = instance_name;
          preserved_paths = preserved;
          warnings = [];
        }
    with e ->
      log "Import failed, rolling back..." ;
      let* () =
        rollback_import
          ~original_unit:config.unit_name
          ~new_instance:(Some instance_name)
      in
      Error (`Msg (Printf.sprintf "Import failed: %s" (Printexc.to_string e)))
