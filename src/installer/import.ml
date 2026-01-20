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

type import_strategy = Installer_types.import_strategy = Takeover | Clone

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
  let config = external_svc.External_service.config in
  let service_user = get_service_user external_svc in
  (* Parse ExecStart to extract extra arguments *)
  let parsed = Execstart_parser.parse config.exec_start in
  (* Preserve extra arguments from original ExecStart (e.g., --metrics-addr, --cors-origin) *)
  let extra_args = parsed.extra_args in
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
      extra_args;
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

(** Extract baker-specific fields from extra_args.
    Returns (delegates, liquidity_baking_vote, remaining_extra_args) 
    
    Note: This uses heuristics to identify delegates as trailing positional arguments.
    According to Octez baker syntax, delegates are always the final arguments on the
    command line, appearing after all flags. We identify them as:
    - Tezos addresses (tz1/tz2/tz3/tz4/KT1 prefixes), or
    - Alphanumeric aliases (letters, numbers, underscores only)
    
    This approach works for standard baker configurations. If users have unusual
    setups (e.g., custom flags after delegates), they may need to manually adjust
    the delegates field after import via the TUI. *)
let extract_baker_fields extra_args =
  let is_likely_delegate arg =
    if String.length arg < 3 then false
    else
      let prefix = String.sub arg 0 3 in
      (* Tezos address prefixes *)
      if
        prefix = "tz1" || prefix = "tz2" || prefix = "tz3" || prefix = "tz4"
        || prefix = "KT1"
      then true
      else
        (* Alias: only alphanumeric and underscore, no slashes or special chars *)
        String.length arg > 0
        && arg.[0] <> '/'
        && arg.[0] <> '-'
        && String.for_all
             (fun c ->
               (c >= 'a' && c <= 'z')
               || (c >= 'A' && c <= 'Z')
               || (c >= '0' && c <= '9')
               || c = '_')
             arg
  in
  (* Extract trailing delegates (reverse list, take while delegate-like, reverse back) *)
  let rec extract_trailing_delegates acc = function
    | [] -> (List.rev acc, [])
    | arg :: rest when is_likely_delegate arg ->
        extract_trailing_delegates (arg :: acc) rest
    | args -> (List.rev acc, args)
  in
  let delegates, remaining_args_rev =
    extract_trailing_delegates [] (List.rev extra_args)
  in
  let remaining_args = List.rev remaining_args_rev in
  (* Extract liquidity baking vote if present *)
  let liquidity_baking_vote, remaining_args =
    let rec extract_lb acc = function
      | [] -> (None, List.rev acc)
      | "--liquidity-baking-toggle-vote" :: value :: rest ->
          (Some value, List.rev_append acc rest)
      | arg :: rest -> extract_lb (arg :: acc) rest
    in
    extract_lb [] remaining_args
  in
  (delegates, liquidity_baking_vote, remaining_args)

let create_baker_from_external ~instance ~external_svc ~network:_ ~base_dir
    ~node_endpoint ~bin_dir ~depends_on ~imported_services
    ~all_external_services =
  let config = external_svc.External_service.config in
  let service_user = get_service_user external_svc in
  (* Parse ExecStart to extract extra arguments *)
  let parsed = Execstart_parser.parse config.exec_start in
  (* Extract baker-specific fields *)
  let delegates, liquidity_baking_vote, remaining_args =
    extract_baker_fields parsed.extra_args
  in
  (* Use Local_instance if we have a managed dependency, otherwise Remote_endpoint *)
  let node_mode =
    match depends_on with
    | Some instance -> Local_instance instance
    | None -> Remote_endpoint node_endpoint
  in
  (* Check for DAL node dependency *)
  let dal_config, dal_node =
    match config.dal_endpoint.value with
    | Some dal_endpoint -> (
        if
          (* Baker has --dal-node, check if we imported that DAL node *)
          Hashtbl.length imported_services = 0 || all_external_services = []
        then (Dal_endpoint dal_endpoint, None)
        else
          let deps =
            External_service.get_dependencies external_svc all_external_services
          in
          let dal_dep =
            List.find_opt
              (fun (dep_name, dep_role) ->
                dep_role = "dal-node" && Hashtbl.mem imported_services dep_name)
              deps
          in
          match dal_dep with
          | Some (dep_name, _) -> (
              match Hashtbl.find_opt imported_services dep_name with
              | Some dal_instance -> (Dal_auto, Some dal_instance)
              | None -> (Dal_endpoint dal_endpoint, None))
          | None -> (Dal_endpoint dal_endpoint, None))
    | None -> (Dal_disabled, None)
  in
  (* Use remaining args after extracting delegates and LB vote *)
  let extra_args = remaining_args in
  let request : baker_request =
    {
      instance;
      node_mode;
      base_dir = Some base_dir;
      delegates;
      dal_config;
      dal_node;
      liquidity_baking_vote;
      extra_args;
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
    ~node_endpoint ~bin_dir ~depends_on =
  let config = external_svc.External_service.config in
  let service_user = get_service_user external_svc in
  (* Parse ExecStart to extract extra arguments *)
  let parsed = Execstart_parser.parse config.exec_start in
  (* Use Local_instance if we have a managed dependency, otherwise Remote_endpoint *)
  let node_mode =
    match depends_on with
    | Some instance -> Local_instance instance
    | None -> Remote_endpoint node_endpoint
  in
  (* Preserve extra arguments from original ExecStart *)
  let extra_args = parsed.extra_args in
  let request : accuser_request =
    {
      instance;
      node_mode;
      base_dir = Some base_dir;
      extra_args;
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
    ~rpc_addr ~net_addr ~node_endpoint ~bin_dir ~strategy ~depends_on =
  let config = external_svc.External_service.config in
  let service_user = get_service_user external_svc in
  (* Parse ExecStart to extract extra arguments *)
  let parsed = Execstart_parser.parse config.exec_start in
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
  (* Preserve extra arguments from original ExecStart (e.g., --attester-profiles) *)
  let service_args = parsed.extra_args in
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
      service_args;
      extra_env = [("OCTEZ_NODE_ENDPOINT", node_endpoint)];
      extra_paths = [];
      auto_enable = true;
      depends_on;
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

(** {1 Dry-Run Helpers} *)

(** Wrap long string to fit within width *)
let wrap_string s max_width =
  if String.length s <= max_width then [s]
  else
    let rec split_at_spaces str acc =
      if String.length str <= max_width then List.rev (str :: acc)
      else
        (* Try to find last space before max_width *)
        let rec find_break pos =
          if pos < 0 then max_width (* No space found, hard break *)
          else if str.[pos] = ' ' then pos
          else find_break (pos - 1)
        in
        let break_pos =
          find_break (min (max_width - 1) (String.length str - 1))
        in
        let line = String.sub str 0 break_pos |> String.trim in
        let rest =
          String.sub str break_pos (String.length str - break_pos)
          |> String.trim
        in
        split_at_spaces rest (line :: acc)
    in
    split_at_spaces s []

(** Format side-by-side comparison for dry-run *)
let format_comparison ~log ~label ~original ~generated =
  let max_len = 40 in
  let pad s =
    let len = String.length s in
    if len >= max_len then String.sub s 0 max_len
    else s ^ String.make (max_len - len) ' '
  in
  log (Printf.sprintf "%s:" label) ;
  log
    (Printf.sprintf
       "  %-40s │ %-40s"
       (pad "ORIGINAL")
       (pad "GENERATED (managed)")) ;
  log (Printf.sprintf "  %s-+-%s" (String.make 40 '-') (String.make 40 '-')) ;
  List.iter2
    (fun (k1, v1) (k2, v2) ->
      let left_lines = wrap_string (k1 ^ ": " ^ v1) max_len in
      let right_lines = wrap_string (k2 ^ ": " ^ v2) max_len in
      let max_lines = max (List.length left_lines) (List.length right_lines) in
      for i = 0 to max_lines - 1 do
        let left =
          if i < List.length left_lines then List.nth left_lines i else ""
        in
        let right =
          if i < List.length right_lines then List.nth right_lines i else ""
        in
        log (Printf.sprintf "  %-40s | %-40s" (pad left) (pad right))
      done)
    original
    generated

(** Show dry-run details for a single service *)
let show_dry_run_details ~log ~external_svc ~instance_name ~network ~data_dir
    ~base_dir:_ ~rpc_addr ~net_addr:_ ~node_endpoint ~bin_dir ~options
    ~depends_on =
  let config = external_svc.External_service.config in
  let role_str =
    match config.role.value with
    | Some r -> External_service.role_to_string r
    | None -> "unknown"
  in

  log "" ;
  log
    "================================================================================" ;
  log (Printf.sprintf "Service: %s -> %s" config.unit_name instance_name) ;
  log
    (Printf.sprintf
       "Role: %s | Strategy: %s"
       role_str
       (match options.strategy with Takeover -> "Takeover" | Clone -> "Clone")) ;
  log
    "================================================================================" ;

  (* Side-by-side configuration *)
  let original_config =
    [
      ("Unit name", config.unit_name);
      ("User", Option.value config.user ~default:"(unset)");
      ("ExecStart", config.exec_start);
    ]
  in
  let generated_config =
    [
      ("Instance", instance_name);
      ("User", get_service_user external_svc);
      ("Managed", "via octez-manager systemd templates");
    ]
  in
  format_comparison
    ~log
    ~label:"Service Identity"
    ~original:original_config
    ~generated:generated_config ;

  log "" ;

  (* Configuration comparison *)
  let original_fields =
    [
      ( "Network",
        match config.network.value with Some n -> n | None -> "(unknown)" );
      ( "Data dir",
        match config.data_dir.value with Some d -> d | None -> "(unknown)" );
      ( "RPC addr",
        match config.rpc_addr.value with Some r -> r | None -> "(unknown)" );
    ]
  in
  let generated_fields =
    [
      ("Network", network);
      ("Data dir", if data_dir = "" then "(not applicable)" else data_dir);
      ("RPC addr", rpc_addr);
    ]
  in
  format_comparison
    ~log
    ~label:"Configuration"
    ~original:original_fields
    ~generated:generated_fields ;

  log "" ;
  log "Generated files:" ;
  log
    (Printf.sprintf
       "  • Environment: /etc/octez/instances/%s/node.env"
       instance_name) ;
  log
    (Printf.sprintf
       "  • Systemd drop-in: \
        /etc/systemd/system/octez-%s@%s.service.d/override.conf"
       (match config.role.value with
       | Some External_service.Dal_node -> "dal-node"
       | Some External_service.Node -> "node"
       | Some External_service.Baker -> "baker"
       | Some External_service.Accuser -> "accuser"
       | _ -> "unknown")
       instance_name) ;

  log "" ;
  log "Actions (not executed):" ;
  if options.strategy = Takeover then (
    log "  1. Stop and disable original service" ;
    log "  2. Create managed service configuration" ;
    log "  3. Start managed service")
  else (
    log "  1. Create managed service configuration (clone)" ;
    log "  2. Keep original service running" ;
    log "  3. Start managed service (will conflict if using same ports)") ;

  (* Show file contents preview *)
  log "" ;
  log
    "────────────────────────────────────────────────────────────────────────────────" ;
  log (Printf.sprintf "Preview: /etc/octez/instances/%s/node.env" instance_name) ;
  log
    "────────────────────────────────────────────────────────────────────────────────" ;
  (match config.role.value with
  | Some External_service.Dal_node -> (
      log (Printf.sprintf "OCTEZ_DAL_DATA_DIR=%s" data_dir) ;
      log (Printf.sprintf "OCTEZ_DAL_RPC_ADDR=%s" rpc_addr) ;
      (match config.net_addr.value with
      | Some addr -> log (Printf.sprintf "OCTEZ_DAL_NET_ADDR=%s" addr)
      | None -> ()) ;
      log (Printf.sprintf "OCTEZ_NETWORK=%s" network) ;
      match config.node_endpoint.value with
      | Some ep -> log (Printf.sprintf "OCTEZ_NODE_ENDPOINT=%s" ep)
      | None -> ())
  | Some External_service.Node ->
      log (Printf.sprintf "OCTEZ_DATA_DIR=%s" data_dir) ;
      log (Printf.sprintf "OCTEZ_RPC_ADDR=%s" rpc_addr) ;
      (match config.net_addr.value with
      | Some addr -> log (Printf.sprintf "OCTEZ_NET_ADDR=%s" addr)
      | None -> ()) ;
      log (Printf.sprintf "OCTEZ_NETWORK=%s" network)
  | Some External_service.Baker | Some External_service.Accuser ->
      (match config.base_dir.value with
      | Some bd -> log (Printf.sprintf "OCTEZ_CLIENT_BASE_DIR=%s" bd)
      | None -> ()) ;
      (match config.node_endpoint.value with
      | Some ep -> log (Printf.sprintf "OCTEZ_NODE_ENDPOINT=%s" ep)
      | None -> ()) ;
      log (Printf.sprintf "OCTEZ_NETWORK=%s" network)
  | _ -> log "(role-specific environment variables)") ;
  (match config.binary_path.value with
  | Some bp ->
      let bin_dir = Filename.dirname bp in
      log (Printf.sprintf "APP_BIN_DIR=%s" bin_dir)
  | None -> ()) ;

  log "" ;
  log
    "────────────────────────────────────────────────────────────────────────────────" ;
  log
    (Printf.sprintf
       "Preview: /etc/systemd/system/octez-%s@%s.service.d/override.conf"
       (match config.role.value with
       | Some External_service.Dal_node -> "dal-node"
       | Some External_service.Node -> "node"
       | Some External_service.Baker -> "baker"
       | Some External_service.Accuser -> "accuser"
       | _ -> "unknown")
       instance_name) ;
  log
    "────────────────────────────────────────────────────────────────────────────────" ;
  log "[Service]" ;
  log
    (Printf.sprintf
       "EnvironmentFile=/etc/octez/instances/%s/node.env"
       instance_name) ;
  log "" ;
  log
    "────────────────────────────────────────────────────────────────────────────────" ;

  log "" ;
  log
    "────────────────────────────────────────────────────────────────────────────────" ;
  log
    (Printf.sprintf
       "Preview: Service Registry Entry (/var/lib/octez-manager/services.json)") ;
  log
    "────────────────────────────────────────────────────────────────────────────────" ;
  log "{" ;
  log (Printf.sprintf "  \"instance\": \"%s\"," instance_name) ;
  log
    (Printf.sprintf
       "  \"role\": \"%s\","
       (match config.role.value with
       | Some External_service.Node -> "node"
       | Some External_service.Dal_node -> "dal-node"
       | Some External_service.Baker -> "baker"
       | Some External_service.Accuser -> "accuser"
       | _ -> "unknown")) ;
  log (Printf.sprintf "  \"network\": \"%s\"," network) ;
  (* Parse ExecStart to show extra_args *)
  let parsed = Execstart_parser.parse config.exec_start in
  (match config.role.value with
  | Some External_service.Node ->
      log (Printf.sprintf "  \"data_dir\": \"%s\"," data_dir) ;
      log (Printf.sprintf "  \"rpc_addr\": \"%s\"," rpc_addr) ;
      log
        (Printf.sprintf
           "  \"net_addr\": \"%s\","
           (Option.value config.net_addr.value ~default:"")) ;
      log "  \"history_mode\": \"rolling\"," ;
      log
        (Printf.sprintf
           "  \"service_user\": \"%s\","
           (get_service_user external_svc)) ;
      log (Printf.sprintf "  \"app_bin_dir\": \"%s\"," bin_dir) ;
      if List.length parsed.extra_args > 0 then
        log
          (Printf.sprintf
             "  \"extra_args\": [%s],"
             (String.concat
                ", "
                (List.map (Printf.sprintf "\"%s\"") parsed.extra_args)))
      else log "  \"extra_args\": [],"
  | Some External_service.Dal_node ->
      log (Printf.sprintf "  \"data_dir\": \"%s\"," data_dir) ;
      log (Printf.sprintf "  \"rpc_addr\": \"%s\"," rpc_addr) ;
      log
        (Printf.sprintf
           "  \"net_addr\": \"%s\","
           (Option.value config.net_addr.value ~default:"")) ;
      log
        (Printf.sprintf
           "  \"service_user\": \"%s\","
           (get_service_user external_svc)) ;
      log (Printf.sprintf "  \"app_bin_dir\": \"%s\"," bin_dir) ;
      (match depends_on with
      | Some inst ->
          log
            (Printf.sprintf
               "  \"depends_on\": \"%s\",  ✓ Linked to managed node!"
               inst)
      | None -> log "  \"depends_on\": null,  ℹ️ Using remote node endpoint") ;
      if List.length parsed.extra_args > 0 then
        log
          (Printf.sprintf
             "  \"service_args\": [%s],"
             (String.concat
                ", "
                (List.map (Printf.sprintf "\"%s\"") parsed.extra_args)))
      else log "  \"service_args\": [],"
  | Some External_service.Baker ->
      (* Extract baker fields to show processed values *)
      let delegates, liquidity_baking_vote, processed_extra_args =
        extract_baker_fields parsed.extra_args
      in
      (match config.base_dir.value with
      | Some bd -> log (Printf.sprintf "  \"base_dir\": \"%s\"," bd)
      | None -> ()) ;
      log
        (Printf.sprintf
           "  \"service_user\": \"%s\","
           (get_service_user external_svc)) ;
      log (Printf.sprintf "  \"app_bin_dir\": \"%s\"," bin_dir) ;
      (match depends_on with
      | Some inst ->
          log
            (Printf.sprintf
               "  \"node_mode\": \"Local_instance(%s)\",  ✓ Linked to managed \
                node!"
               inst)
      | None ->
          log
            (Printf.sprintf
               "  \"node_mode\": \"Remote_endpoint(%s)\",  ℹ️ Using remote \
                endpoint"
               node_endpoint)) ;
      (* Show extracted delegates *)
      if List.length delegates > 0 then
        log
          (Printf.sprintf
             "  \"delegates\": [%s],"
             (String.concat ", " (List.map (Printf.sprintf "\"%s\"") delegates))) ;
      (* Show extracted LB vote *)
      (match liquidity_baking_vote with
      | Some vote ->
          log (Printf.sprintf "  \"liquidity_baking_vote\": \"%s\"," vote)
      | None -> ()) ;
      (* Show remaining extra args after extraction *)
      if List.length processed_extra_args > 0 then
        log
          (Printf.sprintf
             "  \"extra_args\": [%s]"
             (String.concat
                ", "
                (List.map (Printf.sprintf "\"%s\"") processed_extra_args)))
      else log "  \"extra_args\": []"
  | Some External_service.Accuser ->
      (match config.base_dir.value with
      | Some bd -> log (Printf.sprintf "  \"base_dir\": \"%s\"," bd)
      | None -> ()) ;
      log
        (Printf.sprintf
           "  \"service_user\": \"%s\","
           (get_service_user external_svc)) ;
      log (Printf.sprintf "  \"app_bin_dir\": \"%s\"," bin_dir) ;
      (match depends_on with
      | Some inst ->
          log
            (Printf.sprintf
               "  \"node_mode\": \"Local_instance(%s)\",  ✓ Linked to managed \
                node!"
               inst)
      | None ->
          log
            (Printf.sprintf
               "  \"node_mode\": \"Remote_endpoint(%s)\",  ℹ️ Using remote \
                endpoint"
               node_endpoint)) ;
      if List.length parsed.extra_args > 0 then
        log
          (Printf.sprintf
             "  \"extra_args\": [%s]"
             (String.concat
                ", "
                (List.map (Printf.sprintf "\"%s\"") parsed.extra_args)))
      else log "  \"extra_args\": []"
  | _ -> ()) ;
  log "}" ;
  log "" ;
  log
    "────────────────────────────────────────────────────────────────────────────────"

(** {1 Main Import Function} *)

(** Map external unit name -> managed instance name for dependency resolution *)
type _imported_map = (string, string) Hashtbl.t

let import_service ?(on_log = fun _ -> ())
    ?(imported_services = Hashtbl.create 0)
    ?(all_external_services : External_service.t list = []) ~options
    ~external_svc () =
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
  (* 5. Get node endpoint for baker/accuser/dal and resolve depends_on *)
  let node_endpoint =
    match config.node_endpoint.value with
    | Some ep -> ep
    | None -> "http://127.0.0.1:8732"
  in
  (* 5b. Resolve dependency to managed instance if in cascade import *)
  (* For baker: check run_mode to decide if we should link to local instance *)
  (* For accuser: always use remote endpoint (no local mode) *)
  (* For DAL: link to node if available *)
  let depends_on_instance =
    if Hashtbl.length imported_services = 0 || all_external_services = [] then
      None
    else
      match config.role.value with
      | Some External_service.Baker -> (
          (* Baker: only use Local_instance if original was "with local node" *)
          let parsed = Execstart_parser.parse config.exec_start in
          match parsed.run_mode with
          | Some "with local node" -> (
              (* Find node dependency that was imported *)
              let deps =
                External_service.get_dependencies
                  external_svc
                  all_external_services
              in
              let node_dep =
                List.find_opt
                  (fun (dep_name, dep_role) ->
                    dep_role = "node" && Hashtbl.mem imported_services dep_name)
                  deps
              in
              match node_dep with
              | Some (dep_name, _) ->
                  Hashtbl.find_opt imported_services dep_name
              | None -> None)
          | Some "remotely" | Some _ | None ->
              (* Keep remote endpoint for "remotely" or unknown modes *)
              None)
      | Some External_service.Accuser ->
          (* Accuser always runs remotely, never link *)
          None
      | Some External_service.Dal_node -> (
          (* DAL: link to node if available *)
          let deps =
            External_service.get_dependencies external_svc all_external_services
          in
          let node_dep =
            List.find_opt
              (fun (dep_name, dep_role) ->
                dep_role = "node" && Hashtbl.mem imported_services dep_name)
              deps
          in
          match node_dep with
          | Some (dep_name, _) -> Hashtbl.find_opt imported_services dep_name
          | None -> None)
      | _ -> None
  in
  (* 6. Get bin_dir - extract directory from binary path *)
  let bin_dir =
    match config.binary_path.value with
    | Some binary_path -> Filename.dirname binary_path
    | None -> "/usr/bin"
  in
  (* DRY RUN: Stop here and show what would happen *)
  if options.dry_run then (
    show_dry_run_details
      ~log
      ~external_svc
      ~instance_name
      ~network
      ~data_dir
      ~base_dir
      ~rpc_addr
      ~net_addr
      ~node_endpoint
      ~bin_dir
      ~options
      ~depends_on:depends_on_instance ;
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
              ~depends_on:depends_on_instance
              ~imported_services
              ~all_external_services
        | Some External_service.Accuser ->
            create_accuser_from_external
              ~instance:instance_name
              ~external_svc
              ~network
              ~base_dir
              ~node_endpoint
              ~bin_dir
              ~depends_on:depends_on_instance
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
              ~depends_on:depends_on_instance
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

(** {1 Cascade Import} *)

let import_cascade ?(on_log = fun _ -> ()) ~options ~external_svc ~all_services
    () =
  let log msg = on_log msg in

  (* 1. Build dependency chain *)
  log "Analyzing dependency chain..." ;
  let chain =
    match options.strategy with
    | Takeover ->
        (* For Takeover: must import ALL affected services (dependencies + dependents) *)
        log
          "Takeover strategy: computing full cascade (dependencies + \
           dependents)..." ;
        Import_cascade.get_full_cascade ~service:external_svc ~all_services
    | Clone ->
        (* For Clone: only need dependencies *)
        log "Clone strategy: computing dependency chain..." ;
        Import_cascade.get_dependency_chain ~service:external_svc ~all_services
  in

  log
    (Printf.sprintf
       "Found %d services to import%s"
       (List.length chain)
       (match options.strategy with
       | Takeover -> " (including dependencies and dependents)"
       | Clone -> " (including dependencies)")) ;

  (* 2. Validate cascade *)
  log "Validating import cascade..." ;
  let* () =
    match
      Import_cascade.validate_cascade
        ~services:all_services
        ~target_services:chain
        ~strategy:options.strategy
    with
    | Ok () -> Ok ()
    | Error err ->
        let msg = Format.asprintf "%a" Import_cascade.pp_validation_error err in
        Error (`Msg msg)
  in

  (* 3. Get import order *)
  let analysis =
    Import_cascade.analyze_dependencies
      ~services:all_services
      ~target_services:chain
  in

  log "" ;
  log
    (Printf.sprintf
       "Import order: %s"
       (String.concat
          " → "
          (List.map
             (fun name ->
               match
                 List.find_opt
                   (fun s -> s.External_service.config.unit_name = name)
                   chain
               with
               | Some svc -> (
                   match svc.External_service.config.role.value with
                   | Some r ->
                       Printf.sprintf
                         "%s (%s)"
                         name
                         (External_service.role_to_string r)
                   | None -> name)
               | None -> name)
             analysis.import_order))) ;

  (* DRY RUN: Show plan for all services *)
  if options.dry_run then (
    log "" ;
    log
      "===================================================================================" ;
    log
      "                          CASCADE IMPORT \
       DRY-RUN                                   " ;
    log
      "===================================================================================" ;
    log "" ;
    log
      (Printf.sprintf
         "Strategy: %s"
         (match options.strategy with
         | Takeover -> "Takeover"
         | Clone -> "Clone")) ;
    log (Printf.sprintf "Services to import: %d" (List.length chain)) ;

    (* Track imported services for dependency linking during dry-run *)
    let imported_map = Hashtbl.create 17 in
    (* Pre-populate map with all services that will be imported (for dry-run) *)
    List.iter
      (fun svc_name ->
        match
          List.find_opt
            (fun s -> s.External_service.config.unit_name = svc_name)
            chain
        with
        | Some svc ->
            Hashtbl.add
              imported_map
              svc.External_service.config.unit_name
              svc.External_service.suggested_instance_name
        | None -> ())
      analysis.import_order ;

    (* Show each service in import order *)
    List.iteri
      (fun i svc_name ->
        match
          List.find_opt
            (fun s -> s.External_service.config.unit_name = svc_name)
            chain
        with
        | Some svc ->
            log "" ;
            log
              (Printf.sprintf
                 "--- Service %d of %d \
                  --------------------------------------------------------------"
                 (i + 1)
                 (List.length chain)) ;
            (* Call import_service in dry-run with tracking for dependency linking *)
            let _ =
              import_service
                ~on_log
                ~imported_services:imported_map
                ~all_external_services:all_services
                ~options
                ~external_svc:svc
                ()
            in
            ()
        | None -> ())
      analysis.import_order ;

    log "" ;
    log
      "===================================================================================" ;
    log
      "                        END CASCADE IMPORT \
       DRY-RUN                                 " ;
    log
      "===================================================================================" ;
    log "" ;
    log "Summary:" ;
    log (Printf.sprintf "  • Total services: %d" (List.length chain)) ;
    log "  • No changes have been made" ;
    log "" ;
    log "To execute this import, run the same command without --dry-run" ;

    (* Return mock results *)
    Ok
      (List.map
         (fun svc ->
           {
             original_unit = svc.External_service.config.unit_name;
             new_instance = svc.External_service.suggested_instance_name;
             preserved_paths = [];
             warnings = [];
           })
         chain))
  else
    (* 4. Import each service in order *)
    let results = ref [] in
    let imported_instances = ref [] in
    let imported_map = Hashtbl.create 17 in

    try
      List.iter
        (fun svc_name ->
          match
            List.find_opt
              (fun s -> s.External_service.config.unit_name = svc_name)
              chain
          with
          | None ->
              raise
                (Failure
                   (Printf.sprintf "Service %s not found in chain" svc_name))
          | Some svc ->
              log "" ;
              log (Printf.sprintf "Importing %s..." svc_name) ;
              let result =
                match
                  import_service
                    ~on_log
                    ~imported_services:imported_map
                    ~all_external_services:all_services
                    ~options
                    ~external_svc:svc
                    ()
                with
                | Ok r ->
                    results := r :: !results ;
                    imported_instances := r.new_instance :: !imported_instances ;
                    (* Track this import for dependency resolution *)
                    Hashtbl.add imported_map r.original_unit r.new_instance ;
                    r
                | Error (`Msg msg) -> raise (Failure msg)
              in
              log
                (Printf.sprintf
                   "✓ %s imported as %s"
                   svc_name
                   result.new_instance))
        analysis.import_order ;
      Ok (List.rev !results)
    with e ->
      (* Rollback all imported services in reverse order *)
      log "" ;
      log "Cascade import failed, rolling back..." ;
      List.iter
        (fun instance ->
          log (Printf.sprintf "  Rolling back %s..." instance) ;
          match Removal.remove_service ~delete_data_dir:false ~instance () with
          | Ok () -> log (Printf.sprintf "  ✓ %s removed" instance)
          | Error (`Msg msg) ->
              log (Printf.sprintf "  ⚠ Failed to remove %s: %s" instance msg))
        !imported_instances ;
      Error
        (`Msg
           (Printf.sprintf "Cascade import failed: %s" (Printexc.to_string e)))
