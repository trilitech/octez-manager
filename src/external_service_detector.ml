(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Rresult

let ( let* ) = Result.bind

(** {1 Helpers} *)

let string_contains ~needle haystack =
  let nlen = String.length needle in
  let hlen = String.length haystack in
  let rec loop idx =
    if idx + nlen > hlen then false
    else if String.sub haystack idx nlen = needle then true
    else loop (idx + 1)
  in
  if nlen = 0 then true else loop 0

(** {1 Cache} *)

let cache : External_service.t list ref = ref []

let cache_lock = Mutex.create ()

let get_cached () = Mutex.protect cache_lock (fun () -> !cache)

let clear_cache () = Mutex.protect cache_lock (fun () -> cache := [])

(** {1 Filtering} *)

(** Check if unit name matches octez-manager's naming convention.
    Pattern: octez-<role>@<instance>.service *)
let is_managed_unit_name unit_name =
  (* Must start with "octez-" *)
  if not (String.starts_with ~prefix:"octez-" unit_name) then false
  else
    (* Must contain exactly one @ symbol *)
    match String.split_on_char '@' unit_name with
    | [_role_part; instance_part] ->
        (* instance_part should end with .service *)
        String.ends_with ~suffix:".service" instance_part
    | _ -> false

let is_in_registry ~unit_name =
  (* Extract instance name from unit name *)
  match String.split_on_char '@' unit_name with
  | [_; instance_part] -> (
      (* Remove .service suffix *)
      let instance =
        if String.ends_with ~suffix:".service" instance_part then
          String.sub instance_part 0 (String.length instance_part - 8)
        else instance_part
      in
      (* Check registry *)
      match Service_registry.find ~instance with
      | Ok (Some _) -> true
      | Ok None -> false
      | Error _ -> false)
  | _ -> false

(** {1 Systemd Queries} *)

let systemctl_cmd () =
  if Common.is_root () then ["systemctl"] else ["systemctl"; "--user"]

let list_all_service_units () =
  (* List all service units, including inactive and disabled ones *)
  let cmd =
    systemctl_cmd ()
    @ ["list-units"; "--type=service"; "--all"; "--no-legend"; "--no-pager"]
  in
  match Common.run_out cmd with
  | Ok output ->
      let lines = String.split_on_char '\n' output in
      let units =
        List.filter_map
          (fun line ->
            let trimmed = String.trim line in
            if trimmed = "" then None
            else
              (* Line format: "unit.service   loaded   active   running   Description" *)
              (* Extract first field (unit name) *)
              match String.split_on_char ' ' trimmed with
              | unit_name :: _
                when String.ends_with ~suffix:".service" unit_name ->
                  Some unit_name
              | _ -> None)
          lines
      in
      Ok units
  | Error (`Msg msg) -> Error msg

(** Extract command from systemd's structured ExecStart format.
    Input: "{ path=/bin/foo ; argv[]=/bin/foo --arg val ; ... }"
    Output: "/bin/foo --arg val" *)
let extract_command_from_systemd_format str =
  (* Look for argv[]= ... ; pattern *)
  try
    let argv_start = String.index str '[' in
    (* Check if this is argv[...] by looking backwards from [ *)
    if argv_start >= 4 && String.sub str (argv_start - 4) 4 = "argv" then
      (* Find the content between argv[]= and the next ; *)
      let content_start = String.index_from str argv_start '=' + 1 in
      let content_end =
        try String.index_from str content_start ';'
        with Not_found -> String.length str
      in
      let command =
        String.sub str content_start (content_end - content_start)
      in
      Some (String.trim command)
    else None
  with Not_found -> None

let get_exec_start ~unit_name =
  let cmd =
    systemctl_cmd () @ ["show"; unit_name; "-p"; "ExecStart"; "--value"]
  in
  match Common.run_out cmd with
  | Ok output ->
      let trimmed = String.trim output in
      if trimmed = "" || trimmed = "[not set]" then None
      else if String.starts_with ~prefix:"{" trimmed then
        (* Structured format from systemd - extract argv[]= part *)
        extract_command_from_systemd_format trimmed
      else
        (* Plain format - use as-is *)
        Some trimmed
  | Error _ -> None

let get_unit_properties ~unit_name ~props =
  (* Query all properties at once without -p flag to avoid parsing issues *)
  let cmd = systemctl_cmd () @ ["show"; unit_name] in
  match Common.run_out cmd with
  | Ok output ->
      let lines = String.split_on_char '\n' output in
      let all_props =
        List.filter_map
          (fun line ->
            match String.split_on_char '=' line with
            | [] | [_] -> None
            | prop :: rest ->
                let value = String.concat "=" rest in
                Some (String.trim prop, String.trim value))
          lines
      in
      (* Filter to only requested properties *)
      List.filter (fun (prop, _) -> List.mem prop props) all_props
  | Error _ -> []

let get_unit_content ~unit_name =
  let cmd = systemctl_cmd () @ ["cat"; unit_name] in
  match Common.run_out cmd with
  | Ok content -> Ok content
  | Error (`Msg msg) ->
      let msg_lower = String.lowercase_ascii msg in
      if string_contains ~needle:"permission denied" msg_lower then
        Error `Permission_denied
      else Error (`Error msg)

(** {1 Network Detection} *)

(** Known Tezos network chain IDs *)
let chain_id_to_network = function
  | "NetXdQprcVkpaWU" -> Some "mainnet"
  | "NetXnHfVqm9iesp" -> Some "ghostnet"
  | "NetXsqzbfFenSTS" -> Some "shadownet"
  | _ -> None

(** Probe RPC endpoint to get chain_id and detect network.
    Returns (chain_id, network_name) if successful. *)
let probe_rpc_chain_id rpc_addr =
  let url =
    if String.starts_with ~prefix:"http" rpc_addr then rpc_addr
    else "http://" ^ rpc_addr
  in
  let full_url = url ^ "/chains/main/chain_id" in
  (* Use curl with short timeout *)
  let cmd = ["curl"; "-s"; "--max-time"; "2"; full_url] in
  match Common.run_out cmd with
  | Ok output -> (
      try
        let trimmed = String.trim output in
        (* Response is JSON string like "NetXdQprcVkpaWU" *)
        let chain_id =
          if String.length trimmed > 2 && trimmed.[0] = '"' then
            (* Remove surrounding quotes *)
            String.sub trimmed 1 (String.length trimmed - 2)
          else trimmed
        in
        let network = chain_id_to_network chain_id in
        Some (chain_id, network)
      with _ -> None)
  | Error _ -> None

(** {1 Process Inspection} *)

(** Read /proc/PID/cmdline for a running process.
    Returns the actual command line with all variables expanded. *)
let read_proc_cmdline pid =
  if pid <= 0 then None
  else
    let path = Printf.sprintf "/proc/%d/cmdline" pid in
    try
      let ic = open_in path in
      Fun.protect
        ~finally:(fun () -> close_in_noerr ic)
        (fun () ->
          (* /proc files report size 0, so we must read until EOF *)
          let buffer = Buffer.create 4096 in
          let rec read_loop () =
            try
              Buffer.add_channel buffer ic 4096 ;
              read_loop ()
            with End_of_file -> ()
          in
          read_loop () ;
          let content = Buffer.contents buffer in
          (* cmdline is null-separated, convert to spaces *)
          let cmdline =
            String.map (fun c -> if c = '\000' then ' ' else c) content
          in
          let trimmed = String.trim cmdline in
          if String.length trimmed > 0 then Some trimmed else None)
    with
    | Sys_error _ -> None
    | End_of_file -> None

(** Get the actual running command for a service if it's active.
    Returns expanded command from /proc/PID/cmdline. *)
let get_running_command ~unit_name =
  (* Get MainPID from systemctl show *)
  let cmd =
    systemctl_cmd () @ ["show"; unit_name; "-p"; "MainPID"; "--value"]
  in
  match Common.run_out cmd with
  | Ok output -> (
      let trimmed = String.trim output in
      match int_of_string_opt trimmed with
      | Some pid when pid > 0 -> read_proc_cmdline pid
      | _ -> None)
  | Error _ -> None

(** {1 Detection Logic} *)

(** Detect daily_logs directory based on role and data_dir/base_dir.
    Returns Some path if the directory exists. *)
let detect_daily_logs_dir ~role ~data_dir ~base_dir =
  let check_dir dir =
    if dir <> "" && Sys.file_exists dir && Sys.is_directory dir then Some dir
    else None
  in
  match role with
  | External_service.Node ->
      (* Node: <data_dir>/daily_logs/ *)
      check_dir (Filename.concat data_dir "daily_logs")
  | External_service.Baker ->
      (* Baker: <base_dir>/logs/octez-baker/ *)
      let base = if base_dir <> "" then base_dir else data_dir in
      check_dir (Filename.concat (Filename.concat base "logs") "octez-baker")
  | External_service.Accuser ->
      (* Accuser: <base_dir>/logs/octez-accuser/ *)
      let base = if base_dir <> "" then base_dir else data_dir in
      check_dir (Filename.concat (Filename.concat base "logs") "octez-accuser")
  | External_service.Dal_node ->
      (* DAL node: <data_dir>/daily_logs/ *)
      check_dir (Filename.concat data_dir "daily_logs")
  | External_service.Unknown _ ->
      (* Unknown role: try generic daily_logs *)
      check_dir (Filename.concat data_dir "daily_logs")

(** Check if ExecStart contains an octez binary *)
let contains_octez_binary exec_start =
  let lower = String.lowercase_ascii exec_start in
  string_contains ~needle:"octez-node" lower
  || string_contains ~needle:"octez-baker" lower
  || string_contains ~needle:"octez-accuser" lower
  || string_contains ~needle:"octez-dal-node" lower
  || string_contains ~needle:"tezos-baker" lower
  || string_contains ~needle:"tezos-accuser" lower

(** Build External_service.t from a unit name, ExecStart, and systemd properties.
    Parses ExecStart to extract configuration. *)
let build_external_service ~unit_name ~exec_start ~properties =
  (* Get basic systemd properties *)
  let user = List.assoc_opt "User" properties in
  let group = List.assoc_opt "Group" properties in
  let working_dir = List.assoc_opt "WorkingDirectory" properties in
  let active_state =
    List.assoc_opt "ActiveState" properties |> Option.value ~default:"unknown"
  in
  let sub_state =
    List.assoc_opt "SubState" properties |> Option.value ~default:"unknown"
  in
  let unit_file_state = List.assoc_opt "UnitFileState" properties in

  (* Determine if enabled *)
  let enabled =
    match unit_file_state with
    | Some "enabled" -> Some true
    | Some "disabled" -> Some false
    | _ -> None
  in

  let unit_state : External_service.unit_state =
    {active_state; sub_state; enabled}
  in

  (* Get environment files *)
  let environment_files =
    match List.assoc_opt "EnvironmentFile" properties with
    | Some files_str ->
        (* Can be multiple files separated by spaces or ; *)
        String.split_on_char ' ' files_str
        |> List.filter (fun s -> String.trim s <> "")
    | None -> []
  in

  (* Try to get actual running command for active services *)
  let command_to_parse, command_source =
    if active_state = "active" then
      match get_running_command ~unit_name with
      | Some running_cmd -> (running_cmd, "/proc/PID/cmdline")
      | None -> (exec_start, "ExecStart")
    else (exec_start, "ExecStart")
  in

  (* Parse command line *)
  let parsed = Execstart_parser.parse command_to_parse in

  (* Read environment files if parsing found unexpanded variables *)
  let env_vars =
    if List.length parsed.warnings > 0 && List.length environment_files > 0 then
      (* Try to read env files to expand variables *)
      List.fold_left
        (fun acc file_path ->
          (* Handle optional files (prefixed with -) *)
          let actual_path =
            if String.starts_with ~prefix:"-" file_path then
              String.sub file_path 1 (String.length file_path - 1)
            else file_path
          in
          match Env_file_parser.parse_file actual_path with
          | Ok pairs -> acc @ pairs
          | Error _ -> acc)
        []
        environment_files
    else []
  in

  (* Helper to build a field, handling variable expansion *)
  let build_field parsed_value =
    match parsed_value with
    | None -> External_service.unknown ()
    | Some value ->
        if env_vars = [] then
          (* No env vars available, use value as-is *)
          External_service.detected ~source:command_source value
        else
          (* Try to expand variables *)
          let expanded = Env_file_parser.expand_vars ~env:env_vars value in
          if expanded = value then
            (* No expansion happened (no variables or not found) *)
            External_service.detected ~source:command_source value
          else
            (* Expansion happened - mark as inferred from env file *)
            External_service.inferred ~source:"EnvironmentFile" expanded
  in

  (* Build fields from parsed data *)
  let binary_field = build_field parsed.binary_path in

  let role_field =
    match binary_field.value with
    | Some binary ->
        let role =
          External_service.role_of_binary_name
            ?subcommand:parsed.subcommand
            binary
        in
        {binary_field with value = Some role}
    | None -> External_service.unknown ()
  in

  let data_dir_field = build_field parsed.data_dir in
  let base_dir_field = build_field parsed.base_dir in
  let rpc_addr_field = build_field parsed.rpc_addr in
  let net_addr_field = build_field parsed.net_addr in
  let endpoint_field = build_field parsed.endpoint in
  let history_mode_field = build_field parsed.history_mode in

  (* Try to detect network via RPC probe if not already known *)
  let network_field =
    let parsed_network = build_field parsed.network in
    match (parsed_network.value, active_state) with
    | None, "active" -> (
        (* No network but service is active - try probe *)
        (* Try rpc_addr first (for nodes), then endpoint (for bakers/accusers) *)
        let probe_addr =
          match rpc_addr_field.value with
          | Some addr -> Some addr
          | None -> endpoint_field.value
        in
        match probe_addr with
        | Some addr -> (
            match probe_rpc_chain_id addr with
            | Some (_chain_id, Some network_name) ->
                External_service.inferred ~source:"RPC probe" network_name
            | _result -> parsed_network)
        | None -> parsed_network)
    | _ -> parsed_network
  in

  (* Detect daily_logs directory if we have role and data_dir *)
  let daily_logs_dir =
    match (role_field.value, data_dir_field.value, base_dir_field.value) with
    | Some role, Some data_dir, base_dir_opt ->
        let base_dir = Option.value ~default:"" base_dir_opt in
        detect_daily_logs_dir ~role ~data_dir ~base_dir
    | _ -> None
  in

  (* Build config *)
  let config =
    {
      (External_service.empty_config ~unit_name ~exec_start ~unit_state) with
      user;
      group;
      working_dir;
      environment_files;
      role = role_field;
      binary_path = binary_field;
      data_dir = data_dir_field;
      base_dir = base_dir_field;
      rpc_addr = rpc_addr_field;
      net_addr = net_addr_field;
      node_endpoint = endpoint_field;
      history_mode = history_mode_field;
      network = network_field;
      daily_logs_dir;
      extra_args = parsed.extra_args;
      parse_warnings = parsed.warnings;
    }
  in

  let suggested_instance_name =
    External_service.suggest_instance_name ~unit_name
  in

  {External_service.config; suggested_instance_name}

let infer_network_from_endpoint services =
  (* Build a map of RPC addr -> network for nodes *)
  let node_networks = Hashtbl.create 17 in
  List.iter
    (fun (svc : External_service.t) ->
      match
        ( svc.config.role.value,
          svc.config.network.value,
          svc.config.rpc_addr.value )
      with
      | Some External_service.Node, Some network, Some rpc_addr ->
          (* Normalize rpc_addr: remove http:// prefix if present *)
          let normalized_addr =
            if String.starts_with ~prefix:"http://" rpc_addr then
              String.sub rpc_addr 7 (String.length rpc_addr - 7)
            else rpc_addr
          in
          Hashtbl.replace node_networks normalized_addr network
      | _ -> ())
    services ;

  (* Now update bakers/accusers/dal-nodes that have unknown network but known endpoint *)
  List.map
    (fun (svc : External_service.t) ->
      match
        ( svc.config.role.value,
          svc.config.network.value,
          svc.config.node_endpoint.value )
      with
      | ( Some
            ( External_service.Baker | External_service.Accuser
            | External_service.Dal_node ),
          None,
          Some endpoint ) -> (
          (* Normalize endpoint *)
          let normalized_endpoint =
            if String.starts_with ~prefix:"http://" endpoint then
              String.sub endpoint 7 (String.length endpoint - 7)
            else endpoint
          in
          match Hashtbl.find_opt node_networks normalized_endpoint with
          | Some network ->
              let new_network =
                External_service.inferred ~source:"connected node" network
              in
              {svc with config = {svc.config with network = new_network}}
          | None -> svc)
      | _ -> svc)
    services

(** Convert a standalone process to an External_service.t *)
let process_to_external_service (proc : Process_scanner.process_info) =
  let cmdline = proc.cmdline in
  let binary_path = Option.value ~default:"octez" proc.binary_path in
  (* Prefer realpath for version detection (absolute path resolved from /proc/PID/exe) *)
  let binary_for_version =
    match proc.binary_realpath with
    | Some realpath -> realpath
    | None -> binary_path
  in

  (* Parse role from command line *)
  let role =
    let subcommand =
      if Str.string_match (Str.regexp ".* run dal\\b") cmdline 0 then Some "dal"
      else None
    in
    let detected_role =
      External_service.role_of_binary_name ?subcommand binary_path
    in
    External_service.detected ~source:"cmdline" detected_role
  in

  (* Parse configuration from command line *)
  let parsed = Execstart_parser.parse cmdline in
  let data_dir =
    match parsed.data_dir with
    | Some d -> External_service.detected ~source:"cmdline" d
    | None -> External_service.unknown ()
  in
  let rpc_addr =
    match parsed.rpc_addr with
    | Some r -> External_service.detected ~source:"cmdline" r
    | None -> External_service.unknown ()
  in
  let node_endpoint =
    match parsed.endpoint with
    | Some e -> External_service.detected ~source:"cmdline" e
    | None -> External_service.unknown ()
  in
  let base_dir =
    match parsed.base_dir with
    | Some b -> External_service.detected ~source:"cmdline" b
    | None -> External_service.unknown ()
  in
  let network =
    match parsed.network with
    | Some n -> External_service.detected ~source:"cmdline" n
    | None -> External_service.unknown ()
  in

  (* Create minimal unit_state (always active for running processes) *)
  let unit_state =
    External_service.
      {active_state = "active"; sub_state = "running"; enabled = None}
  in

  (* Detect daily_logs directory if we have role and data_dir *)
  let daily_logs_dir =
    match (role.value, data_dir.value, base_dir.value) with
    | Some detected_role, Some detected_data_dir, base_dir_opt ->
        let detected_base_dir = Option.value ~default:"" base_dir_opt in
        detect_daily_logs_dir
          ~role:detected_role
          ~data_dir:detected_data_dir
          ~base_dir:detected_base_dir
    | _ -> None
  in

  (* Build detected config *)
  let config =
    External_service.
      {
        unit_name = Printf.sprintf "process-%d" proc.pid;
        unit_file_path = None;
        exec_start = cmdline;
        unit_state;
        user = proc.user;
        group = None;
        working_dir = None;
        environment_files = [];
        role;
        binary_path = detected ~source:"cmdline" binary_for_version;
        binary_version = unknown ();
        data_dir;
        rpc_addr;
        net_addr = unknown ();
        network;
        history_mode = unknown ();
        node_endpoint;
        base_dir;
        delegates = unknown ();
        dal_endpoint = unknown ();
        daily_logs_dir;
        extra_args = [];
        parse_warnings = [];
      }
  in

  (* Generate instance name *)
  let suggested_instance_name =
    External_service.suggest_instance_name ~unit_name:config.unit_name
  in

  External_service.{config; suggested_instance_name}

(** Detect Octez processes running outside systemd *)
let detect_standalone_processes () =
  try
    let standalone_procs = Process_scanner.get_standalone_processes () in
    List.map process_to_external_service standalone_procs
  with _e -> []

let detect () =
  try
    (* List all service units *)
    let* all_units = list_all_service_units () in

    (* Filter and process each unit *)
    let external_services =
      List.filter_map
        (fun unit_name ->
          (* Skip managed units *)
          if is_managed_unit_name unit_name && is_in_registry ~unit_name then
            None
          else
            (* Get ExecStart *)
            match get_exec_start ~unit_name with
            | Some exec_start when contains_octez_binary exec_start ->
                (* Get additional properties *)
                let properties =
                  get_unit_properties
                    ~unit_name
                    ~props:
                      [
                        "User";
                        "Group";
                        "WorkingDirectory";
                        "ActiveState";
                        "SubState";
                        "UnitFileState";
                        "EnvironmentFile";
                      ]
                in
                Some (build_external_service ~unit_name ~exec_start ~properties)
            | _ -> None)
        all_units
    in

    (* Infer networks from connected nodes *)
    let enriched_services = infer_network_from_endpoint external_services in

    (* Detect standalone processes (not managed by systemd) *)
    let standalone_services = detect_standalone_processes () in

    (* Combine systemd services and standalone processes *)
    let all_services = enriched_services @ standalone_services in

    (* Update cache *)
    Mutex.protect cache_lock (fun () -> cache := all_services) ;

    Ok all_services
  with e ->
    let msg = Printf.sprintf "Detection failed: %s" (Printexc.to_string e) in
    Error msg
