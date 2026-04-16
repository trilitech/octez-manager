(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Rresult

let ( let* ) = Result.bind

(** {1 Helpers} *)

(** {1 Cache} *)

let cache : External_service.t list ref = ref []

let cache_lock = Mutex.create ()

let get_cached () = Mutex.protect cache_lock (fun () -> !cache)

let clear_cache () = Mutex.protect cache_lock (fun () -> cache := [])

(** {1 Filtering} *)

(** Check if unit name matches octez-manager's naming convention.
    Patterns: 
    - octez-<role>@<instance>.service (for node, baker, accuser, dal-node)
    - signatory@<instance>.service (for signatory) *)
let is_managed_unit_name unit_name =
  (* Check for signatory@ pattern *)
  if String.starts_with ~prefix:"signatory@" unit_name then
    (* Must end with .service *)
    String.ends_with ~suffix:".service" unit_name
  else if String.starts_with ~prefix:"octez-" unit_name then
    (* Must contain exactly one @ symbol *)
    match String.split_on_char '@' unit_name with
    | [_role_part; instance_part] ->
        (* instance_part should end with .service *)
        String.ends_with ~suffix:".service" instance_part
    | _ -> false
  else false

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

let list_all_service_units () =
  (* List all service units from both loaded units and unit files.
     This ensures we detect both running services and newly created ones. *)
  let list_loaded_units () =
    let cmd =
      Systemd.systemctl_cmd ()
      @ ["list-units"; "--type=service"; "--all"; "--no-legend"; "--no-pager"]
    in
    match Cmd_runner.run_out cmd with
    | Ok output ->
        let lines = String.split_on_char '\n' output in
        List.filter_map
          (fun line ->
            let trimmed = String.trim line in
            if trimmed = "" then None
            else
              (* Line format: "[●] unit.service   loaded   active   running   Description" *)
              (* Failed services have a ● bullet prefix, skip it *)
              (* Extract first field that ends with .service *)
              let fields =
                String.split_on_char ' ' trimmed
                |> List.filter (fun s -> s <> "")
              in
              match
                List.find_opt (String.ends_with ~suffix:".service") fields
              with
              | Some unit_name -> Some unit_name
              | None -> None)
          lines
    | Error _ -> []
  in
  let list_unit_files () =
    (* Query both octez-* and signatory@ patterns *)
    let query_pattern pattern =
      let cmd =
        Systemd.systemctl_cmd ()
        @ [
            "list-unit-files";
            "--type=service";
            pattern;
            "--no-legend";
            "--no-pager";
          ]
      in
      match Cmd_runner.run_out cmd with
      | Ok output ->
          let lines = String.split_on_char '\n' output in
          List.filter_map
            (fun line ->
              let trimmed = String.trim line in
              if trimmed = "" then None
              else
                (* Line format: "unit.service   enabled/disabled/static" *)
                (* Extract first field (unit name) *)
                match String.split_on_char ' ' trimmed with
                | unit_name :: _
                  when String.ends_with ~suffix:".service" unit_name
                       (* Skip template units (ending with @.service) *)
                       && not (String.ends_with ~suffix:"@.service" unit_name)
                  ->
                    Some unit_name
                | _ -> None)
            lines
      | Error _ -> []
    in
    (* Query both patterns and combine results *)
    query_pattern "octez-*.service" @ query_pattern "signatory@*.service"
  in
  (* Get loaded units and unit files *)
  let loaded = list_loaded_units () in
  let files = list_unit_files () in

  (* Create a set of unit files for fast lookup *)
  let file_set = List.fold_left (fun acc unit -> unit :: acc) [] files in

  (* Helper to check if a unit has its own file (not just using a template) *)
  let has_own_unit_file unit_name =
    (* First check if it's in our file list *)
    if List.mem unit_name file_set then true
    else
      (* For services not matching octez-* pattern, check FragmentPath
         to see if they have their own file vs using a template *)
      match
        Cmd_runner.run_out
          (Systemd.systemctl_cmd () @ ["show"; unit_name; "-p"; "FragmentPath"])
      with
      | Ok output ->
          let path = String.trim output in
          (* FragmentPath format: "FragmentPath=/path/to/service.service" *)
          if String.starts_with ~prefix:"FragmentPath=" path then
            let file_path =
              String.sub path 13 (String.length path - 13)
              (* skip "FragmentPath=" *)
            in
            (* Check if the path matches the unit name (not a template)
               Template: /path/octez-node@.service
               Instance: /path/octez-node@instance.service or /path/test.service *)
            String.ends_with ~suffix:unit_name file_path
          else false
      | Error _ -> false
  in

  (* Filter loaded units to only include those with their own unit files
     This prevents purged services (still in systemd memory, using templates) from appearing *)
  let loaded_with_files = List.filter has_own_unit_file loaded in

  (* Combine and deduplicate *)
  let all_units = loaded_with_files @ files in
  let unique_units =
    List.fold_left
      (fun acc unit -> if List.mem unit acc then acc else unit :: acc)
      []
      all_units
  in
  Ok (List.rev unique_units)

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
    Systemd.systemctl_cmd () @ ["show"; unit_name; "-p"; "ExecStart"; "--value"]
  in
  match Cmd_runner.run_out cmd with
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
  let cmd = Systemd.systemctl_cmd () @ ["show"; unit_name] in
  match Cmd_runner.run_out cmd with
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
  let cmd = Systemd.systemctl_cmd () @ ["cat"; unit_name] in
  match Cmd_runner.run_out cmd with
  | Ok content -> Ok content
  | Error (`Msg msg) ->
      let msg_lower = String.lowercase_ascii msg in
      if String_utils.string_contains ~needle:"permission denied" msg_lower then
        Error `Permission_denied
      else Error (`Error msg)

(** {1 Network Detection} *)

(** Known Tezos network chain IDs *)
let chain_id_to_network = function
  | "NetXdQprcVkpaWU" -> Some "mainnet"
  | "NetXsqzbfFenSTS" -> Some "shadownet"
  | "NetXe8DbhW9A1eS" -> Some "tallinnnet"
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
  match Cmd_runner.run_out cmd with
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

(** Try to infer network from data_dir/config.json.
    Returns Some network_name if the config file exists and contains a valid network.
    This is used as a fallback when RPC is not accessible (e.g., stopped nodes). *)
let probe_network_from_config data_dir =
  let config_path = Filename.concat data_dir "config.json" in
  if not (Sys.file_exists config_path) then None
  else
    try
      let json = Yojson.Safe.from_file config_path in
      let open Yojson.Safe.Util in
      (* Network can be:
         - Missing/null (mainnet): no network field
         - String (built-in networks): {"network": "shadownet"}
         - Object (custom networks): {"network": {"chain_name": "TEZOS_..."}} *)
      match member "network" json with
      | `Null ->
          (* No network field means mainnet *)
          Some "mainnet"
      | `String network_alias ->
          (* Built-in network: {"network": "shadownet"} *)
          Some network_alias
      | `Assoc _ as network_obj -> (
          (* Custom network: {"network": {"chain_name": "TEZOS_..."}} *)
          match member "chain_name" network_obj with
          | `String chain_name -> (
              match Teztnets.resolve_network_from_node_chain chain_name with
              | Ok network -> Some network.alias
              | Error _ -> None)
          | _ -> None)
      | _ -> None
    with _ -> None

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
    Systemd.systemctl_cmd () @ ["show"; unit_name; "-p"; "MainPID"; "--value"]
  in
  match Cmd_runner.run_out cmd with
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
  | External_service.Index ->
      (* Index: <data_dir>/daily_logs/ *)
      check_dir (Filename.concat data_dir "daily_logs")
  | External_service.Signatory ->
      (* Signatory: <base_dir>/logs/signatory/ *)
      let base = if base_dir <> "" then base_dir else data_dir in
      check_dir (Filename.concat (Filename.concat base "logs") "signatory")
  | External_service.Unknown _ ->
      (* Unknown role: try generic daily_logs *)
      check_dir (Filename.concat data_dir "daily_logs")

(** Check if ExecStart contains an octez binary *)
let contains_octez_binary exec_start =
  let lower = String.lowercase_ascii exec_start in
  String_utils.string_contains ~needle:"octez-node" lower
  || String_utils.string_contains ~needle:"octez-baker" lower
  || String_utils.string_contains ~needle:"octez-accuser" lower
  || String_utils.string_contains ~needle:"octez-dal-node" lower
  || String_utils.string_contains ~needle:"tezos-baker" lower
  || String_utils.string_contains ~needle:"tezos-accuser" lower

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

  (* Helper to check if a string contains unexpanded variables *)
  let contains_unexpanded_vars value =
    (* Check for ${VAR} or $VAR patterns *)
    try
      let idx = String.index value '$' in
      (* Found a $, check if it looks like a variable *)
      if idx + 1 < String.length value then
        let next_char = value.[idx + 1] in
        match next_char with
        | '{' -> true (* ${VAR} format *)
        | 'A' .. 'Z' | 'a' .. 'z' | '_' -> true (* $VAR format *)
        | _ -> false (* Just a lone $ *)
      else false
    with Not_found -> false
  in

  (* Parse environment files from systemd property value.
     Handles multiple formats:
     - Space-separated: "/etc/foo /etc/bar"
     - Semicolon-separated: "/etc/foo;/etc/bar"
     - Optional files with - prefix: "-/etc/optional"
     - Mixed: "/etc/foo;-/etc/bar /etc/baz" *)
  let parse_environment_files files_str =
    let split_on_delimiters str =
      (* Split on space, semicolon, or newline *)
      let delimiters = [' '; ';'; '\n'; '\t'] in
      let rec split acc current = function
        | [] -> if current = "" then List.rev acc else List.rev (current :: acc)
        | c :: rest ->
            if List.mem c delimiters then
              if current = "" then split acc "" rest
              else split (current :: acc) "" rest
            else split acc (current ^ String.make 1 c) rest
      in
      split [] "" (List.init (String.length str) (String.get str))
    in
    split_on_delimiters files_str |> List.filter (fun s -> String.trim s <> "")
  in

  (* Get environment files *)
  let environment_files =
    match List.assoc_opt "EnvironmentFiles" properties with
    | Some files_str ->
        (* Strip any (ignore_errors=...) suffix that systemd adds *)
        let cleaned =
          match String.index_opt files_str '(' with
          | Some idx -> String.sub files_str 0 idx |> String.trim
          | None -> files_str
        in
        parse_environment_files cleaned
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
    if List.length parsed.warnings > 0 && List.length environment_files > 0 then (
      (* Try to read env files to expand variables *)
      Logs.debug (fun m ->
          m
            "Attempting to read %d environment file(s) to expand variables"
            (List.length environment_files)) ;
      let result =
        List.fold_left
          (fun acc file_path ->
            (* Handle optional files (prefixed with -) *)
            let is_optional = String.starts_with ~prefix:"-" file_path in
            let actual_path =
              if is_optional then
                String.sub file_path 1 (String.length file_path - 1)
              else file_path
            in
            match Env_file_parser.parse_file actual_path with
            | Ok pairs ->
                Logs.debug (fun m ->
                    m
                      "Read %d variable(s) from environment file: %s"
                      (List.length pairs)
                      actual_path) ;
                acc @ pairs
            | Error msg ->
                if is_optional then
                  Logs.debug (fun m ->
                      m "Optional environment file not found: %s" actual_path)
                else
                  Logs.warn (fun m ->
                      m "Failed to read environment file %s: %s" actual_path msg) ;
                acc)
          []
          environment_files
      in
      result)
    else []
  in

  (* Helper to build a field, handling variable expansion *)
  let build_field field_name parsed_value =
    match parsed_value with
    | None -> External_service.unknown ()
    | Some value ->
        if contains_unexpanded_vars value then
          (* Value contains variables that need expansion *)
          if env_vars = [] then (
            (* No env vars available to expand - mark as unknown *)
            Logs.warn (fun m ->
                m
                  "Field %s contains unexpanded variable but no environment \
                   files available: %s"
                  field_name
                  value) ;
            External_service.unknown ())
          else
            (* Try to expand variables *)
            let expanded = Env_file_parser.expand_vars ~env:env_vars value in
            if String.equal expanded value || contains_unexpanded_vars expanded
            then (
              (* Expansion failed or incomplete - mark as unknown *)
              Logs.warn (fun m ->
                  m
                    "Failed to fully expand variables in field %s: %s"
                    field_name
                    value) ;
              External_service.unknown ())
            else (
              (* Expansion succeeded *)
              Logs.debug (fun m ->
                  m "Expanded %s: %s -> %s" field_name value expanded) ;
              External_service.inferred ~source:"EnvironmentFile" expanded)
        else
          (* No variables to expand, use value as-is *)
          External_service.detected ~source:command_source value
  in

  (* Build fields from parsed data *)
  let binary_field = build_field "binary_path" parsed.binary_path in

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

  let data_dir_field = build_field "data_dir" parsed.data_dir in
  let base_dir_field = build_field "base_dir" parsed.base_dir in
  let rpc_addr_field = build_field "rpc_addr" parsed.rpc_addr in
  let net_addr_field = build_field "net_addr" parsed.net_addr in
  let endpoint_field = build_field "endpoint" parsed.endpoint in
  let dal_endpoint_field = build_field "dal_endpoint" parsed.dal_endpoint in
  let history_mode_field = build_field "history_mode" parsed.history_mode in

  (* Try to detect network if not already known from command-line parsing *)
  let network_field =
    let parsed_network = build_field "network" parsed.network in
    match (parsed_network.value, role_field.value) with
    | None, Some External_service.Node -> (
        (* Network unknown for node - try fallbacks in priority order:
           1. Read from config.json (no network I/O, works for stopped nodes)
           2. RPC probe (only if service is active) *)
        let from_config =
          match data_dir_field.value with
          | Some data_dir -> probe_network_from_config data_dir
          | None -> None
        in
        match from_config with
        | Some network_name ->
            External_service.inferred ~source:"config.json" network_name
        | None -> (
            (* Fallback to RPC probe if service is active *)
            match (active_state, rpc_addr_field.value) with
            | "active", Some addr -> (
                match probe_rpc_chain_id addr with
                | Some (_chain_id, Some network_name) ->
                    External_service.inferred ~source:"RPC probe" network_name
                | _ -> parsed_network)
            | _ -> parsed_network))
    | None, Some role when active_state = "active" -> (
        (* For other roles (baker/accuser/dal/signatory), only try RPC probe if active *)
        let probe_addr =
          match role with
          | External_service.Baker | External_service.Accuser
          | External_service.Dal_node | External_service.Index ->
              (* Baker/Accuser/DAL/Index: probe their connected node's endpoint *)
              endpoint_field.value
          | External_service.Signatory ->
              (* Signatory: doesn't connect to node RPC *)
              None
          | External_service.Unknown _ -> (
              (* Unknown: try rpc_addr first, then endpoint *)
              match rpc_addr_field.value with
              | Some addr -> Some addr
              | None -> endpoint_field.value)
          | External_service.Node ->
              (* Already handled above *)
              None
        in
        match probe_addr with
        | Some addr -> (
            match probe_rpc_chain_id addr with
            | Some (_chain_id, Some network_name) ->
                External_service.inferred ~source:"RPC probe" network_name
            | _ -> parsed_network)
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
      dal_endpoint = dal_endpoint_field;
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
      else if Str.string_match (Str.regexp ".* run accuser\\b") cmdline 0 then
        Some "accuser"
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
  let dal_endpoint =
    match parsed.dal_endpoint with
    | Some d -> External_service.detected ~source:"cmdline" d
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

  (* Try to detect network via RPC probe for active processes *)
  let network_field =
    match (network.value, role.value) with
    | None, Some detected_role -> (
        (* Unknown network - try RPC probe based on role *)
        let probe_addr =
          match detected_role with
          | External_service.Node ->
              (* Nodes: probe their own RPC endpoint *)
              rpc_addr.value
          | External_service.Baker | External_service.Accuser
          | External_service.Dal_node | External_service.Index ->
              (* Baker/Accuser/DAL/Index: probe their connected node's endpoint *)
              node_endpoint.value
          | External_service.Signatory ->
              (* Signatory: doesn't connect to node RPC *)
              None
          | External_service.Unknown _ -> (
              (* Unknown: try rpc_addr first, then endpoint *)
              match rpc_addr.value with
              | Some addr -> Some addr
              | None -> node_endpoint.value)
        in
        match probe_addr with
        | Some addr -> (
            match probe_rpc_chain_id addr with
            | Some (_chain_id, Some network_name) ->
                External_service.inferred ~source:"RPC probe" network_name
            | _ -> network)
        | None -> network)
    | _ -> network
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
        network = network_field;
        history_mode = unknown ();
        node_endpoint;
        base_dir;
        delegates = unknown ();
        dal_endpoint;
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
          (* Skip managed units that are already in registry *)
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
                        "EnvironmentFiles";
                      ]
                in
                Some (build_external_service ~unit_name ~exec_start ~properties)
            | _ -> None)
        all_units
    in

    (* Filter out external services whose instance name matches a managed instance *)
    let external_services =
      List.filter
        (fun (svc : External_service.t) ->
          let instance = svc.suggested_instance_name in
          match Service_registry.find ~instance with
          | Ok (Some _) ->
              false (* Instance is managed, don't show as external *)
          | Ok None -> true (* Not managed, show as external *)
          | Error _ -> true (* Error reading registry, show as external *))
        external_services
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

(** {1 Testing Utilities} *)

module For_tests = struct
  let string_contains = String_utils.string_contains

  let is_managed_unit_name = is_managed_unit_name

  let extract_command_from_systemd_format = extract_command_from_systemd_format

  let chain_id_to_network = chain_id_to_network

  let systemctl_cmd = Systemd.systemctl_cmd

  let contains_octez_binary = contains_octez_binary
end
