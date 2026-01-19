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
          let length = in_channel_length ic in
          Format.eprintf
            "[DEBUG] /proc/%d/cmdline: file length = %d bytes@."
            pid
            length ;
          let content = really_input_string ic length in
          Format.eprintf
            "[DEBUG] /proc/%d/cmdline: read %d bytes@."
            pid
            (String.length content) ;
          (* cmdline is null-separated, convert to spaces *)
          let cmdline =
            String.map (fun c -> if c = '\000' then ' ' else c) content
          in
          let trimmed = String.trim cmdline in
          Format.eprintf
            "[DEBUG] /proc/%d/cmdline: after trim length = %d@."
            pid
            (String.length trimmed) ;
          if String.length trimmed > 0 then
            Format.eprintf
              "[DEBUG] /proc/%d/cmdline: first 80 chars = %s@."
              pid
              (String.sub trimmed 0 (min 80 (String.length trimmed)))
          else
            Format.eprintf "[DEBUG] /proc/%d/cmdline: EMPTY after trim!@." pid ;
          Some trimmed)
    with
    | Sys_error msg ->
        Format.eprintf "[DEBUG] /proc/%d/cmdline: Sys_error: %s@." pid msg ;
        None
    | End_of_file ->
        Format.eprintf "[DEBUG] /proc/%d/cmdline: End_of_file@." pid ;
        None

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
      Format.eprintf
        "[DEBUG] %s: MainPID query returned: '%s'@."
        unit_name
        trimmed ;
      match int_of_string_opt trimmed with
      | Some pid when pid > 0 ->
          Format.eprintf "[DEBUG] %s: Reading /proc/%d/cmdline@." unit_name pid ;
          read_proc_cmdline pid
      | _ ->
          Format.eprintf "[DEBUG] %s: Invalid PID: %s@." unit_name trimmed ;
          None)
  | Error _ ->
      Format.eprintf "[DEBUG] %s: MainPID query failed@." unit_name ;
      None

(** {1 Detection Logic} *)

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
    if active_state = "active" then (
      Format.eprintf
        "[DEBUG] %s: ActiveState=active, trying /proc/PID/cmdline@."
        unit_name ;
      match get_running_command ~unit_name with
      | Some running_cmd ->
          Format.eprintf
            "[DEBUG] %s: Got running command: %s@."
            unit_name
            (String.sub running_cmd 0 (min 80 (String.length running_cmd))) ;
          (running_cmd, "/proc/PID/cmdline")
      | None ->
          Format.eprintf
            "[DEBUG] %s: Failed to get running command, using ExecStart@."
            unit_name ;
          (exec_start, "ExecStart"))
    else (
      Format.eprintf
        "[DEBUG] %s: ActiveState=%s, using ExecStart@."
        unit_name
        active_state ;
      (exec_start, "ExecStart"))
  in

  (* Parse command line *)
  Format.eprintf
    "[DEBUG] %s: Parsing command from %s@."
    unit_name
    command_source ;
  let parsed = Execstart_parser.parse command_to_parse in
  Format.eprintf
    "[DEBUG] %s: Parsed binary_path=%s data_dir=%s@."
    unit_name
    (match parsed.binary_path with Some b -> b | None -> "NONE")
    (match parsed.data_dir with Some d -> d | None -> "NONE") ;

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
        let role = External_service.role_of_binary_name binary in
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
    match (parsed_network.value, rpc_addr_field.value, active_state) with
    | None, Some rpc_addr, "active" -> (
        (* No network but have RPC and service is active - try probe *)
        match probe_rpc_chain_id rpc_addr with
        | Some (_chain_id, Some network_name) ->
            External_service.inferred ~source:"RPC probe" network_name
        | _ -> parsed_network)
    | _ -> parsed_network
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
      extra_args = parsed.extra_args;
      parse_warnings = parsed.warnings;
    }
  in

  let suggested_instance_name =
    External_service.suggest_instance_name ~unit_name
  in

  {External_service.config; suggested_instance_name}

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

    (* Update cache *)
    Mutex.protect cache_lock (fun () -> cache := external_services) ;

    Ok external_services
  with e ->
    let msg = Printf.sprintf "Detection failed: %s" (Printexc.to_string e) in
    Error msg
