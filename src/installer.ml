(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

open Rresult
open Installer_types
open Helpers
open Snapshot
open Config
open Dal_node

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

let install_accuser ?(quiet = false) (request : accuser_request) =
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
    | _ -> Common.default_role_dir "accuser" request.instance
  in
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
        role = "accuser";
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
            ("OCTEZ_CLIENT_BASE_DIR", base_dir);
            ("OCTEZ_NODE_ENDPOINT", node_endpoint);
            ( "OCTEZ_NODE_INSTANCE",
              match node_mode with
              | Local svc -> svc.Service.instance
              | Remote _ -> "" );
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
  Ok service

let start_service ?quiet ~instance () =
  let* svc_opt = Service_registry.find ~instance in
  match svc_opt with
  | None -> R.error_msgf "Instance '%s' not found" instance
  | Some svc ->
      (* Check parent dependency is running *)
      let* () =
        match svc.depends_on with
        | None -> Ok ()
        | Some parent_instance -> (
            match Service_registry.find ~instance:parent_instance with
            | Ok (Some parent) -> (
                match
                  Systemd.is_active ~role:parent.role ~instance:parent_instance
                with
                | Ok true -> Ok ()
                | Ok false ->
                    R.error_msgf
                      "Cannot start %s: dependency '%s' is not running.\n\
                       Start it first with: octez-manager instance %s start"
                      instance
                      parent_instance
                      parent_instance
                | Error _ ->
                    R.error_msgf
                      "Cannot start %s: dependency '%s' is not running.\n\
                       Start it first with: octez-manager instance %s start"
                      instance
                      parent_instance
                      parent_instance)
            | _ ->
                (* Parent not found in registry, skip check *)
                Ok ())
      in
      Systemd.start ?quiet ~role:svc.role ~instance ()

let stop_service_cascade ?quiet ~instance () =
  let* svc_opt = Service_registry.find ~instance in
  match svc_opt with
  | None -> R.error_msgf "Instance '%s' not found" instance
  | Some svc ->
      (* Stop dependents first *)
      let* () =
        if svc.dependents <> [] then (
          if not (Option.value ~default:false quiet) then
            Printf.printf
              "Stopping dependents: %s\n"
              (String.concat ", " svc.dependents) ;
          List.fold_left
            (fun acc dep ->
              let* () = acc in
              (* Silently ignore missing dependents during cascade *)
              match Service_registry.find ~instance:dep with
              | Ok (Some dep_svc) ->
                  Systemd.stop ?quiet ~role:dep_svc.role ~instance:dep ()
              | _ -> Ok ())
            (Ok ())
            svc.dependents)
        else Ok ()
      in
      Systemd.stop ?quiet ~role:svc.role ~instance ()

let stop_service ?quiet ~instance () = stop_service_cascade ?quiet ~instance ()

let get_stopped_dependencies ~instance () =
  let* svc_opt = Service_registry.find ~instance in
  match svc_opt with
  | None -> R.error_msgf "Instance '%s' not found" instance
  | Some _svc ->
      (* Collect all stopped parent dependencies *)
      let rec collect_deps acc inst =
        match Service_registry.find ~instance:inst with
        | Ok (Some s) -> (
            match s.depends_on with
            | None -> Ok acc
            | Some parent_inst -> (
                match Service_registry.find ~instance:parent_inst with
                | Ok (Some parent) -> (
                    match
                      Systemd.is_active ~role:parent.role ~instance:parent_inst
                    with
                    | Ok true -> collect_deps acc parent_inst
                    | Ok false | Error _ ->
                        (* Parent is stopped, add it and check its dependencies *)
                        collect_deps (parent :: acc) parent_inst)
                | _ -> Ok acc))
        | _ -> Ok acc
      in
      let* deps = collect_deps [] instance in
      (* Return in order: topmost parent first *)
      Ok (List.rev deps)

let get_stopped_dependents ~instance () =
  let* svc_opt = Service_registry.find ~instance in
  match svc_opt with
  | None -> R.error_msgf "Instance '%s' not found" instance
  | Some svc ->
      (* Collect all stopped dependents *)
      let stopped =
        List.filter_map
          (fun dep_inst ->
            match Service_registry.find ~instance:dep_inst with
            | Ok (Some dep) -> (
                match Systemd.is_active ~role:dep.role ~instance:dep_inst with
                | Ok true -> None
                | Ok false | Error _ -> Some dep)
            | _ -> None)
          svc.dependents
      in
      Ok stopped

let restart_service ?quiet ~instance () =
  let* svc_opt = Service_registry.find ~instance in
  match svc_opt with
  | Some svc -> Systemd.restart ?quiet ~role:svc.role ~instance ()
  | None -> R.error_msgf "Instance '%s' not found" instance

let remove_service ?(quiet = false) ~delete_data_dir ~instance () =
  let* svc_opt = Service_registry.find ~instance in
  match svc_opt with
  | None -> R.error_msgf "Instance '%s' not found" instance
  | Some svc ->
      (* Stop dependents first (cascade stop) *)
      let* () =
        if svc.dependents <> [] then (
          if not quiet then
            Format.printf
              "Stopping dependents: %s@."
              (String.concat ", " svc.dependents) ;
          List.fold_left
            (fun acc dep ->
              let* () = acc in
              match Service_registry.find ~instance:dep with
              | Ok (Some dep_svc) ->
                  Systemd.stop ~quiet ~role:dep_svc.role ~instance:dep ()
              | _ -> Ok ())
            (Ok ())
            svc.dependents)
        else Ok ()
      in
      (* Unregister from parent's dependents list *)
      let* () =
        match svc.depends_on with
        | None -> Ok ()
        | Some parent_instance -> (
            match Service_registry.find ~instance:parent_instance with
            | Ok (Some parent) ->
                let updated_deps =
                  List.filter (( <> ) instance) parent.dependents
                in
                let updated_parent = {parent with dependents = updated_deps} in
                Service_registry.write updated_parent
            | _ -> Ok ())
      in
      let* () =
        Systemd.disable ~quiet ~role:svc.role ~instance ~stop_now:true ()
      in
      Systemd.remove_dropin ~role:svc.role ~instance ;
      let* () =
        match delete_data_dir with
        | true -> Common.remove_tree svc.data_dir
        | false -> Ok ()
      in
      let* () = Service_registry.remove ~instance in
      let* services = Service_registry.list () in
      Systemd.sync_logrotate (logrotate_specs_of services)

let purge_service ?(quiet = false) ~prompt_yes_no ~instance () =
  let* svc_opt = Service_registry.find ~instance in
  match svc_opt with
  | None -> R.error_msgf "Instance '%s' not found" instance
  | Some svc ->
      let* () = remove_service ~quiet ~delete_data_dir:true ~instance () in
      let* () =
        let is_baker = svc.role = "baker" in
        let is_accuser = svc.role = "accuser" in
        if is_baker || is_accuser then
          let env =
            match Node_env.read ~inst:svc.instance with
            | Ok pairs -> pairs
            | Error _ -> []
          in
          let base_dir =
            List.assoc
              (if is_baker then "OCTEZ_BAKER_BASE_DIR"
               else "OCTEZ_CLIENT_BASE_DIR")
              env
          in
          let remove_base_dir =
            prompt_yes_no
              (Format.sprintf "Purge base-dir %S?" base_dir)
              ~default:false
          in
          if remove_base_dir then Common.remove_tree base_dir else Ok ()
        else Ok ()
      in
      (* Also remove per-instance env files under XDG_CONFIG_HOME or /etc when purging *)
      let env_dir =
        Filename.concat (Common.env_instances_base_dir ()) instance
      in
      let _ =
        (* Best-effort: don't fail purge if env removal fails *)
        match Common.remove_tree env_dir with
        | Ok () -> ()
        | Error _ -> ()
      in
      let* () = remove_logging_artifacts svc.logging_mode in
      let* remaining = Service_registry.list () in
      if
        should_drop_service_user
          ~user:svc.service_user
          ~remaining_services:remaining
      then System_user.remove_service_account ~quiet ~name:svc.service_user ()
      else Ok ()

let list_services () = Service_registry.list ()

(** Clean up old instance after rename.
    Removes service registry entry and systemd dropin but preserves data. *)
let cleanup_renamed_instance ?(quiet = false) ~old_instance ~new_instance () =
  let* svc_opt = Service_registry.find ~instance:old_instance in
  match svc_opt with
  | None -> Ok () (* Old instance already gone, nothing to clean *)
  | Some old_svc ->
      (* Stop the old service if still running *)
      let _ = Systemd.stop ~role:old_svc.role ~instance:old_instance () in
      (* Disable old service *)
      let* () =
        Systemd.disable
          ~quiet
          ~role:old_svc.role
          ~instance:old_instance
          ~stop_now:true
          ()
      in
      (* Remove old dropin *)
      Systemd.remove_dropin ~role:old_svc.role ~instance:old_instance ;
      (* Update dependents to point to new instance *)
      let* () =
        List.fold_left
          (fun acc dep_inst ->
            let* () = acc in
            (* Update OCTEZ_NODE_INSTANCE and OCTEZ_DAL_INSTANCE in dependent's env file *)
            let* () =
              match Node_env.read ~inst:dep_inst with
              | Ok pairs ->
                  let updated_pairs =
                    List.map
                      (fun (k, v) ->
                        if
                          (k = "OCTEZ_NODE_INSTANCE" || k = "OCTEZ_DAL_INSTANCE")
                          && String.trim v = old_instance
                        then (k, new_instance)
                        else (k, v))
                      pairs
                  in
                  Node_env.write_pairs ~inst:dep_inst updated_pairs
              | Error _ -> Ok () (* Skip if can't read env *)
            in
            (* Update depends_on in dependent's service registry entry *)
            let* () =
              match Service_registry.find ~instance:dep_inst with
              | Ok (Some dep_svc)
                when dep_svc.Service.depends_on = Some old_instance ->
                  let updated_dep =
                    {dep_svc with depends_on = Some new_instance}
                  in
                  Service_registry.write updated_dep
              | _ -> Ok ()
            in
            (* Regenerate systemd dropin for dependent with new parent reference *)
            match Service_registry.find ~instance:dep_inst with
            | Ok (Some dep_svc) ->
                Systemd.write_dropin
                  ~role:dep_svc.Service.role
                  ~inst:dep_inst
                  ~data_dir:dep_svc.Service.data_dir
                  ~logging_mode:dep_svc.Service.logging_mode
                  ~extra_paths:[]
                  ~depends_on:(old_svc.role, new_instance)
                  ()
            | _ -> Ok ())
          (Ok ())
          old_svc.dependents
      in
      (* Update parent's dependents list: replace old instance name with new *)
      let* () =
        match old_svc.depends_on with
        | None -> Ok ()
        | Some parent_inst -> (
            match Service_registry.find ~instance:parent_inst with
            | Ok (Some parent) ->
                let updated_deps =
                  parent.dependents
                  |> List.filter (( <> ) old_instance)
                  |> List.cons new_instance
                  |> List.sort_uniq String.compare
                in
                let updated_parent = {parent with dependents = updated_deps} in
                Service_registry.write updated_parent
            | _ -> Ok ())
      in
      (* Also update DAL node's dependents list if service uses DAL *)
      let* () =
        match Node_env.read ~inst:old_instance with
        | Ok pairs -> (
            let dal_inst =
              List.assoc_opt "OCTEZ_DAL_INSTANCE" pairs
              |> Option.map String.trim |> Option.value ~default:""
            in
            if dal_inst = "" then Ok ()
            else
              match Service_registry.find ~instance:dal_inst with
              | Ok (Some dal_svc) ->
                  let updated_deps =
                    dal_svc.dependents
                    |> List.filter (( <> ) old_instance)
                    |> List.cons new_instance
                    |> List.sort_uniq String.compare
                  in
                  let updated_dal = {dal_svc with dependents = updated_deps} in
                  Service_registry.write updated_dal
              | _ -> Ok ())
        | Error _ -> Ok ()
      in
      (* Transfer dependents to new service (deduplicate) *)
      let* new_svc_opt = Service_registry.find ~instance:new_instance in
      let* () =
        match new_svc_opt with
        | Some new_svc ->
            let merged_deps =
              old_svc.dependents @ new_svc.dependents
              |> List.sort_uniq String.compare
            in
            let updated_new = {new_svc with dependents = merged_deps} in
            Service_registry.write updated_new
        | None -> Ok ()
      in
      (* Remove old registry entry *)
      let* () = Service_registry.remove ~instance:old_instance in
      (* Remove old env files directory *)
      let old_env_dir =
        Filename.concat (Common.env_instances_base_dir ()) old_instance
      in
      let _ = Common.remove_tree old_env_dir in
      (* Sync logrotate *)
      let* services = Service_registry.list () in
      Systemd.sync_logrotate (logrotate_specs_of services)

let cleanup_dependencies () =
  let* services = Service_registry.list () in
  let all_instances =
    List.map (fun svc -> svc.Service.instance) services
    |> List.sort_uniq String.compare
  in
  let is_valid_dependent dep = List.mem dep all_instances in
  let updates =
    List.filter_map
      (fun svc ->
        let valid_deps =
          List.filter is_valid_dependent svc.Service.dependents
        in
        let stale_deps =
          List.filter (fun d -> not (is_valid_dependent d)) svc.dependents
        in
        if stale_deps <> [] then Some (svc, valid_deps, stale_deps) else None)
      services
  in
  let* cleaned_count =
    List.fold_left
      (fun acc (svc, valid_deps, stale_deps) ->
        let* count = acc in
        Printf.printf
          "Cleaning %s: removing stale dependents: %s\n"
          svc.Service.instance
          (String.concat ", " stale_deps) ;
        let updated_svc : Service.t = {svc with dependents = valid_deps} in
        let* () = Service_registry.write updated_svc in
        Ok (count + List.length stale_deps))
      (Ok 0)
      updates
  in
  Ok cleaned_count

let find_orphan_directories () =
  let* services = Service_registry.list () in
  let registered_dirs =
    List.map (fun svc -> svc.Service.data_dir) services
    |> List.sort_uniq String.compare
  in
  (* Base directories to scan for orphans *)
  let octez_data_base =
    if Common.is_root () then "/var/lib/octez"
    else Filename.concat (Common.xdg_data_home ()) "octez"
  in
  let octez_log_base =
    if Common.is_root () then "/var/log/octez"
    else Filename.concat (Common.xdg_state_home ()) "octez/logs"
  in
  let list_subdirs dir =
    if Sys.file_exists dir && Sys.is_directory dir then
      Sys.readdir dir |> Array.to_list
      |> List.map (fun name -> Filename.concat dir name)
      |> List.filter Sys.is_directory
    else []
  in
  let list_files dir =
    if Sys.file_exists dir && Sys.is_directory dir then
      Sys.readdir dir |> Array.to_list
      |> List.map (fun name -> Filename.concat dir name)
      |> List.filter (fun p -> Sys.file_exists p && not (Sys.is_directory p))
    else []
  in
  (* Find orphan data directories *)
  let all_data_dirs = list_subdirs octez_data_base in
  let orphan_data_dirs =
    List.filter (fun d -> not (List.mem d registered_dirs)) all_data_dirs
  in
  (* Find orphan log files *)
  let orphan_log_files = list_files octez_log_base in
  Ok (orphan_data_dirs, orphan_log_files)

let cleanup_orphans ~dry_run =
  let* orphan_dirs, orphan_logs = find_orphan_directories () in
  let removed = ref [] in
  let errors = ref [] in
  let process_path path =
    if dry_run then removed := path :: !removed
    else
      match Common.remove_tree path with
      | Ok () -> removed := path :: !removed
      | Error (`Msg msg) -> errors := (path, msg) :: !errors
  in
  List.iter process_path orphan_dirs ;
  List.iter process_path orphan_logs ;
  Ok (List.rev !removed, List.rev !errors)

let backup_file_if_exists_for_tests = backup_file_if_exists

let restore_backup_for_tests = restore_backup

module For_tests = struct
  type nonrec file_backup = file_backup

  let validate_instance_name_chars = validate_instance_name_chars

  let validate_instance_name_unique = validate_instance_name_unique

  let validate_instance_name = validate_instance_name

  let ensure_logging_base_directory = ensure_logging_base_directory

  let remove_logging_artifacts = remove_logging_artifacts

  let should_drop_service_user = should_drop_service_user

  let backup_file_if_exists ~path = backup_file_if_exists_for_tests path

  let restore_backup ~owner ~group backup =
    restore_backup_for_tests ~owner ~group backup

  let normalize_data_dir = normalize_data_dir

  let build_run_args = build_run_args

  let snapshot_plan_of_request = snapshot_plan_of_request

  let snapshot_metadata_of_plan = snapshot_metadata_of_plan

  let strip_file_uri = strip_file_uri

  let is_http_url = is_http_url

  let is_file_uri = is_file_uri

  let resolve_snapshot_download = resolve_snapshot_download

  let history_mode_matches = history_mode_matches

  let known_baker_global_options = known_baker_global_options

  let split_baker_extra_args = split_baker_extra_args
end
