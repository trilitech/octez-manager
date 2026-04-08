(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

open Rresult
include Helpers
include Config

(** Check if a data directory is shared by multiple services.
    Returns true if other services (besides [instance]) use the same data_dir. *)
let is_shared_data_dir ~instance ~data_dir () =
  match Service_registry.list () with
  | Error _ -> false
  | Ok all_services ->
      List.exists
        (fun (svc : Service.t) ->
          svc.instance <> instance && svc.data_dir = data_dir)
        all_services

(** Check if a base directory is shared by multiple baker/accuser instances.
    Returns a list of instance names (excluding [instance]) that use the same base_dir.
    Empty list means no other instances use this base_dir. *)
let get_base_dir_users ~instance ~base_dir () =
  match Service_registry.list () with
  | Error _ -> []
  | Ok all_services ->
      List.filter_map
        (fun (svc : Service.t) ->
          if svc.instance = instance then None
          else
            match Node_env.read ~inst:svc.instance with
            | Error _ -> None
            | Ok pairs -> (
                let base_dir_key =
                  match svc.role with
                  | "baker" -> Some "OCTEZ_BAKER_BASE_DIR"
                  | "accuser" -> Some "OCTEZ_CLIENT_BASE_DIR"
                  | _ -> None
                in
                match base_dir_key with
                | None -> None
                | Some key -> (
                    match List.assoc_opt key pairs with
                    | Some bd when String.trim bd = base_dir ->
                        Some svc.instance
                    | _ -> None)))
        all_services

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
        | true ->
            if is_shared_data_dir ~instance ~data_dir:svc.data_dir () then (
              if not quiet then
                Format.printf
                  "⚠ Skipping data directory deletion (shared with other \
                   services): %s@."
                  svc.data_dir ;
              Ok ())
            else File_ops.remove_tree svc.data_dir
        | false -> Ok ()
      in
      Service_registry.remove ~instance

let purge_service ?(quiet = false) ~force_purge ~prompt_yes_no ~instance () =
  let* svc_opt = Service_registry.find ~instance in
  match svc_opt with
  | None -> R.error_msgf "Instance '%s' not found" instance
  | Some svc ->
      let* () = remove_service ~quiet ~delete_data_dir:true ~instance () in
      (* Clean up empty parent directory for signatory *)
      let* () =
        if svc.role = "signatory" then
          let parent_dir = Filename.dirname svc.data_dir in
          (* Only remove if it's the signatory parent dir and it's empty *)
          if
            Filename.basename parent_dir = "signatory"
            && Sys.file_exists parent_dir
          then
            try
              let entries = Sys.readdir parent_dir in
              if Array.length entries = 0 then (
                Unix.rmdir parent_dir ;
                Ok ())
              else Ok ()
            with Unix.Unix_error _ -> Ok ()
          else Ok ()
        else Ok ()
      in
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
          (* Check if other instances are using the same base-dir *)
          let other_users = get_base_dir_users ~instance ~base_dir () in
          if other_users <> [] then (
            (* Base-dir is shared - cannot delete *)
            if not quiet then
              Format.printf
                "⚠ Skipping base directory deletion (shared with other \
                 services): %s@.Used by: %s@."
                base_dir
                (String.concat ", " other_users) ;
            Ok ())
          else
            (* Base-dir not shared - proceed with deletion logic *)
            let remove_base_dir =
              if force_purge then true
              else
                prompt_yes_no
                  (Format.sprintf "Purge base-dir %S?" base_dir)
                  ~default:false
            in
            if remove_base_dir then File_ops.remove_tree base_dir else Ok ()
        else Ok ()
      in
      (* Also remove per-instance env files under XDG_CONFIG_HOME or /etc when purging *)
      let env_dir =
        Filename.concat (Paths.env_instances_base_dir ()) instance
      in
      let _ =
        (* Best-effort: don't fail purge if env removal fails *)
        match File_ops.remove_tree env_dir with
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
                  ~app_bin_dir:dep_svc.Service.app_bin_dir
                  ~depends_on:[(old_svc.role, new_instance)]
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
        Filename.concat (Paths.env_instances_base_dir ()) old_instance
      in
      let _ = File_ops.remove_tree old_env_dir in
      Ok ()

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
    if Paths.is_root () then "/var/lib/octez"
    else Filename.concat (Paths.xdg_data_home ()) "octez"
  in
  let octez_log_base =
    if Paths.is_root () then "/var/log/octez"
    else Filename.concat (Paths.xdg_state_home ()) "octez/logs"
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
  let process_path (removed, errors) path =
    if dry_run then (path :: removed, errors)
    else
      match File_ops.remove_tree path with
      | Ok () -> (path :: removed, errors)
      | Error (`Msg msg) -> (removed, (path, msg) :: errors)
  in
  let all_paths = orphan_dirs @ orphan_logs in
  let removed, errors = List.fold_left process_path ([], []) all_paths in
  Ok (List.rev removed, List.rev errors)
