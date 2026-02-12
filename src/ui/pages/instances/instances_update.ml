(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Version update, cascade update, and rollback for managed services *)

open Octez_manager_lib

let ( let* ) = Result.bind

type version_choice =
  | ManagedVersion of string
  | RegisteredDir of string * string (* alias, path *)

(** Get all dependent services transitively.
    If A depends on B and B depends on C, updating C should include both A and B.
    Returns services in dependency order (direct dependents first). *)
let get_dependent_services instance =
  match Service_registry.list () with
  | Error _ -> []
  | Ok all_services ->
      (* Find direct dependents of a given instance *)
      let direct_dependents_of inst =
        (* First, check if the service has a dependents field populated *)
        match
          List.find_opt (fun s -> s.Service.instance = inst) all_services
        with
        | Some svc when svc.Service.dependents <> [] ->
            (* Use the dependents field - this is more reliable *)
            List.filter_map
              (fun dep_inst ->
                List.find_opt
                  (fun s -> s.Service.instance = dep_inst)
                  all_services)
              svc.Service.dependents
        | _ ->
            (* Fall back to searching by depends_on field *)
            List.filter
              (fun svc ->
                match svc.Service.depends_on with
                | Some dep when dep = inst -> true
                | _ -> false)
              all_services
      in
      (* BFS to find all transitive dependents *)
      let rec collect_all visited queue acc =
        match queue with
        | [] -> List.rev acc
        | inst :: rest ->
            if List.mem inst visited then collect_all visited rest acc
            else
              let deps = direct_dependents_of inst in
              let new_instances = List.map (fun s -> s.Service.instance) deps in
              collect_all (inst :: visited) (rest @ new_instances) (deps @ acc)
      in
      (* Start from the target instance, collect all dependents *)
      let all_deps = collect_all [] [instance] [] in
      (* Remove duplicates while preserving order *)
      let seen = Hashtbl.create 16 in
      List.filter
        (fun svc ->
          let inst = svc.Service.instance in
          if Hashtbl.mem seen inst then false
          else (
            Hashtbl.add seen inst () ;
            true))
        all_deps

(** Show cascade selection modal with checkboxes for dependents *)
let rec show_cascade_modal ~instance ~new_version_str ~dependents ~on_confirm =
  let dependent_instances = List.map (fun s -> s.Service.instance) dependents in

  if dependents = [] then
    (* No dependents, just confirm the update *)
    Modal_helpers.confirm_modal
      ~title:(Printf.sprintf "Update %s to %s?" instance new_version_str)
      ~message:"This service has no dependents."
      ~on_result:(fun confirmed ->
        if confirmed then on_confirm ~cascade_instances:[])
      ()
  else
    (* Show confirmation for cascade update *)
    let dep_list = String.concat ", " dependent_instances in
    let message =
      Printf.sprintf
        "The following services depend on %s and will also be updated:\n\
         %s\n\
         All services will be stopped, updated to %s, and restarted.\n\
         If any service fails to start, all will be rolled back."
        instance
        dep_list
        new_version_str
    in
    Modal_helpers.confirm_modal
      ~title:"Cascade Update"
      ~message
      ~on_result:(fun confirmed ->
        if confirmed then on_confirm ~cascade_instances:dependent_instances)
      ()

(** Try to restart service with original config after a failure *)
and try_restart_with_old_config ~svc ~old_bin_source:_ =
  let instance = svc.Service.instance in
  let role = svc.Service.role in
  (* Best effort - try to restart with original config *)
  let cap = Miaou_interfaces.Service_lifecycle.require () in
  match
    Miaou_interfaces.Service_lifecycle.start cap ~role ~service:instance
  with
  | Ok () ->
      Context.toast_info
        (Printf.sprintf "%s restarted with previous version" instance)
  | Error _ ->
      Context.toast_error
        (Printf.sprintf
           "%s left stopped - manual intervention required"
           instance)

(** Update a single service with rollback support *)
and do_update_single_service ~svc ~old_bin_source ~new_bin_source () =
  let instance = svc.Service.instance in
  let role = svc.Service.role in

  (* Stop the service first *)
  let* () =
    let cap = Miaou_interfaces.Service_lifecycle.require () in
    Miaou_interfaces.Service_lifecycle.stop cap ~role ~service:instance
    |> Result.map_error (fun e -> `Msg e)
  in

  (* Resolve the new bin_source to get the actual path *)
  match Binary_registry.resolve_bin_source new_bin_source with
  | Error (`Msg msg) ->
      (* Resolution failed - try to restart with old config *)
      try_restart_with_old_config ~svc ~old_bin_source ;
      Error (`Msg (Printf.sprintf "Failed to resolve new version: %s" msg))
  | Ok new_path -> (
      (* Update the service config *)
      let updated_svc =
        {
          svc with
          Service.app_bin_dir = new_path;
          bin_source = Some new_bin_source;
        }
      in
      let* () = Service_registry.write updated_svc in

      (* Regenerate systemd unit file with new APP_BIN_DIR *)
      let* () =
        Systemd.install_unit
          ~quiet:true
          ~role
          ~app_bin_dir:new_path
          ~user:svc.Service.service_user
          ()
      in

      (* Update the per-instance dropin to override APP_BIN_DIR *)
      let* () =
        Systemd.write_dropin
          ~quiet:true
          ~role
          ~inst:instance
          ~data_dir:svc.Service.data_dir
          ~logging_mode:svc.Service.logging_mode
          ~app_bin_dir:new_path
          ()
      in

      (* Try to start the service *)
      let cap = Miaou_interfaces.Service_lifecycle.require () in
      match
        Miaou_interfaces.Service_lifecycle.start cap ~role ~service:instance
      with
      | Ok () ->
          (* Success - invalidate version cache to force immediate refresh *)
          System_metrics_scheduler.invalidate_version ~role ~instance ;
          Ok ()
      | Error start_error ->
          (* Start failed - offer rollback *)
          show_rollback_modal
            ~instance
            ~svc
            ~old_bin_source
            ~new_bin_source
            ~error:start_error ;
          Error (`Msg start_error))

and show_rollback_modal ~instance ~svc ~old_bin_source ~new_bin_source ~error:_
    =
  let old_version_str =
    match old_bin_source with
    | Binary_registry.Managed_octez_version v -> "v" ^ v
    | Binary_registry.Managed_signatory_version v -> "signatory-v" ^ v
    | Binary_registry.Registered_alias a -> a
    | Binary_registry.Raw_path p -> p
  in

  let new_version_str =
    match new_bin_source with
    | Binary_registry.Managed_octez_version v -> "v" ^ v
    | Binary_registry.Managed_signatory_version v -> "signatory-v" ^ v
    | Binary_registry.Registered_alias a -> a
    | Binary_registry.Raw_path p -> p
  in

  (* Keep title concise - error details shown via View Logs *)
  let modal_title = Printf.sprintf "Update to %s failed" new_version_str in

  Modal_helpers.open_choice_modal
    ~title:modal_title
    ~items:[`Rollback; `ViewLogs; `KeepStopped]
    ~to_string:(function
      | `Rollback -> Printf.sprintf "Rollback to %s" old_version_str
      | `ViewLogs -> "View Logs"
      | `KeepStopped -> "Keep stopped")
    ~on_select:(function
      | `Rollback ->
          Background_runner.enqueue (fun () ->
              match do_rollback ~svc ~old_bin_source () with
              | Ok () ->
                  Context.toast_success
                    (Printf.sprintf
                       "Rolled back %s to %s"
                       instance
                       old_version_str) ;
                  Context.mark_instances_dirty ()
              | Error (`Msg msg) ->
                  Context.toast_error (Printf.sprintf "Rollback failed: %s" msg))
      | `ViewLogs ->
          Context.set_pending_instance_detail instance ;
          Context.navigate Log_viewer_page.name
      | `KeepStopped ->
          Context.toast_info (Printf.sprintf "%s remains stopped" instance))
    ()

and do_rollback ~svc ~old_bin_source () =
  let instance = svc.Service.instance in
  let role = svc.Service.role in

  (* Resolve old bin_source back to path *)
  let* old_path = Binary_registry.resolve_bin_source old_bin_source in

  (* Restore old config *)
  let restored_svc =
    {svc with Service.app_bin_dir = old_path; bin_source = Some old_bin_source}
  in
  let* () = Service_registry.write restored_svc in

  (* Regenerate systemd unit file with old APP_BIN_DIR *)
  let* () =
    Systemd.install_unit
      ~quiet:true
      ~role
      ~app_bin_dir:old_path
      ~user:svc.Service.service_user
      ()
  in

  (* Update the per-instance dropin to override APP_BIN_DIR *)
  let* () =
    Systemd.write_dropin
      ~quiet:true
      ~role
      ~inst:instance
      ~data_dir:svc.Service.data_dir
      ~logging_mode:svc.Service.logging_mode
      ~app_bin_dir:old_path
      ()
  in

  (* Try to start with old version *)
  let cap = Miaou_interfaces.Service_lifecycle.require () in
  let result =
    Miaou_interfaces.Service_lifecycle.start cap ~role ~service:instance
    |> Result.map_error (fun e -> `Msg e)
  in
  (* Invalidate cache on successful rollback *)
  (match result with
  | Ok () -> System_metrics_scheduler.invalidate_version ~role ~instance
  | Error _ -> ()) ;
  result

(** Check if a service is currently running *)
and is_service_running svc =
  let instance = svc.Service.instance in
  let services = Data.load_service_states () in
  List.exists
    (fun s ->
      s.Data.Service_state.service.Service.instance = instance
      && s.Data.Service_state.status = Data.Service_state.Running)
    services

(** Just restart a service that's already at the target version *)
and restart_service_for_cascade ~svc () =
  let instance = svc.Service.instance in
  let role = svc.Service.role in
  let cap = Miaou_interfaces.Service_lifecycle.require () in

  (* Stop the service *)
  let* () =
    Miaou_interfaces.Service_lifecycle.stop cap ~role ~service:instance
    |> Result.map_error (fun e -> `Msg e)
  in

  (* Start it back up *)
  let result =
    Miaou_interfaces.Service_lifecycle.start cap ~role ~service:instance
    |> Result.map_error (fun e -> `Msg e)
  in
  (* Invalidate cache on successful restart *)
  (match result with
  | Ok () -> System_metrics_scheduler.invalidate_version ~role ~instance
  | Error _ -> ()) ;
  result

(** Perform cascade update of multiple services *)
and do_cascade_update ~services ~new_bin_source () =
  (* First, record which services are currently running *)
  let was_running =
    List.filter_map
      (fun svc ->
        if is_service_running svc then Some svc.Service.instance else None)
      services
  in
  let rec update_all acc_updated = function
    | [] -> Ok acc_updated
    | svc :: rest -> (
        let old_bin_source = Service.get_bin_source svc in
        (* Check if service is already at target version *)
        let result =
          if old_bin_source = new_bin_source then
            (* Already at target version - just restart if it was running *)
            if List.mem svc.Service.instance was_running then
              restart_service_for_cascade ~svc ()
            else Ok () (* Not running, no action needed *)
          else
            (* Different version - do full update *)
            do_update_single_service ~svc ~old_bin_source ~new_bin_source ()
        in
        match result with
        | Ok () -> update_all ((svc, old_bin_source) :: acc_updated) rest
        | Error _ as e ->
            (* One update/restart failed - rollback all successfully updated ones *)
            List.iter
              (fun (updated_svc, old_bs) ->
                (* Only rollback (restart) if service was running before *)
                let inst = updated_svc.Service.instance in
                if List.mem inst was_running then
                  let current_bs = Service.get_bin_source updated_svc in
                  (* Check if this was an update or just a restart *)
                  if old_bs = current_bs then
                    (* Was a restart-only - just try to start it again *)
                    let cap = Miaou_interfaces.Service_lifecycle.require () in
                    match
                      Miaou_interfaces.Service_lifecycle.start
                        cap
                        ~role:updated_svc.Service.role
                        ~service:inst
                    with
                    | Ok () ->
                        Context.toast_info
                          (Printf.sprintf "Restarted %s after failure" inst)
                    | Error _ ->
                        Context.toast_error
                          (Printf.sprintf "Failed to restart %s" inst)
                  else
                    (* Was a full update - do full rollback *)
                    match
                      do_rollback ~svc:updated_svc ~old_bin_source:old_bs ()
                    with
                    | Ok () ->
                        Context.toast_info
                          (Printf.sprintf "Rolled back %s" inst)
                    | Error _ ->
                        Context.toast_error
                          (Printf.sprintf "Failed to rollback %s" inst)
                else
                  (* Service wasn't running before, just restore config if updated *)
                  let current_bs = Service.get_bin_source updated_svc in
                  if old_bs <> current_bs then
                    match Binary_registry.resolve_bin_source old_bs with
                    | Error _ -> ()
                    | Ok old_path ->
                        let restored =
                          {
                            updated_svc with
                            Service.app_bin_dir = old_path;
                            bin_source = Some old_bs;
                          }
                        in
                        ignore (Service_registry.write restored) ;
                        (* Regenerate systemd unit file with restored APP_BIN_DIR *)
                        ignore
                          (Systemd.install_unit
                             ~quiet:true
                             ~role:updated_svc.Service.role
                             ~app_bin_dir:old_path
                             ~user:updated_svc.Service.service_user
                             ()) ;
                        (* Update per-instance dropin with restored APP_BIN_DIR *)
                        ignore
                          (Systemd.write_dropin
                             ~quiet:true
                             ~role:updated_svc.Service.role
                             ~inst
                             ~data_dir:updated_svc.Service.data_dir
                             ~logging_mode:updated_svc.Service.logging_mode
                             ~app_bin_dir:old_path
                             ()))
              acc_updated ;
            e)
  in
  update_all [] services

let update_version_modal svc =
  let instance = svc.Service.instance in
  let current_bin_source = Service.get_bin_source svc in

  (* Get current version for filtering - try to extract from binary *)
  let current_version_opt =
    match current_bin_source with
    | Binary_registry.Managed_octez_version v -> Some v
    | Binary_registry.Managed_signatory_version _
    | Binary_registry.Registered_alias _ | Binary_registry.Raw_path _ ->
        (* Try to get version from the actual binary *)
        let binary_name = Systemd_unit_template.role_binary svc.Service.role in
        let binary_path = Filename.concat svc.Service.app_bin_dir binary_name in
        if Sys.file_exists binary_path then
          match Cmd_runner.run_out [binary_path; "--version"] with
          | Ok version_output -> (
              (* Parse "24.0 (hash)" or "Octez 24.0" to extract "24.0" *)
              let version_str = String.trim version_output in
              (* Try to extract X.Y or X.Y.Z pattern *)
              try
                let _ =
                  Str.search_forward
                    (Str.regexp "\\([0-9]+\\.[0-9]+\\(\\.[0-9]+\\)?\\)")
                    version_str
                    0
                in
                Some (Str.matched_group 1 version_str)
              with Not_found -> None)
          | Error _ -> None
        else None
  in

  (* Load available versions - filter to only newer or equal versions *)
  let managed_versions =
    match Binary_registry.list_managed_versions () with
    | Ok versions ->
        let filtered_versions =
          match current_version_opt with
          | Some current_v ->
              (* Only include versions >= current version *)
              List.filter
                (fun v -> Binary_registry.compare_versions v current_v >= 0)
                versions
          | None ->
              (* No current version detected, show all *)
              versions
        in
        List.map (fun v -> ManagedVersion v) filtered_versions
    | Error _ -> []
  in

  let registered_dirs =
    match Binary_registry.load_registered_dirs () with
    | Ok dirs ->
        List.map
          (fun (ld : Binary_registry.registered_dir) ->
            RegisteredDir (ld.alias, ld.path))
          dirs
    | Error _ -> []
  in
  let all_choices = managed_versions @ registered_dirs in

  if all_choices = [] then (
    Modal_helpers.show_error
      ~title:"No Versions Available"
      "No managed versions or registered directories available. Download a \
       version or register a directory first." ;
    ())
  else
    let modal_title = Printf.sprintf "Update Version · %s" instance in

    let to_string = function
      | ManagedVersion v -> Printf.sprintf "v%s (managed)" v
      | RegisteredDir (alias, path) -> Printf.sprintf "%s (%s)" alias path
    in

    Modal_helpers.open_choice_modal
      ~title:modal_title
      ~items:all_choices
      ~to_string
      ~on_select:(fun choice ->
        let new_bin_source =
          match choice with
          | ManagedVersion v -> Binary_registry.Managed_octez_version v
          | RegisteredDir (alias, _) -> Binary_registry.Registered_alias alias
        in

        (* Check if it's actually different *)
        if new_bin_source = current_bin_source then (
          Context.toast_info "Version unchanged" ;
          ())
        else
          let new_version_str =
            match new_bin_source with
            | Binary_registry.Managed_octez_version v -> "v" ^ v
            | Binary_registry.Managed_signatory_version v -> "signatory-v" ^ v
            | Binary_registry.Registered_alias a -> a
            | Binary_registry.Raw_path p -> p
          in

          (* Get dependent services *)
          let dependents = get_dependent_services instance in

          (* Show cascade modal *)
          show_cascade_modal
            ~instance
            ~new_version_str
            ~dependents
            ~on_confirm:(fun ~cascade_instances ->
              (* Perform update in background *)
              Background_runner.enqueue (fun () ->
                  (* Build list of services to update *)
                  let cascade_services =
                    List.filter_map
                      (fun dep_inst ->
                        match Service_registry.find ~instance:dep_inst with
                        | Ok (Some s) -> Some s
                        | _ -> None)
                      cascade_instances
                  in

                  let all_services = svc :: cascade_services in

                  (* Perform cascade update *)
                  match
                    do_cascade_update ~services:all_services ~new_bin_source ()
                  with
                  | Ok updated ->
                      Context.toast_success
                        (Printf.sprintf
                           "Updated %d service(s) to %s"
                           (List.length updated)
                           new_version_str) ;
                      Context.mark_instances_dirty ()
                  | Error (`Msg msg) ->
                      Context.toast_error
                        (Printf.sprintf "Update failed: %s" msg))))
      ()

module For_tests = struct
  (** Extract version string from binary --version output.
      Parses patterns like "24.0 (hash)", "Octez 24.0", "v24.0.1" *)
  let extract_version_string version_output =
    let version_str = String.trim version_output in
    try
      let _ =
        Str.search_forward
          (Str.regexp "\\([0-9]+\\.[0-9]+\\(\\.[0-9]+\\)?\\)")
          version_str
          0
      in
      Some (Str.matched_group 1 version_str)
    with Not_found -> None

  (** Map a service role to its binary name *)
  let role_to_binary_name = Systemd_unit_template.role_binary

  (** BFS to collect all transitive dependents, parameterized on deps lookup.
      Returns the accumulated dependent services (not the root). *)
  let collect_dependents ~get_deps instance =
    let rec collect_all visited queue acc =
      match queue with
      | [] -> List.rev acc
      | inst :: rest ->
          if List.mem inst visited then collect_all visited rest acc
          else
            let deps = get_deps inst in
            let new_instances = List.map (fun s -> s.Service.instance) deps in
            collect_all (inst :: visited) (rest @ new_instances) (deps @ acc)
    in
    collect_all [] [instance] []

  (** Remove duplicate services while preserving order *)
  let dedup_services services =
    let seen = Hashtbl.create 16 in
    List.filter
      (fun svc ->
        let inst = svc.Service.instance in
        if Hashtbl.mem seen inst then false
        else (
          Hashtbl.add seen inst () ;
          true))
      services
end
