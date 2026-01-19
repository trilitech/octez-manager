(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

(** Action handlers for the instances page *)

open Octez_manager_lib
open Rresult
open Instances_state

let ( let* ) = Result.bind

let with_service state handler =
  match current_service state with
  | None ->
      Modal_helpers.show_error ~title:"Instances" "Select an instance first" ;
      state
  | Some svc -> handler svc

let run_unit_action ~verb ~instance action =
  let description =
    Printf.sprintf "%s %s" (String.capitalize_ascii verb) instance
  in
  (* Toast immediately that it started *)
  Context.toast_info (Printf.sprintf "Started: %s %s" instance verb) ;
  (* Submit to background job manager *)
  Job_manager.submit
    ~description
    (fun ~append_log:_ () -> action ())
    ~on_complete:(fun status ->
      match status with
      | Job_manager.Succeeded ->
          Context.toast_success (Printf.sprintf "%s: %s finished" instance verb) ;
          Context.mark_instances_dirty ()
      | Job_manager.Failed msg ->
          (* Record failure for display in status line *)
          record_failure ~instance ~error:msg ;
          Context.toast_error
            (Printf.sprintf "%s: %s failed: %s" instance verb msg) ;
          Context.mark_instances_dirty ()
      | _ -> ())

let require_installer () =
  match
    Miaou_interfaces.Capability.get Manager_interfaces.Installer_capability.key
  with
  | Some cap ->
      let module I = (val (cap : Manager_interfaces.Installer_capability.t)) in
      Ok (module I : Manager_interfaces.Installer)
  | None -> Error (`Msg "Installer capability not available")

let do_remove ~instance ~delete_data_dir () =
  Rpc_scheduler.stop_head_monitor instance ;
  let* (module I) = require_installer () in
  I.remove_service ~quiet:true ~delete_data_dir ~instance ()

let do_purge ~instance () =
  Rpc_scheduler.stop_head_monitor instance ;
  let* (module I) = require_installer () in
  I.purge_service
    ~quiet:true
    ~prompt_yes_no:(fun _ ~default:_ -> true)
    ~instance
    ()

let remove_with_dependents_confirm ~instance ~dependents ~delete_data_dir =
  Modal_helpers.open_choice_modal
    ~title:"Confirm Removal"
    ~items:[`Confirm; `Cancel]
    ~to_string:(function
      | `Confirm ->
          Printf.sprintf
            "Proceed (will stop: %s)"
            (String.concat ", " dependents)
      | `Cancel -> "Cancel")
    ~on_select:(function
      | `Confirm ->
          run_unit_action ~verb:"remove" ~instance (fun () ->
              do_remove ~instance ~delete_data_dir ())
      | `Cancel -> ())
    ()

let purge_with_dependents_confirm ~instance ~dependents =
  Modal_helpers.open_choice_modal
    ~title:"Confirm Purge"
    ~items:[`Confirm; `Cancel]
    ~to_string:(function
      | `Confirm ->
          Printf.sprintf
            "Proceed (will stop: %s)"
            (String.concat ", " dependents)
      | `Cancel -> "Cancel")
    ~on_select:(function
      | `Confirm ->
          run_unit_action ~verb:"purge" ~instance (fun () ->
              do_purge ~instance ())
      | `Cancel -> ())
    ()

let remove_modal state =
  with_service state (fun svc_state ->
      let svc = svc_state.Service_state.service in
      let instance = svc.Service.instance in
      let dependents = svc.Service.dependents in
      Modal_helpers.open_choice_modal
        ~title:(Printf.sprintf "Remove · %s" instance)
        ~items:[`Remove; `RemoveData; `Purge]
        ~to_string:(function
          | `Remove -> "Remove (keep data)"
          | `RemoveData -> "Remove + delete data"
          | `Purge -> "Purge (also drop user/logs)")
        ~on_select:(fun choice ->
          match choice with
          | `Remove ->
              if dependents = [] then
                run_unit_action ~verb:"remove" ~instance (fun () ->
                    do_remove ~instance ~delete_data_dir:false ())
              else
                remove_with_dependents_confirm
                  ~instance
                  ~dependents
                  ~delete_data_dir:false
          | `RemoveData ->
              if dependents = [] then
                run_unit_action ~verb:"remove" ~instance (fun () ->
                    do_remove ~instance ~delete_data_dir:true ())
              else
                remove_with_dependents_confirm
                  ~instance
                  ~dependents
                  ~delete_data_dir:true
          | `Purge ->
              if dependents = [] then
                run_unit_action ~verb:"purge" ~instance (fun () ->
                    do_purge ~instance ())
              else purge_with_dependents_confirm ~instance ~dependents)
        () ;
      state)

let journalctl_args unit_name =
  if Common.is_root () then
    ["journalctl"; "-u"; unit_name; "--no-pager"; "-n"; "200"]
  else ["journalctl"; "--user"; "-u"; unit_name; "--no-pager"; "-n"; "200"]

(* Replaced by log_viewer page navigation *)
let _view_logs_old state =
  with_service state (fun svc_state ->
      let svc = svc_state.Service_state.service in
      let title = Printf.sprintf "Logs · %s" svc.Service.instance in
      let env =
        match Node_env.read ~inst:svc.Service.instance with
        | Ok pairs -> pairs
        | Error _ -> []
      in
      (* Find the directory where daily logs are written *)
      let logs_dir () =
        let lookup key =
          match List.assoc_opt key env with
          | Some v when String.trim v <> "" -> Some (String.trim v)
          | _ -> None
        in
        match svc.Service.role with
        | "node" ->
            (* Node: <data_dir>/daily_logs/ *)
            Filename.concat svc.Service.data_dir "daily_logs"
        | "baker" ->
            (* Baker: <base_dir>/logs/octez-baker/ *)
            let base =
              Option.value
                (lookup "OCTEZ_BAKER_BASE_DIR")
                ~default:svc.Service.data_dir
            in
            Filename.concat (Filename.concat base "logs") "octez-baker"
        | "accuser" ->
            (* Accuser: <base_dir>/logs/octez-accuser/ *)
            let base =
              Option.value
                (lookup "OCTEZ_CLIENT_BASE_DIR")
                ~default:svc.Service.data_dir
            in
            Filename.concat (Filename.concat base "logs") "octez-accuser"
        | "dal-node" ->
            (* DAL node: <data_dir>/daily_logs/ *)
            let base =
              Option.value
                (lookup "OCTEZ_DAL_DATA_DIR")
                ~default:svc.Service.data_dir
            in
            Filename.concat base "daily_logs"
        | "signer" ->
            (* Signer: <base_dir>/logs/octez-signer/ *)
            let base =
              Option.value
                (lookup "OCTEZ_SIGNER_BASE_DIR")
                ~default:svc.Service.data_dir
            in
            Filename.concat (Filename.concat base "logs") "octez-signer"
        | _ -> Filename.concat svc.Service.data_dir "daily_logs"
      in
      let daily_logs () =
        let dir = logs_dir () in
        if Sys.file_exists dir && Sys.is_directory dir then
          Sys.readdir dir |> Array.to_list
          |> List.map (Filename.concat dir)
          |> List.filter Sys.file_exists
        else []
      in
      let latest path_candidates =
        path_candidates
        |> List.filter_map (fun p ->
            try Some ((Unix.stat p).Unix.st_mtime, p) with _ -> None)
        |> List.sort (fun (a, _) (b, _) -> Float.compare b a)
        |> function
        | (_, p) :: _ -> Some p
        | [] -> None
      in
      let tail_file path =
        match Common.run_out ["tail"; "-n"; "200"; path] with
        | Ok text ->
            Modal_helpers.open_text_modal
              ~title
              ~lines:(String.split_on_char '\n' text) ;
            state
        | Error (`Msg msg) ->
            Modal_helpers.show_error ~title msg ;
            state
      in
      let show_journald () =
        let unit = Systemd.unit_name svc.Service.role svc.Service.instance in
        match Common.run_out (journalctl_args unit) with
        | Ok text ->
            Modal_helpers.open_text_modal
              ~title
              ~lines:(String.split_on_char '\n' text)
        | Error (`Msg msg) -> Modal_helpers.show_error ~title msg
      in
      (* All octez binaries write daily logs - offer choice if they exist *)
      let logs = daily_logs () in
      match latest logs with
      | Some path ->
          Modal_helpers.open_choice_modal
            ~title:"View Logs"
            ~items:[`Journald; `DailyLogs]
            ~to_string:(function
              | `Journald -> "Journald (systemd)"
              | `DailyLogs -> "Daily Logs (octez)")
            ~on_select:(function
              | `Journald -> show_journald ()
              | `DailyLogs -> ignore (tail_file path))
            () ;
          state
      | None ->
          (* No daily logs found, just show journald *)
          show_journald () ;
          state)

(* Start a single service (internal helper) *)
let do_start_service ~instance ~role =
  let cap = Miaou_interfaces.Service_lifecycle.require () in
  Miaou_interfaces.Service_lifecycle.start cap ~role ~service:instance
  |> Result.map_error (fun e -> `Msg e)

(* Offer to start stopped dependents after starting a service *)
let offer_start_dependents ~instance =
  match Lifecycle.get_stopped_dependents ~instance () with
  | Ok [] -> ()
  | Ok stopped ->
      let dep_names = List.map (fun s -> s.Service.instance) stopped in
      Modal_helpers.open_choice_modal
        ~title:"Start Dependents?"
        ~items:[`StartAll; `Dismiss]
        ~to_string:(function
          | `StartAll ->
              Printf.sprintf "Start all (%s)" (String.concat ", " dep_names)
          | `Dismiss -> "Dismiss (start later)")
        ~on_select:(function
          | `StartAll ->
              stopped
              |> List.iter (fun dep ->
                  Context.toast_info
                    (Printf.sprintf "Starting %s..." dep.Service.instance) ;
                  match
                    do_start_service
                      ~instance:dep.Service.instance
                      ~role:dep.Service.role
                  with
                  | Ok () ->
                      Context.toast_success
                        (Printf.sprintf "%s started" dep.Service.instance)
                  | Error (`Msg e) ->
                      record_failure ~instance:dep.Service.instance ~error:e ;
                      Context.toast_error
                        (Printf.sprintf "%s: %s" dep.Service.instance e)) ;
              Context.mark_instances_dirty ()
          | `Dismiss -> ())
        ()
  | Error _ -> ()

(* Start with cascade: check dependencies first, then offer to start dependents *)
let start_with_cascade ~instance ~role =
  match Lifecycle.get_stopped_dependencies ~instance () with
  | Error (`Msg e) ->
      Context.toast_error (Printf.sprintf "Error checking dependencies: %s" e)
  | Ok [] ->
      (* No stopped dependencies, start directly *)
      run_unit_action ~verb:"start" ~instance (fun () ->
          do_start_service ~instance ~role) ;
      (* After a short delay, offer to start dependents *)
      Job_manager.submit
        ~description:(Printf.sprintf "Check dependents for %s" instance)
        (fun ~append_log:_ () ->
          (* Small delay to let the start complete *)
          Unix.sleepf 0.5 ;
          Ok ())
        ~on_complete:(fun _ -> offer_start_dependents ~instance)
  | Ok stopped_deps ->
      (* Dependencies are stopped, ask user to start them first *)
      let dep_names = List.map (fun s -> s.Service.instance) stopped_deps in
      Modal_helpers.open_choice_modal
        ~title:"Dependencies Not Running"
        ~items:[`StartDeps; `Cancel]
        ~to_string:(function
          | `StartDeps ->
              Printf.sprintf
                "Start dependencies first (%s)"
                (String.concat ", " dep_names)
          | `Cancel -> "Cancel")
        ~on_select:(function
          | `Cancel -> ()
          | `StartDeps ->
              (* Start dependencies in order (topmost parent first) *)
              let success =
                List.fold_left
                  (fun acc dep ->
                    if acc then (
                      Context.toast_info
                        (Printf.sprintf "Starting %s..." dep.Service.instance) ;
                      match
                        do_start_service
                          ~instance:dep.Service.instance
                          ~role:dep.Service.role
                      with
                      | Ok () ->
                          Context.toast_success
                            (Printf.sprintf "%s started" dep.Service.instance) ;
                          true
                      | Error (`Msg e) ->
                          record_failure ~instance:dep.Service.instance ~error:e ;
                          Context.toast_error
                            (Printf.sprintf "%s: %s" dep.Service.instance e) ;
                          false)
                    else acc)
                  true
                  stopped_deps
              in
              if success then (
                (* Now start the actual service *)
                Context.toast_info (Printf.sprintf "Starting %s..." instance) ;
                match do_start_service ~instance ~role with
                | Ok () ->
                    Context.toast_success (Printf.sprintf "%s started" instance) ;
                    Context.mark_instances_dirty () ;
                    (* Offer to start dependents *)
                    offer_start_dependents ~instance
                | Error (`Msg e) ->
                    record_failure ~instance ~error:e ;
                    Context.toast_error (Printf.sprintf "%s: %s" instance e) ;
                    Context.mark_instances_dirty ())
              else Context.mark_instances_dirty ())
        ()

(* Restart a single service (internal helper) *)
let do_restart_service ~instance ~role =
  let cap = Miaou_interfaces.Service_lifecycle.require () in
  Miaou_interfaces.Service_lifecycle.restart cap ~role ~service:instance
  |> Result.map_error (fun e -> `Msg e)

(* Offer to restart dependents after restarting a service *)
(* Shows ALL dependents, not just stopped ones - they need restart to reconnect *)
let offer_restart_dependents ~instance =
  match Service_registry.find ~instance with
  | Ok (Some svc) when svc.Service.dependents <> [] ->
      let dep_names = svc.Service.dependents in
      Modal_helpers.open_choice_modal
        ~title:"Restart Dependents?"
        ~items:[`RestartAll; `Dismiss]
        ~to_string:(function
          | `RestartAll ->
              Printf.sprintf "Restart all (%s)" (String.concat ", " dep_names)
          | `Dismiss -> "Dismiss (restart later)")
        ~on_select:(function
          | `RestartAll ->
              (* Run in background to avoid blocking UI *)
              Job_manager.submit
                ~description:"Restarting dependents"
                (fun ~append_log () ->
                  (* Wait a bit for parent service to be fully ready *)
                  Unix.sleepf 1.0 ;
                  dep_names
                  |> List.iter (fun dep_inst ->
                      match Service_registry.find ~instance:dep_inst with
                      | Ok (Some dep) ->
                          append_log
                            (Printf.sprintf
                               "Restarting %s..."
                               dep.Service.instance) ;
                          (* Retry logic: try up to 3 times with delay *)
                          let rec try_restart retries =
                            match
                              do_restart_service
                                ~instance:dep.Service.instance
                                ~role:dep.Service.role
                            with
                            | Ok () ->
                                append_log
                                  (Printf.sprintf
                                     "%s restarted"
                                     dep.Service.instance)
                            | Error (`Msg e) ->
                                if retries > 0 then (
                                  Unix.sleepf 2.0 ;
                                  try_restart (retries - 1))
                                else (
                                  record_failure
                                    ~instance:dep.Service.instance
                                    ~error:e ;
                                  append_log
                                    (Printf.sprintf
                                       "Failed: %s: %s"
                                       dep.Service.instance
                                       e))
                          in
                          try_restart 2
                      | _ ->
                          append_log
                            (Printf.sprintf "Service %s not found" dep_inst)) ;
                  Ok ())
                ~on_complete:(fun _ -> Context.mark_instances_dirty ())
          | `Dismiss -> ())
        ()
  | _ -> ()

(* Restart with cascade: check dependencies first, then offer to restart dependents *)
let restart_with_cascade ~instance ~role =
  match Lifecycle.get_stopped_dependencies ~instance () with
  | Error (`Msg e) ->
      Context.toast_error (Printf.sprintf "Error checking dependencies: %s" e)
  | Ok [] ->
      (* No stopped dependencies, restart directly *)
      run_unit_action ~verb:"restart" ~instance (fun () ->
          do_restart_service ~instance ~role) ;
      (* After a short delay, offer to restart dependents *)
      Job_manager.submit
        ~description:(Printf.sprintf "Check dependents for %s" instance)
        (fun ~append_log:_ () ->
          Unix.sleepf 0.5 ;
          Ok ())
        ~on_complete:(fun _ -> offer_restart_dependents ~instance)
  | Ok stopped_deps ->
      (* Dependencies are stopped, ask user to start them first *)
      let dep_names = List.map (fun s -> s.Service.instance) stopped_deps in
      Modal_helpers.open_choice_modal
        ~title:"Dependencies Not Running"
        ~items:[`StartDeps; `Cancel]
        ~to_string:(function
          | `StartDeps ->
              Printf.sprintf
                "Start dependencies first (%s)"
                (String.concat ", " dep_names)
          | `Cancel -> "Cancel")
        ~on_select:(function
          | `Cancel -> ()
          | `StartDeps ->
              (* Start dependencies in order (topmost parent first) *)
              let success =
                List.fold_left
                  (fun acc dep ->
                    if acc then (
                      Context.toast_info
                        (Printf.sprintf "Starting %s..." dep.Service.instance) ;
                      match
                        do_start_service
                          ~instance:dep.Service.instance
                          ~role:dep.Service.role
                      with
                      | Ok () ->
                          Context.toast_success
                            (Printf.sprintf "%s started" dep.Service.instance) ;
                          true
                      | Error (`Msg e) ->
                          record_failure ~instance:dep.Service.instance ~error:e ;
                          Context.toast_error
                            (Printf.sprintf "%s: %s" dep.Service.instance e) ;
                          false)
                    else acc)
                  true
                  stopped_deps
              in
              if success then (
                (* Now restart the actual service *)
                Context.toast_info (Printf.sprintf "Restarting %s..." instance) ;
                match do_restart_service ~instance ~role with
                | Ok () ->
                    Context.toast_success
                      (Printf.sprintf "%s restarted" instance) ;
                    Context.mark_instances_dirty () ;
                    (* Offer to restart dependents *)
                    offer_restart_dependents ~instance
                | Error (`Msg e) ->
                    record_failure ~instance ~error:e ;
                    Context.toast_error (Printf.sprintf "%s: %s" instance e) ;
                    Context.mark_instances_dirty ())
              else Context.mark_instances_dirty ())
        ()

(* Edit instance - navigate to appropriate form *)
let do_edit_instance svc =
  (* Set the edit context (service will be stopped when form is submitted) *)
  Context.set_pending_edit_service
    ~service:svc
    ~stopped_dependents:svc.Service.dependents ;
  (* Navigate to the appropriate install form based on role *)
  let form_page =
    match svc.Service.role with
    | "node" -> "install_node_form_v3"
    | "baker" -> "install_baker_form_v3"
    | "accuser" -> "install_accuser_form_v3"
    | "dal-node" | "dal" -> "install_dal_node_form_v3"
    | _ -> "instances"
  in
  Context.navigate form_page

let confirm_edit_modal svc =
  if svc.Service.dependents = [] then do_edit_instance svc
  else
    Modal_helpers.open_choice_modal
      ~title:"Confirm Edit"
      ~items:[`Confirm; `Cancel]
      ~to_string:(function
        | `Confirm ->
            Printf.sprintf
              "Proceed (will stop: %s)"
              (String.concat ", " svc.Service.dependents)
        | `Cancel -> "Cancel")
      ~on_select:(fun choice ->
        match choice with `Confirm -> do_edit_instance svc | `Cancel -> ())
      ()

let instance_actions_modal state =
  with_service state (fun svc_state ->
      let svc = svc_state.Service_state.service in
      Modal_helpers.open_choice_modal
        ~title:("Actions · " ^ svc.Service.instance)
        ~items:
          [
            `Details;
            `Edit;
            `Start;
            `Stop;
            `Restart;
            `Logs;
            `Export_logs;
            `Remove;
          ]
        ~to_string:(function
          | `Details -> "Details"
          | `Edit -> "Edit"
          | `Start -> "Start"
          | `Stop -> "Stop"
          | `Restart -> "Restart"
          | `Logs -> "View Logs"
          | `Export_logs -> "Export Logs"
          | `Remove -> "Remove")
        ~on_select:(fun choice ->
          let instance = svc.Service.instance in
          let role = svc.Service.role in
          match choice with
          | `Details ->
              Context.set_pending_instance_detail instance ;
              Context.navigate Instance_details.name
          | `Edit -> confirm_edit_modal svc
          | `Start -> start_with_cascade ~instance ~role
          | `Stop ->
              (* Clear any previous failure when user intentionally stops *)
              clear_failure ~instance ;
              run_unit_action ~verb:"stop" ~instance (fun () ->
                  let cap = Miaou_interfaces.Service_lifecycle.require () in
                  Miaou_interfaces.Service_lifecycle.stop
                    cap
                    ~role
                    ~service:instance
                  |> Result.map_error (fun e -> `Msg e))
          | `Restart -> restart_with_cascade ~instance ~role
          | `Logs ->
              Context.set_pending_instance_detail instance ;
              Context.navigate Log_viewer_page.name
          | `Export_logs -> (
              match Log_export.export_logs ~instance ~svc with
              | Ok path ->
                  Context.toast_info
                    (Printf.sprintf "Logs exported to: %s" path)
              | Error (`Msg err) ->
                  Context.toast_error (Printf.sprintf "Export failed: %s" err))
          | `Remove -> remove_modal state |> ignore)
        () ;
      state)

let create_menu_modal state =
  let open Modal_helpers in
  open_choice_modal
    ~title:"Create Service"
    ~items:[`Node; `DalNode; `Baker; `Accuser]
    ~to_string:(function
      | `Node -> "Node"
      | `DalNode -> "DAL Node"
      | `Baker -> "Baker"
      | `Accuser -> "Accuser")
    ~on_select:(function
      | `Node -> Context.navigate Install_node_form_v3.name
      | `Baker -> Context.navigate Install_baker_form_v3.name
      | `Accuser -> Context.navigate Install_accuser_form_v3.name
      | `DalNode -> Context.navigate Install_dal_node_form_v3.name)
    () ;
  state

let go_to_diagnostics state =
  Context.navigate Diagnostics.name ;
  state

let current_external_service s =
  let external_start_idx = services_start_idx + List.length s.services in
  if s.selected >= external_start_idx then
    let ext_idx = s.selected - external_start_idx in
    List.nth_opt s.external_services ext_idx
  else None

let external_service_actions_modal state ext =
  let unit_name = ext.External_service.config.unit_name in
  let display_name = ext.External_service.suggested_instance_name in
  Modal_helpers.open_choice_modal
    ~title:("External Service · " ^ display_name)
    ~items:[`Details; `Start; `Stop; `Restart; `Logs]
    ~to_string:(function
      | `Details -> "Details"
      | `Start -> "Start"
      | `Stop -> "Stop"
      | `Restart -> "Restart"
      | `Logs -> "View Logs")
    ~on_select:(fun choice ->
      match choice with
      | `Details ->
          (* Show a simple info modal with detected configuration *)
          let cfg = ext.External_service.config in
          let lines =
            [
              Printf.sprintf "Unit: %s" cfg.unit_name;
              Printf.sprintf "State: %s" cfg.unit_state.active_state;
              (match cfg.binary_path.value with
              | Some b -> Printf.sprintf "Binary: %s" b
              | None -> "Binary: (not detected)");
              (match cfg.data_dir.value with
              | Some d -> Printf.sprintf "Data dir: %s" d
              | None -> "Data dir: (not detected)");
              (match cfg.rpc_addr.value with
              | Some r -> Printf.sprintf "RPC: %s" r
              | None -> "");
              (match cfg.node_endpoint.value with
              | Some e -> Printf.sprintf "Node endpoint: %s" e
              | None -> "");
              (match cfg.network.value with
              | Some n -> Printf.sprintf "Network: %s" n
              | None -> "");
            ]
            |> List.filter (fun s -> s <> "")
          in
          Modal_helpers.open_text_modal
            ~title:("Details · " ^ display_name)
            ~lines
      | `Start ->
          run_unit_action ~verb:"start" ~instance:display_name (fun () ->
              Systemd.start_unit ~unit_name)
      | `Stop ->
          run_unit_action ~verb:"stop" ~instance:display_name (fun () ->
              Systemd.stop_unit ~unit_name)
      | `Restart ->
          run_unit_action ~verb:"restart" ~instance:display_name (fun () ->
              Systemd.restart_unit ~unit_name)
      | `Logs ->
          (* Set up log viewing for external service *)
          Context.set_pending_external_unit unit_name ;
          Context.navigate Log_viewer_page.name)
    () ;
  state

let activate_selection s =
  if s.selected = 0 then create_menu_modal s
  else
    match current_service s with
    | Some _ -> instance_actions_modal s
    | None -> (
        (* Check if it's an external service *)
        match current_external_service s with
        | Some ext -> external_service_actions_modal s ext
        | None -> s)

let dismiss_failure s =
  match current_service s with
  | Some st ->
      let instance = st.Service_state.service.Service.instance in
      clear_failure ~instance ;
      Context.toast_info (Printf.sprintf "Cleared failure for %s" instance) ;
      Context.mark_instances_dirty () ;
      s
  | None -> s
