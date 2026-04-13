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
open Instances_helpers

let ( let* ) = Result.bind

(** Track export operations to prevent concurrent exports for the same instance *)
let export_in_progress : (string, bool) Hashtbl.t = Hashtbl.create 17

let export_lock = Mutex.create ()

let do_remove ~instance ~delete_data_dir () =
  Rpc_scheduler.stop_head_monitor instance ;
  let* (module I) = require_installer () in
  I.remove_service ~quiet:true ~delete_data_dir ~instance ()

let do_purge ~instance ~force_purge () =
  Rpc_scheduler.stop_head_monitor instance ;
  let* (module I) = require_installer () in
  I.purge_service
    ~quiet:true
    ~force_purge
    ~prompt_yes_no:(fun _ ~default:_ -> true)
    ~instance
    ()

(** Check if base-dir should be deleted and show appropriate modals.
    For bakers/accusers: check if other instances share the same base-dir.
    - If shared: show warning modal (non-blocking)
    - If not shared: show confirmation modal, then purge on confirm *)
let purge_with_base_dir_check ~instance () =
  match Service_registry.find ~instance with
  | Error _ ->
      Modal_helpers.show_error
        ~title:"Purge Error"
        (Printf.sprintf "Instance '%s' not found" instance)
  | Ok None ->
      Modal_helpers.show_error
        ~title:"Purge Error"
        (Printf.sprintf "Instance '%s' not found" instance)
  | Ok (Some svc) ->
      let is_baker = svc.Service.role = "baker" in
      let is_accuser = svc.Service.role = "accuser" in
      if is_baker || is_accuser then
        (* Read the base_dir from env file *)
        let env =
          match Node_env.read ~inst:instance with
          | Ok pairs -> pairs
          | Error _ -> []
        in
        let base_dir_opt =
          List.assoc_opt
            (if is_baker then "OCTEZ_BAKER_BASE_DIR"
             else "OCTEZ_CLIENT_BASE_DIR")
            env
        in
        match base_dir_opt with
        | None ->
            (* No base_dir found, just purge directly *)
            run_unit_action ~verb:"purge" ~instance (fun () ->
                do_purge ~instance ~force_purge:true ())
        | Some base_dir ->
            (* Check if other instances use this base_dir *)
            let other_users =
              Removal.get_base_dir_users ~instance ~base_dir ()
            in
            if other_users <> [] then (
              (* Base-dir is shared - show warning and purge without deleting base-dir *)
              let warning_message =
                Printf.sprintf
                  "Base directory is shared with other instances:\n\n\
                   %s\n\n\
                   The base directory will NOT be deleted.\n\
                   Used by: %s"
                  base_dir
                  (String.concat ", " other_users)
              in
              Modal_helpers.open_text_modal
                ~title:"⚠ Shared Base Directory"
                ~lines:(String.split_on_char '\n' warning_message) ;
              run_unit_action ~verb:"purge" ~instance (fun () ->
                  do_purge ~instance ~force_purge:false ()))
            else
              (* Base-dir not shared - show confirmation modal *)
              Modal_helpers.open_choice_modal
                ~title:"Confirm Base Directory Deletion"
                ~items:[`DeleteAndPurge; `SkipBaseDirDeletion]
                ~to_string:(function
                  | `DeleteAndPurge ->
                      Printf.sprintf "Delete base-dir and purge: %s" base_dir
                  | `SkipBaseDirDeletion -> "Skip base-dir deletion")
                ~on_select:(function
                  | `DeleteAndPurge ->
                      run_unit_action ~verb:"purge" ~instance (fun () ->
                          do_purge ~instance ~force_purge:true ())
                  | `SkipBaseDirDeletion ->
                      run_unit_action ~verb:"purge" ~instance (fun () ->
                          do_purge ~instance ~force_purge:false ()))
                ()
      else
        (* Not a baker/accuser - just purge directly *)
        run_unit_action ~verb:"purge" ~instance (fun () ->
            do_purge ~instance ~force_purge:true ())

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
      | `Confirm -> purge_with_base_dir_check ~instance () | `Cancel -> ())
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
              if dependents = [] then purge_with_base_dir_check ~instance ()
              else purge_with_dependents_confirm ~instance ~dependents)
        () ;
      state)

let journalctl_args unit_name =
  if Paths.is_root () then
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
        | "signatory" ->
            (* Signatory: <base_dir>/logs/signatory/ *)
            let base =
              Option.value
                (lookup "SIGNATORY_BASE_DIR")
                ~default:svc.Service.data_dir
            in
            Filename.concat (Filename.concat base "logs") "signatory"
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
        match Cmd_runner.run_out ["tail"; "-n"; "200"; path] with
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
        match Cmd_runner.run_out (journalctl_args unit) with
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

let add_to_group_modal (svc : Service.t) =
  let svc_net = Instances_render.network_short svc.Service.network in
  let groups =
    match Group_registry.list () with Ok gs -> gs | Error _ -> []
  in
  let compatible_groups =
    List.filter
      (fun (g : Group.t) ->
        String.equal (Instances_render.network_short g.network) svc_net)
      groups
  in
  let items =
    [`Create_new] @ List.map (fun g -> `Existing g) compatible_groups
  in
  Modal_helpers.open_choice_modal
    ~title:("Add to Group · " ^ svc.Service.instance)
    ~items
    ~to_string:(function
      | `Create_new -> "+ Create New Group"
      | `Existing (g : Group.t) -> Printf.sprintf "%s (%s)" g.name g.network)
    ~on_select:(function
      | `Create_new ->
          let initial =
            Instances_render.network_short svc.Service.network ^ "-"
          in
          Modal_helpers.prompt_validated_text_modal
            ~title:"New Group Name"
            ~initial
            ~placeholder:(Some "e.g. mainnet-prod")
            ~validator:(fun name ->
              if String.length name = 0 then Error "Name cannot be empty"
              else
                match Group_registry.find ~name with
                | Ok (Some _) ->
                    Error (Printf.sprintf "Group '%s' already exists" name)
                | _ -> Ok ())
            ~on_submit:(fun name ->
              let grp =
                Group.make
                  ~name
                  ~network:svc.Service.network
                  ~bin_source:(Service.get_bin_source svc)
                  ~service_user:svc.Service.service_user
                  ~app_bin_dir:svc.Service.app_bin_dir
                  ()
              in
              match Group_registry.write grp with
              | Ok () -> (
                  match Service_registry.write {svc with group = Some name} with
                  | Ok () ->
                      Context.toast_info
                        (Printf.sprintf
                           "Created group '%s' and added %s"
                           name
                           svc.Service.instance) ;
                      Context.mark_instances_dirty ()
                  | Error (`Msg e) -> Context.toast_error e)
              | Error (`Msg e) -> Context.toast_error e)
            ()
      | `Existing (grp : Group.t) -> (
          match Service_registry.write {svc with group = Some grp.name} with
          | Ok () ->
              Context.toast_info
                (Printf.sprintf
                   "Added %s to group '%s'"
                   svc.Service.instance
                   grp.name) ;
              Context.mark_instances_dirty ()
          | Error (`Msg e) -> Context.toast_error e))
    ()

let remove_from_group (svc : Service.t) =
  match svc.Service.group with
  | None -> Context.toast_info "Service is not in any group"
  | Some gname -> (
      match Service_registry.write {svc with group = None} with
      | Ok () ->
          (* Auto-remove group if no services remain in it *)
          let group_still_used =
            match Service_registry.list () with
            | Ok svcs ->
                List.exists
                  (fun (s : Service.t) ->
                    Option.equal String.equal s.group (Some gname)
                    && not (String.equal s.instance svc.instance))
                  svcs
            | Error _ -> true
          in
          if not group_still_used then
            ignore (Group_registry.remove ~name:gname) ;
          Context.toast_info
            (Printf.sprintf
               "Removed %s from group '%s'"
               svc.Service.instance
               gname) ;
          Context.mark_instances_dirty ()
      | Error (`Msg e) -> Context.toast_error e)

let instance_actions_modal state =
  with_service state (fun svc_state ->
      let svc = svc_state.Service_state.service in
      let is_node = svc.Service.role = "node" in
      let is_baker = String.equal svc.Service.role "baker" in
      let in_group = Option.is_some svc.Service.group in
      let base_items =
        (if is_baker then [`Wallet] else [])
        @ [`Details; `Edit; `Start; `Stop; `Restart; `Update_version]
        @ [`Add_to_group]
        @ (if in_group then [`Remove_from_group] else [])
        @ [`Logs; `Export_logs; `Remove]
      in
      let items = if is_node then `Browse_rpc :: base_items else base_items in
      Modal_helpers.open_choice_modal
        ~title:("Actions · " ^ svc.Service.instance)
        ~items
        ~to_string:(function
          | `Wallet -> "Wallet"
          | `Browse_rpc -> "Browse RPC"
          | `Details -> "Details"
          | `Edit -> "Edit"
          | `Start -> "Start"
          | `Stop -> "Stop"
          | `Restart -> "Restart"
          | `Update_version -> "Update Version"
          | `Add_to_group -> "Add to Group"
          | `Remove_from_group ->
              Printf.sprintf
                "Remove from Group (%s)"
                (Option.value ~default:"" svc.Service.group)
          | `Logs -> "View Logs"
          | `Export_logs -> "Export Logs"
          | `Remove -> "Remove")
        ~on_select:(fun choice ->
          let instance = svc.Service.instance in
          let role = svc.Service.role in
          match choice with
          | `Wallet -> Instances_wallet.wallet_modal ~svc
          | `Browse_rpc -> Context.navigate Rpc_browser.name
          | `Details ->
              Context.set_pending_instance_detail instance ;
              Context.navigate Instance_details.name
          | `Edit -> Instances_lifecycle.confirm_edit_modal svc
          | `Start -> Instances_lifecycle.start_with_cascade ~instance ~role
          | `Stop ->
              clear_failure ~instance ;
              run_unit_action ~verb:"stop" ~instance (fun () ->
                  let cap = Miaou_interfaces.Service_lifecycle.require () in
                  Miaou_interfaces.Service_lifecycle.stop
                    cap
                    ~role
                    ~service:instance
                  |> Result.map_error (fun e -> `Msg e))
          | `Restart -> Instances_lifecycle.restart_with_cascade ~instance ~role
          | `Update_version -> Instances_update.update_version_modal svc
          | `Add_to_group -> add_to_group_modal svc
          | `Remove_from_group -> remove_from_group svc
          | `Logs ->
              Context.set_pending_instance_detail instance ;
              Context.navigate Log_viewer_page.name
          | `Export_logs ->
              let already_running =
                Mutex.protect export_lock (fun () ->
                    match Hashtbl.find_opt export_in_progress instance with
                    | Some true -> true
                    | _ ->
                        Hashtbl.replace export_in_progress instance true ;
                        false)
              in
              if already_running then
                Context.toast_warn
                  (Printf.sprintf "Export already in progress for %s" instance)
              else
                Modal_helpers.open_export_logs_modal
                  ~instance
                  ~svc
                  ~on_complete:(fun _result ->
                    Mutex.protect export_lock (fun () ->
                        Hashtbl.remove export_in_progress instance) ;
                    Context.mark_instances_dirty ())
          | `Remove -> remove_modal state |> ignore)
        () ;
      state)

let open_create_menu () =
  let open Modal_helpers in
  open_choice_modal
    ~title:"Create"
    ~items:[`Node; `DalNode; `Baker; `Accuser; `Signatory; `Index]
    ~to_string:(function
      | `Node -> "Node"
      | `DalNode -> "DAL Node"
      | `Baker -> "Baker"
      | `Accuser -> "Accuser"
      | `Signatory -> "Signatory"
      | `Index -> "Indexer")
    ~on_select:(function
      | `Node -> Context.navigate Install_node_form_v3.name
      | `Baker -> Context.navigate Install_baker_form_v3.name
      | `Accuser -> Context.navigate Install_accuser_form_v3.name
      | `DalNode -> Context.navigate Install_dal_node_form_v3.name
      | `Signatory -> Context.navigate Install_signatory_form.name
      | `Index -> Context.navigate Install_index_form_v3.name)
    ()

let create_menu_modal state =
  open_create_menu () ;
  state

let go_to_diagnostics state =
  Context.navigate Diagnostics.name ;
  state

let go_to_topology state =
  Context.navigate Topology_page.name ;
  state

let go_to_binaries state =
  Context.navigate Binaries.name ;
  state

let go_to_rpc_browser state =
  Context.navigate Rpc_node_selection.name ;
  state

let activate_selection s =
  (* Check what's selected (services_start_idx = 0, so always >= 0) *)
  let display_items = display_ordered_items s in
  match List.nth_opt display_items (s.selected - services_start_idx) with
  | Some (Real_service _) -> instance_actions_modal s
  | Some (Ghost_add_new role) ->
      (* Navigate to the appropriate form based on role *)
      (match role with
      | "node" -> Context.navigate Install_node_form_v3.name
      | "baker" -> Context.navigate Install_baker_form_v3.name
      | "accuser" -> Context.navigate Install_accuser_form_v3.name
      | "dal-node" -> Context.navigate Install_dal_node_form_v3.name
      | "signatory" -> Context.navigate Install_signatory_form.name
      | "index" -> Context.navigate Install_index_form_v3.name
      | _ -> ()) ;
      s
  | None -> (
      (* Check if it's an external service *)
      match Instances_external.current_external_service s with
      | Some ext -> Instances_external.external_service_actions_modal s ext
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

let group_action_modal (grp : Group.t) =
  Modal_helpers.open_choice_modal
    ~title:(Printf.sprintf "Group · %s" grp.name)
    ~items:[`Start; `Stop; `Restart]
    ~to_string:(function
      | `Start -> "Start all" | `Stop -> "Stop all" | `Restart -> "Restart all")
    ~on_select:(fun choice ->
      let verb, action =
        match choice with
        | `Start ->
            ( "start",
              fun () ->
                Lifecycle.start_group ~quiet:true ~group_name:grp.name ()
                |> Result.map (fun _ -> ()) )
        | `Stop ->
            ( "stop",
              fun () ->
                Lifecycle.stop_group ~quiet:true ~group_name:grp.name ()
                |> Result.map (fun _ -> ()) )
        | `Restart ->
            ( "restart",
              fun () ->
                Lifecycle.restart_group ~quiet:true ~group_name:grp.name ()
                |> Result.map (fun _ -> ()) )
      in
      run_unit_action
        ~verb:(Printf.sprintf "%s group %s" verb grp.name)
        ~instance:grp.name
        action)
    ()

let group_actions_modal state =
  (* If selected service is in a group, jump directly to that group's actions *)
  let direct_group =
    match current_service state with
    | Some st -> st.Service_state.service.Service.group
    | None -> None
  in
  match direct_group with
  | Some gname -> (
      match Group_registry.find ~name:gname with
      | Ok (Some grp) ->
          group_action_modal grp ;
          state
      | _ -> state)
  | None -> (
      (* No group context — show group selector *)
      let groups = state.groups in
      match groups with
      | [] ->
          Context.toast_info "No groups configured" ;
          state
      | _ ->
          Modal_helpers.open_choice_modal
            ~title:"Select Group"
            ~items:groups
            ~to_string:(fun (g : Group.t) ->
              Printf.sprintf "%s (%s)" g.name g.network)
            ~on_select:(fun grp -> group_action_modal grp)
            () ;
          state)

module For_tests = struct
  let journalctl_args = journalctl_args

  let current_external_service = Instances_external.current_external_service
end
