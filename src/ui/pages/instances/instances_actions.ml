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

let instance_actions_modal state =
  with_service state (fun svc_state ->
      let svc = svc_state.Service_state.service in
      let is_node = svc.Service.role = "node" in
      let base_items =
        [
          `Details;
          `Edit;
          `Start;
          `Stop;
          `Restart;
          `Update_version;
          `Logs;
          `Export_logs;
          `Remove;
        ]
      in
      let items = if is_node then `Browse_rpc :: base_items else base_items in
      Modal_helpers.open_choice_modal
        ~title:("Actions · " ^ svc.Service.instance)
        ~items
        ~to_string:(function
          | `Browse_rpc -> "Browse RPC"
          | `Details -> "Details"
          | `Edit -> "Edit"
          | `Start -> "Start"
          | `Stop -> "Stop"
          | `Restart -> "Restart"
          | `Update_version -> "Update Version"
          | `Logs -> "View Logs"
          | `Export_logs -> "Export Logs"
          | `Remove -> "Remove")
        ~on_select:(fun choice ->
          let instance = svc.Service.instance in
          let role = svc.Service.role in
          match choice with
          | `Browse_rpc -> Context.navigate Rpc_browser.name
          | `Details ->
              Context.set_pending_instance_detail instance ;
              Context.navigate Instance_details.name
          | `Edit -> Instances_lifecycle.confirm_edit_modal svc
          | `Start -> Instances_lifecycle.start_with_cascade ~instance ~role
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
          | `Restart -> Instances_lifecycle.restart_with_cascade ~instance ~role
          | `Update_version -> Instances_update.update_version_modal svc
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
    ~items:[`Node; `DalNode; `Baker; `Accuser; `Signatory]
    ~to_string:(function
      | `Node -> "Node"
      | `DalNode -> "DAL Node"
      | `Baker -> "Baker"
      | `Accuser -> "Accuser"
      | `Signatory -> "Signatory")
    ~on_select:(function
      | `Node -> Context.navigate Install_node_form_v3.name
      | `Baker -> Context.navigate Install_baker_form_v3.name
      | `Accuser -> Context.navigate Install_accuser_form_v3.name
      | `DalNode -> Context.navigate Install_dal_node_form_v3.name
      | `Signatory -> Context.navigate Signatory_info.name)
    () ;
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
  match s.selected with
  | 0 -> create_menu_modal s
  | 1 -> go_to_binaries s
  | 2 -> go_to_rpc_browser s
  | _ -> (
      match current_service s with
      | Some _ -> instance_actions_modal s
      | None -> (
          (* Check if it's an external service *)
          match Instances_external.current_external_service s with
          | Some ext -> Instances_external.external_service_actions_modal s ext
          | None -> s))

let dismiss_failure s =
  match current_service s with
  | Some st ->
      let instance = st.Service_state.service.Service.instance in
      clear_failure ~instance ;
      Context.toast_info (Printf.sprintf "Cleared failure for %s" instance) ;
      Context.mark_instances_dirty () ;
      s
  | None -> s

module For_tests = struct
  let journalctl_args = journalctl_args

  let current_external_service = Instances_external.current_external_service
end
