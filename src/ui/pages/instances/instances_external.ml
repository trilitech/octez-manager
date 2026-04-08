(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Actions for external (unmanaged) services *)

open Octez_manager_lib
open Instances_state
open Instances_helpers

let current_external_service s =
  let display_items = display_ordered_items s in
  let external_start_idx = services_start_idx + List.length display_items in
  if s.selected >= external_start_idx then
    let ext_idx = s.selected - external_start_idx in
    List.nth_opt s.external_services ext_idx
  else None

let external_service_actions_modal state ext =
  let unit_name = ext.External_service.config.unit_name in
  let display_name = ext.External_service.suggested_instance_name in
  (* Check if this is a standalone process (not a systemd service) *)
  let is_standalone_process = String.starts_with ~prefix:"process-" unit_name in
  (* For standalone processes, only show Details and Import *)
  (* For systemd services, show all actions including Import *)
  let items =
    if is_standalone_process then [`Details; `Import]
    else [`Details; `Import; `Start; `Stop; `Restart; `Logs]
  in
  (* For standalone processes, show modal with Details and Import *)
  if is_standalone_process then (
    Modal_helpers.open_choice_modal
      ~title:("Standalone Process · " ^ display_name)
      ~items
      ~to_string:(function
        | `Details -> "Details" | `Import -> "Import to Managed" | _ -> "")
      ~on_select:(fun choice ->
        match choice with
        | `Import ->
            (* Standalone processes cannot be imported (systemd only) *)
            let lines =
              [
                "⚠ Cannot import standalone processes";
                "";
                "Only systemd-managed services can be imported.";
                "";
                "This is a standalone process (not managed by systemd).";
                "To manage it, you'll need to:";
                "  1. Stop the process manually";
                "  2. Install a managed service with octez-manager";
                "  3. Migrate any configuration/data if needed";
              ]
            in
            Modal_helpers.open_text_modal
              ~title:("Cannot Import · " ^ display_name)
              ~lines
        | `Details ->
            let cfg = ext.External_service.config in
            let lines =
              [
                Printf.sprintf
                  "PID: %s"
                  (String.sub unit_name 8 (String.length unit_name - 8));
                (* extract PID from "process-12345" *)
                Printf.sprintf "State: running";
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
        | _ -> ())
      () ;
    state)
  else (
    (* Systemd service: show action modal *)
    Modal_helpers.open_choice_modal
      ~title:("Unmanaged Service · " ^ display_name)
      ~items
      ~to_string:(function
        | `Details -> "Details"
        | `Import -> "Import to Managed"
        | `Start -> "Start"
        | `Stop -> "Stop"
        | `Restart -> "Restart"
        | `Logs -> "View Logs")
      ~on_select:(fun choice ->
        match choice with
        | `Import ->
            (* Navigate to import wizard *)
            Context.navigate Import_wizard.name
        | `Details ->
            (* Show a simple info modal with detected configuration *)
            let cfg = ext.External_service.config in
            (* Get dependencies and dependents *)
            let dependencies =
              External_service.get_dependencies ext state.external_services
            in
            let dependents =
              External_service.get_dependents ext state.external_services
            in
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
                (match cfg.dal_endpoint.value with
                | Some e -> Printf.sprintf "DAL endpoint: %s" e
                | None -> "");
                (match cfg.network.value with
                | Some n -> Printf.sprintf "Network: %s" n
                | None -> "");
              ]
              |> List.filter (fun s -> s <> "")
            in
            (* Add dependencies section *)
            let dep_lines =
              if dependencies = [] then []
              else
                "" :: "Dependencies (via endpoints):"
                :: List.map
                     (fun (unit_name, role) ->
                       Printf.sprintf "  → %s (%s)" unit_name role)
                     dependencies
            in
            (* Add dependents section *)
            let dependent_lines =
              if dependents = [] then []
              else
                "" :: "Dependents (services that use this):"
                :: List.map
                     (fun (unit_name, role) ->
                       Printf.sprintf "  ← %s (%s)" unit_name role)
                     dependents
            in
            let all_lines = lines @ dep_lines @ dependent_lines in
            Modal_helpers.open_text_modal
              ~title:("Details · " ^ display_name)
              ~lines:all_lines
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
            Context.set_pending_external_service ext ;
            Context.navigate Log_viewer_page.name)
      () ;
    state)
