(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Service lifecycle actions: start, restart, and edit with cascade logic *)

open Octez_manager_lib
open Instances_state
open Instances_helpers

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
          (Unix.sleepf [@allow_forbidden "job delay - TODO: use Eio"]) 0.5 ;
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
                  (Unix.sleepf [@allow_forbidden "job delay - TODO: use Eio"])
                    1.0 ;
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
                                  (Unix.sleepf
                                  [@allow_forbidden
                                    "retry delay - TODO: use Eio"])
                                    2.0 ;
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
          (Unix.sleepf [@allow_forbidden "job delay - TODO: use Eio"]) 0.5 ;
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
    | "signatory" -> "install_signatory_form"
    | "index" -> "install_index_form_v3"
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
