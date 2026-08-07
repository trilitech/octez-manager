(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Shared helpers for instances action modules *)

open Octez_manager_lib
open Instances_state

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
          (* Force immediate data refresh for removal/purge operations *)
          if verb = "remove" || verb = "purge" then Data.force_refresh () ;
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
