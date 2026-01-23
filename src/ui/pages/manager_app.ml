(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

open Rresult
open Octez_manager_lib

let ( let* ) = Result.bind

let register_pages () =
  Instances.register () ;
  Instance_details.register () ;
  Install_node_form_v3.register () ;
  Install_baker_form_v3.register () ;
  Install_accuser_form_v3.register () ;
  Install_dal_node_form_v3.register () ;
  Import_wizard.register () ;
  Binaries.register () ;
  Diagnostics.register () ;
  Log_viewer_page.register ()

let find_page_or_default name default_name =
  let module Registry = Miaou.Core.Registry in
  match Registry.find name with
  | Some page -> Ok page
  | None -> (
      match Registry.find default_name with
      | Some page ->
          prerr_endline
            (Printf.sprintf
               "Unknown page '%s', falling back to '%s'"
               name
               default_name) ;
          Ok page
      | None -> Error (`Msg "Instances page missing from registry"))

let run ?page ?(log = false) ?logfile () =
  let quit_requested = ref false in
  let handle_break _ =
    quit_requested := true ;
    raise_notrace Exit
  in
  Sys.catch_break true ;
  Sys.set_signal Sys.sigint (Sys.Signal_handle handle_break) ;
  Sys.set_signal Sys.sigterm (Sys.Signal_handle handle_break) ;
  Capabilities.register () ;
  register_pages () ;
  Runtime.initialize ~log ?logfile () ;
  (* Cleanup stale temporary download directories from interrupted sessions *)
  Binary_downloader.cleanup_stale_temp_dirs () ;
  (* Check for version updates in background *)
  Background_runner.enqueue (fun () ->
      match Version_checker.check_for_updates () with
      | Version_checker.UpdateAvailable
          {latest_version; current_version; should_notify}
        when should_notify ->
          let current_str =
            match current_version with
            | Some v -> Printf.sprintf "v%s" v
            | None -> "none"
          in
          Context.toast_info
            (Printf.sprintf
               "Octez v%s is available (you have %s). Press B to manage \
                binaries."
               latest_version
               current_str)
      | _ -> ()) ;
  let start_name = Option.value ~default:Instances.name page in
  let rec loop history current_name =
    if !quit_requested then raise Exit
    else
      let* current_page = find_page_or_default current_name Instances.name in
      let result =
        Miaou_runner_tui.Runner_tui.run ~enable_mouse:false current_page
      in
      match result with
      | `Quit -> raise Exit
      | `SwitchTo "__EXIT__" -> raise Exit
      | `SwitchTo "__BACK__" -> (
          match history with [] -> raise Exit | prev :: rest -> loop rest prev)
      | `SwitchTo next_page -> loop (current_name :: history) next_page
  in
  try loop [] start_name
  with Exit | Sys.Break ->
    (* Cleanup: shutdown all background schedulers and workers *)
    Background_runner.shutdown () ;
    Rpc_scheduler.shutdown () ;
    Delegate_scheduler.shutdown () ;
    System_metrics_scheduler.shutdown () ;
    External_services_scheduler.shutdown () ;
    Versions_scheduler.shutdown () ;
    (* Cleanup: kill any active download process *)
    Common.kill_active_download () ;
    Ok ()
