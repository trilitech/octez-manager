(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

open Rresult
open Octez_manager_lib
module Style_context = Miaou_style.Style_context

let ( let* ) = Result.bind

let register_pages () =
  Instances.register () ;
  Instance_details.register () ;
  Install_node_form_v3.register () ;
  Install_baker_form_v3.register () ;
  Install_accuser_form_v3.register () ;
  Install_dal_node_form_v3.register () ;
  Install_signatory_form.register () ;
  Import_wizard.register () ;
  Binaries.register () ;
  Diagnostics.register () ;
  Log_viewer_page.register () ;
  Rpc_node_selection.register () ;
  Rpc_browser.register () ;
  Topology_page.register () ;
  Keys_page.register () ;
  Rewards_page.register () ;
  Sandbox_page.register () ;
  Sandbox_create_form.register () ;
  Sandbox_key_alloc_page.register ()

let find_page_or_default name default_name =
  let module Registry = Miaou.Core.Registry in
  match Registry.find name with
  | Some page -> Ok page
  | None -> (
      match Registry.find default_name with
      | Some page ->
          (prerr_endline [@allow_forbidden "startup warning before TUI init"])
            (Printf.sprintf
               "Unknown page '%s', falling back to '%s'"
               name
               default_name) ;
          Ok page
      | None -> Error (`Msg "Instances page missing from registry"))

let register_and_init ?(log = false) ?logfile () =
  Capabilities.register () ;
  register_pages () ;
  Runtime.initialize ~log ?logfile () ;
  Binary_downloader.cleanup_stale_temp_dirs () ;
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
  Self_update_scheduler.start () ;
  Rewards_scheduler.start () ;
  Background_runner.enqueue (fun () ->
      Self_update_scheduler.check_now () ;
      if Self_update_scheduler.update_available () then
        match Self_update_scheduler.get_latest_version () with
        | Some version ->
            Context.toast_info
              (Printf.sprintf
                 "octez-manager %s is available. Run 'octez-manager \
                  self-update' to upgrade."
                 version)
        | None -> ())

let shutdown () =
  Background_runner.shutdown () ;
  Rpc_scheduler.shutdown () ;
  Delegate_scheduler.shutdown () ;
  System_metrics_scheduler.shutdown () ;
  External_services_scheduler.shutdown () ;
  Rewards_scheduler.shutdown () ;
  Versions_scheduler.shutdown () ;
  Self_update_scheduler.stop () ;
  Domain_pool.shutdown () ;
  Download.kill_active_download ()

(** Open theme picker modal with live preview *)
let open_theme_picker () =
  let items = Theme_manager.list_available () in
  (* Remember current theme to restore on cancel *)
  let original_theme = Theme_manager.get_current () in
  let load_theme name =
    let theme, _warn = Theme_manager.load ~name () in
    Theme_manager.set_current theme ;
    Style_context.set_theme theme
  in
  Modal_helpers.open_theme_picker_modal
    ~title:"Switch Theme (Ctrl+T)"
    ~items
    ~to_string:(fun s -> s)
    ~load_theme
    ~on_select:(fun name ->
      (* Theme already applied via live preview, just save preference and notify *)
      Theme_manager.save_preference name ;
      Context.toast_info (Printf.sprintf "Switched to theme: %s" name))
    ~on_cancel:(fun () ->
      (* Restore the original theme on Esc *)
      Theme_manager.set_current original_theme ;
      Style_context.set_theme original_theme)
    ()

(** Register global key handler for Ctrl+T *)
let register_global_keys () =
  Context.register_global_key "C-t" (fun () -> open_theme_picker ()) ;
  Context.register_global_key "K" (fun () -> Context.navigate Keys_page.name) ;
  Context.register_global_key "R" (fun () -> Context.navigate Rewards_page.name)

let run ?page ?(log = false) ?logfile ?theme () =
  let initial_theme, warning = Theme_manager.load ?name:theme () in
  Theme_manager.set_current initial_theme ;
  let quit_requested = ref false in
  let handle_break _ =
    quit_requested := true ;
    raise_notrace Exit
  in
  Sys.catch_break true ;
  Sys.set_signal Sys.sigint (Sys.Signal_handle handle_break) ;
  Sys.set_signal Sys.sigterm (Sys.Signal_handle handle_break) ;
  register_and_init ~log ?logfile () ;
  register_global_keys () ;
  (match warning with Some msg -> Context.toast_warn msg | None -> ()) ;
  let start_name = Option.value ~default:Instances.name page in
  let rec loop history current_name =
    if !quit_requested then raise Exit
    else
      let* current_page = find_page_or_default current_name Instances.name in
      let result =
        Miaou_runner_tui.Runner_tui.run
          ~enable_mouse:true
          ~handle_sigint:false
          current_page
      in
      match result with
      | `Quit -> raise Exit
      | `Back | `SwitchTo "__BACK__" -> (
          match history with [] -> raise Exit | prev :: rest -> loop rest prev)
      | `SwitchTo next_page -> loop (current_name :: history) next_page
  in
  try loop [] start_name
  with Exit | Sys.Break ->
    shutdown () ;
    Ok ()
