(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Side-effecting action handlers for the binaries page.

    Handles version removal, directory unregistration, downloads,
    directory registration, pruning, and dispatching actions based
    on the selected item. *)

open Octez_manager_lib
open Binaries_types

(** Show confirm modal and remove a managed version in background.
    Refuses removal if the version is used by any instances. *)
let remove_version version =
  let bin_source = Binary_registry.Managed_version version in
  let count = Service_registry.count_instances_using bin_source in
  if count > 0 then
    Modal_helpers.show_error
      ~title:"Version In Use"
      (Printf.sprintf
         "Version v%s is used by %d instance(s). Cannot remove."
         version
         count)
  else
    Modal_helpers.confirm_modal
      ~title:(Printf.sprintf "Remove managed version v%s?" version)
      ~message:""
      ~on_result:(fun confirmed ->
        if confirmed then
          (* Run removal in background to avoid blocking UI *)
          Background_runner.enqueue (fun () ->
              match Binary_downloader.remove_version version with
              | Ok () ->
                  Context.toast_success (Printf.sprintf "Removed v%s" version) ;
                  Context.mark_instances_dirty ()
              | Error (`Msg msg) ->
                  Context.toast_error (Printf.sprintf "Remove failed: %s" msg)))
      ()

(** Show confirm modal and unregister a directory in background.
    Refuses unregistration if the directory is used by any instances. *)
let unregister_directory ld =
  let count =
    Service_registry.count_instances_using
      (Binary_registry.Registered_alias ld.Binary_registry.alias)
  in
  if count > 0 then
    Modal_helpers.show_error
      ~title:"Directory In Use"
      (Printf.sprintf
         "Registered directory '%s' is used by %d instance(s). Cannot \
          unregister."
         ld.alias
         count)
  else
    Modal_helpers.confirm_modal
      ~title:(Printf.sprintf "Unregister directory '%s'?" ld.alias)
      ~message:""
      ~on_result:(fun confirmed ->
        if confirmed then
          (* Run unregister in background to avoid blocking UI *)
          Background_runner.enqueue (fun () ->
              match Binary_registry.remove_registered_dir ld.alias with
              | Ok () ->
                  Context.toast_success
                    (Printf.sprintf "Unregistered '%s'" ld.alias) ;
                  Context.mark_instances_dirty ()
              | Error (`Msg msg) ->
                  Context.toast_error
                    (Printf.sprintf "Unregister failed: %s" msg)))
      ()

(** Download a version in background with multi-progress UI. *)
let download_version (version_info : Binary_downloader.version_info) =
  (* Start download in background *)
  Background_runner.enqueue (fun () ->
      let version = version_info.Binary_downloader.version in
      Context.toast_info (Printf.sprintf "Downloading v%s..." version) ;

      (* Initialize multi-progress with list of binaries *)
      Context.multi_progress_start
        ~version
        ~binaries:
          ["octez-node"; "octez-client"; "octez-baker"; "octez-dal-node"] ;

      (* Multi-progress callback *)
      let multi_progress (mp : Binary_downloader.multi_progress_state) =
        Context.multi_progress_update
          ~binary:mp.current_file
          ~downloaded:mp.downloaded
          ~total:mp.total
      in

      let result =
        Binary_downloader.download_version
          ~version
          ~verify_checksums:true
          ~multi_progress
          ()
      in

      (* Handle checksums *)
      match result with
      | Ok res ->
          Context.multi_progress_checksum "Verifying checksums..." ;
          Unix.sleepf 0.5 ;
          (match res.Binary_downloader.checksum_status with
          | Binary_downloader.Verified ->
              Context.multi_progress_checksum
                "\xe2\x9c\x93 All checksums verified"
          | Binary_downloader.Skipped ->
              Context.multi_progress_checksum
                "\xe2\x9a\xa0 Checksum verification skipped"
          | Binary_downloader.Failed reason ->
              Context.multi_progress_checksum
                (Printf.sprintf "\xe2\x9c\x97 Failed: %s" reason)) ;
          Unix.sleepf 2.0 ;
          (* Linger to show final status *)
          Context.multi_progress_finish () ;
          Context.toast_success (Printf.sprintf "Downloaded v%s" version) ;
          Context.mark_instances_dirty ()
      | Error (`Msg msg) ->
          Context.multi_progress_finish () ;
          Context.toast_error (Printf.sprintf "Download failed: %s" msg))

(** Open file browser modal to register a new binary directory. *)
let register_directory () =
  Modal_helpers.open_file_browser_modal
    ~dirs_only:true
    ~require_writable:false
    ~on_select:(fun path ->
      let alias = Filename.basename path in
      match Binary_registry.add_registered_dir ~alias ~path with
      | Ok () ->
          Context.toast_success (Printf.sprintf "Registered '%s'" alias) ;
          Context.mark_instances_dirty ()
      | Error (`Msg msg) ->
          Modal_helpers.show_error ~title:"Register Failed" msg)
    ()

(** Calculate unused versions, show confirm modal, bulk-remove them. *)
let prune_unused s =
  let unused =
    List.filter (fun (_v, _s, count) -> count = 0) s.managed_versions
  in
  if unused = [] then (
    Modal_helpers.show_error ~title:"Prune" "No unused versions to prune." ;
    s)
  else
    (* Calculate total size and build detailed message *)
    let version_details, total_bytes =
      List.fold_left
        (fun (details, bytes) (v, _, _) ->
          match Binary_downloader.get_version_size v with
          | Ok (size_bytes, formatted) ->
              ((v, formatted) :: details, Int64.add bytes size_bytes)
          | Error _ -> ((v, "unknown size") :: details, bytes))
        ([], 0L)
        unused
    in
    let details_lines =
      List.map
        (fun (v, size) -> Printf.sprintf "  \xe2\x80\xa2 v%s (%s)" v size)
        (List.rev version_details)
    in
    let total_formatted = Binary_downloader.format_size_bytes total_bytes in
    let message =
      String.concat
        "\n"
        (["The following versions will be removed:"; ""]
        @ details_lines
        @ [""; Printf.sprintf "Total space to free: %s" total_formatted])
    in
    Modal_helpers.confirm_modal
      ~title:(Printf.sprintf "Prune %d unused version(s)?" (List.length unused))
      ~message
      ~on_result:(fun confirmed ->
        if confirmed then (
          let success_count, fail_count =
            List.fold_left
              (fun (succ, fail) (v, _, _) ->
                match Binary_downloader.remove_version v with
                | Ok () -> (succ + 1, fail)
                | Error _ -> (succ, fail + 1))
              (0, 0)
              unused
          in
          if fail_count = 0 then
            Context.toast_info
              (Printf.sprintf
                 "Removed %d version(s), freed %s"
                 success_count
                 total_formatted)
          else
            Context.toast_error
              (Printf.sprintf
                 "Removed %d version(s), %d failed"
                 success_count
                 fail_count) ;
          Context.mark_instances_dirty ()))
      () ;
    s

(** Dispatch Enter key based on selected item type. *)
let handle_action ~toggle_managed_expansion ~toggle_registered_expansion
    ~toggle_major_expansion s =
  if s.items = [] then s
  else
    let item = List.nth s.items s.selected in
    match item with
    | ManagedVersion (version, _, count) ->
        if count > 0 then
          (* If has instances, toggle expansion *)
          toggle_managed_expansion s version
        else (
          (* If unused, allow removal *)
          remove_version version ;
          s)
    | RegisteredDir (ld, count) ->
        if count > 0 then
          (* If has instances, toggle expansion *)
          toggle_registered_expansion s ld.Binary_registry.alias
        else (
          (* If unused, allow unregistering *)
          unregister_directory ld ;
          s)
    | RegisterAction ->
        register_directory () ;
        s
    | AvailableVersion vi ->
        download_version vi ;
        s
    | AvailableMajorGroup (major, _) ->
        (* Toggle expansion on Enter *)
        toggle_major_expansion s major

(** Dispatch Tab key to toggle expansion of current item. *)
let toggle_current_group ~toggle_managed_expansion ~toggle_registered_expansion
    ~toggle_major_expansion s =
  if s.items = [] then s
  else
    let item = List.nth s.items s.selected in
    match item with
    | AvailableMajorGroup (major, _) -> toggle_major_expansion s major
    | ManagedVersion (version, _, count) ->
        if count > 0 then toggle_managed_expansion s version else s
    | RegisteredDir (ld, count) ->
        if count > 0 then toggle_registered_expansion s ld.Binary_registry.alias
        else s
    | _ -> s
