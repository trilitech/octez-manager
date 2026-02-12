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

(** Show confirm modal and remove a managed Octez version in background.
    Refuses removal if the version is used by any instances. *)
let remove_octez_version version =
  let bin_source = Binary_registry.Managed_octez_version version in
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

(** Show confirm modal and remove a managed Signatory version in background.
    Refuses removal if the version is used by any instances. *)
let remove_signatory_version version =
  let bin_source = Binary_registry.Managed_signatory_version version in
  let count = Service_registry.count_instances_using bin_source in
  if count > 0 then
    Modal_helpers.show_error
      ~title:"Version In Use"
      (Printf.sprintf
         "Signatory version v%s is used by %d instance(s). Cannot remove."
         version
         count)
  else
    Modal_helpers.confirm_modal
      ~title:(Printf.sprintf "Remove managed Signatory version v%s?" version)
      ~message:""
      ~on_result:(fun confirmed ->
        if confirmed then
          (* Run removal in background to avoid blocking UI *)
          Background_runner.enqueue (fun () ->
              match Signatory_downloader.remove_version version with
              | Ok () ->
                  Context.toast_success
                    (Printf.sprintf "Removed Signatory v%s" version) ;
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

(** Download an Octez version in background with multi-progress UI. *)
let download_octez_version (version_info : Binary_downloader.version_info) =
  (* Start download in background *)
  Background_runner.enqueue (fun () ->
      let version = version_info.Binary_downloader.version in
      Context.toast_info (Printf.sprintf "Downloading Octez v%s..." version) ;

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
          Context.toast_success (Printf.sprintf "Downloaded Octez v%s" version) ;
          Context.mark_instances_dirty ()
      | Error (`Msg msg) ->
          Context.multi_progress_finish () ;
          Context.toast_error (Printf.sprintf "Download failed: %s" msg))

(** Download a Signatory version in background with multi-progress UI. *)
let download_signatory_version
    (version_info : Signatory_downloader.version_info) =
  (* Start download in background *)
  Background_runner.enqueue (fun () ->
      let version = version_info.Signatory_downloader.version in
      Context.toast_info (Printf.sprintf "Downloading Signatory v%s..." version) ;

      (* Initialize multi-progress with just signatory binary *)
      Context.multi_progress_start ~version ~binaries:["signatory"] ;

      (* Simple progress callback that updates multi-progress *)
      let progress ~downloaded ~total =
        Context.multi_progress_update ~binary:"signatory" ~downloaded ~total
      in

      let result =
        Signatory_downloader.download_version
          ~version
          ~verify_checksums:true
          ~progress
          ()
      in

      (* Handle checksums *)
      match result with
      | Ok res ->
          Context.multi_progress_checksum "Verifying checksums..." ;
          Unix.sleepf 0.5 ;
          (match res.Signatory_downloader.checksum_status with
          | Signatory_downloader.Verified ->
              Context.multi_progress_checksum "\xe2\x9c\x93 Checksum verified"
          | Signatory_downloader.Skipped ->
              Context.multi_progress_checksum
                "\xe2\x9a\xa0 Checksum verification skipped"
          | Signatory_downloader.Failed reason ->
              Context.multi_progress_checksum
                (Printf.sprintf "\xe2\x9c\x97 Failed: %s" reason)) ;
          Unix.sleepf 2.0 ;
          (* Linger to show final status *)
          Context.multi_progress_finish () ;
          Context.toast_success
            (Printf.sprintf "Downloaded Signatory v%s" version) ;
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

(** Calculate unused versions (both Octez and Signatory), show confirm modal, bulk-remove them. *)
let prune_unused s =
  let unused_octez =
    List.filter (fun (_v, _s, count) -> count = 0) s.managed_octez_versions
  in
  let unused_signatory =
    List.filter (fun (_v, _s, count) -> count = 0) s.managed_signatory_versions
  in
  if unused_octez = [] && unused_signatory = [] then (
    Modal_helpers.show_error ~title:"Prune" "No unused versions to prune." ;
    s)
  else
    (* Calculate total size and build detailed message *)
    let octez_details, octez_bytes =
      List.fold_left
        (fun (details, bytes) (v, _, _) ->
          match Binary_downloader.get_version_size v with
          | Ok (size_bytes, formatted) ->
              (("Octez", v, formatted) :: details, Int64.add bytes size_bytes)
          | Error _ -> (("Octez", v, "unknown size") :: details, bytes))
        ([], 0L)
        unused_octez
    in
    let signatory_details, signatory_bytes =
      List.fold_left
        (fun (details, bytes) (v, _, _) ->
          match Signatory_downloader.get_version_size v with
          | Ok (size_bytes, formatted) ->
              ( ("Signatory", v, formatted) :: details,
                Int64.add bytes size_bytes )
          | Error _ -> (("Signatory", v, "unknown size") :: details, bytes))
        ([], 0L)
        unused_signatory
    in
    let all_details = octez_details @ signatory_details in
    let total_bytes = Int64.add octez_bytes signatory_bytes in
    let details_lines =
      List.map
        (fun (kind, v, size) ->
          Printf.sprintf "  \xe2\x80\xa2 %s v%s (%s)" kind v size)
        (List.rev all_details)
    in
    let total_formatted = Binary_downloader.format_size_bytes total_bytes in
    let message =
      String.concat
        "\n"
        (["The following versions will be removed:"; ""]
        @ details_lines
        @ [""; Printf.sprintf "Total space to free: %s" total_formatted])
    in
    let total_count = List.length unused_octez + List.length unused_signatory in
    Modal_helpers.confirm_modal
      ~title:(Printf.sprintf "Prune %d unused version(s)?" total_count)
      ~message
      ~on_result:(fun confirmed ->
        if confirmed then (
          let octez_success, octez_fail =
            List.fold_left
              (fun (succ, fail) (v, _, _) ->
                match Binary_downloader.remove_version v with
                | Ok () -> (succ + 1, fail)
                | Error _ -> (succ, fail + 1))
              (0, 0)
              unused_octez
          in
          let signatory_success, signatory_fail =
            List.fold_left
              (fun (succ, fail) (v, _, _) ->
                match Signatory_downloader.remove_version v with
                | Ok () -> (succ + 1, fail)
                | Error _ -> (succ, fail + 1))
              (0, 0)
              unused_signatory
          in
          let success_count = octez_success + signatory_success in
          let fail_count = octez_fail + signatory_fail in
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
    ~toggle_major_expansion ~toggle_managed_group ~toggle_available_group s =
  if s.items = [] then s
  else
    let item = List.nth s.items s.selected in
    match item with
    | ManagedGroup (kind, _) ->
        (* Toggle group expansion on Enter *)
        toggle_managed_group s kind
    | ManagedVersion (kind, version, _, count) ->
        if count > 0 then
          (* If has instances, toggle expansion *)
          toggle_managed_expansion s version
        else (
          (* If unused, allow removal *)
          (match kind with
          | Octez -> remove_octez_version version
          | Signatory -> remove_signatory_version version) ;
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
    | AvailableGroup (kind, _) ->
        (* Toggle group expansion on Enter *)
        toggle_available_group s kind
    | AvailableVersion (_, vi) ->
        download_octez_version vi ;
        s
    | AvailableSignatoryVersion vi ->
        download_signatory_version vi ;
        s
    | AvailableMajorGroup (major, _) ->
        (* Toggle expansion on Enter *)
        toggle_major_expansion s major

(** Dispatch Tab key to toggle expansion of current item. *)
let toggle_current_group ~toggle_managed_expansion ~toggle_registered_expansion
    ~toggle_major_expansion ~toggle_managed_group ~toggle_available_group s =
  if s.items = [] then s
  else
    let item = List.nth s.items s.selected in
    match item with
    | ManagedGroup (kind, _) -> toggle_managed_group s kind
    | AvailableGroup (kind, _) -> toggle_available_group s kind
    | AvailableMajorGroup (major, _) -> toggle_major_expansion s major
    | ManagedVersion (_, version, _, count) ->
        if count > 0 then toggle_managed_expansion s version else s
    | RegisteredDir (ld, count) ->
        if count > 0 then toggle_registered_expansion s ld.Binary_registry.alias
        else s
    | _ -> s
