(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Cmdliner
open Octez_manager_lib

(** list-remote command *)
let list_remote_cmd =
  let term =
    let run include_rc =
      match Binary_downloader.fetch_versions ~include_rc () with
      | Error (`Msg msg) -> Cli_helpers.cmdliner_error msg
      | Ok versions ->
          (* Filter out versions < 23.0 *)
          let versions =
            List.filter
              (fun (v : Binary_downloader.version_info) ->
                Binary_registry.compare_versions v.version "23.0" >= 0)
              versions
          in
          if versions = [] then (
            Printf.printf "No versions available.\n" ;
            `Ok ())
          else (
            Printf.printf "Available versions:\n" ;
            List.iter
              (fun (v : Binary_downloader.version_info) ->
                let rc_marker = if v.is_rc then " (RC)" else "" in
                let date_str =
                  match v.release_date with
                  | Some d -> Printf.sprintf " - %s" d
                  | None -> ""
                in
                Printf.printf "  %s%s%s\n" v.version rc_marker date_str)
              versions ;
            `Ok ())
    in
    let all_flag =
      let doc = "Include release candidates" in
      Arg.(value & flag & info ["all"; "a"] ~doc)
    in
    Term.(ret (const run $ all_flag))
  in
  let info =
    Cmd.info
      "list-remote"
      ~doc:"List available versions from remote repository"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Fetches and displays available Octez versions from the official \
             distribution.";
          `P "By default, only stable releases are shown.";
          `P "Use --all to include release candidates.";
        ]
  in
  Cmd.v info term

(** list command *)
let list_cmd =
  let term =
    let run () =
      (* List managed Octez versions *)
      (match Binary_registry.list_managed_versions () with
      | Error (`Msg msg) ->
          Printf.eprintf "Warning: Failed to list managed versions: %s\n" msg
      | Ok versions ->
          if versions <> [] then (
            Printf.printf "Octez Managed Versions:\n" ;
            List.iter
              (fun version ->
                let path = Binary_registry.managed_version_path version in
                let size_str =
                  match File_ops.get_dir_size path with
                  | Some size -> String_utils.format_size size
                  | None -> "unknown size"
                in
                let count =
                  Service_registry.count_instances_using
                    (Binary_registry.Managed_version version)
                in
                let usage =
                  if count = 0 then "unused"
                  else if count = 1 then "1 instance"
                  else Printf.sprintf "%d instances" count
                in
                Printf.printf "  v%s - %s (%s)\n" version size_str usage)
              versions)) ;

      (* List managed Signatory versions *)
      (match Signatory_downloader.list_managed_versions () with
      | Error (`Msg msg) ->
          Printf.eprintf "Warning: Failed to list Signatory versions: %s\n" msg
      | Ok versions ->
          if versions <> [] then (
            Printf.printf "\nSignatory Managed Versions:\n" ;
            List.iter
              (fun version ->
                let size_str =
                  match Signatory_downloader.get_version_size version with
                  | Ok (_bytes, formatted) -> formatted
                  | Error _ -> "unknown size"
                in
                (* TODO: Track which signatory instances use which binary *)
                Printf.printf "  v%s - %s\n" version size_str)
              versions)) ;

      (* List registered directories *)
      (match Binary_registry.load_registered_dirs () with
      | Error (`Msg msg) ->
          Printf.eprintf
            "Warning: Failed to load registered directories: %s\n"
            msg
      | Ok dirs ->
          if dirs <> [] then (
            Printf.printf "\nRegistered Directories:\n" ;
            List.iter
              (fun (ld : Binary_registry.registered_dir) ->
                let count =
                  Service_registry.count_instances_using
                    (Binary_registry.Registered_alias ld.alias)
                in
                let usage =
                  if count = 0 then "unused"
                  else if count = 1 then "1 instance"
                  else Printf.sprintf "%d instances" count
                in
                Printf.printf "  %s -> %s (%s)\n" ld.alias ld.path usage)
              dirs)) ;

      `Ok ()
    in
    Term.(ret (const run $ const ()))
  in
  let info =
    Cmd.info
      "list"
      ~doc:"List installed managed versions and registered directories"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Shows all managed binary versions and registered directories with \
             their disk usage and instance counts.";
        ]
  in
  Cmd.v info term

(** download command *)
let download_cmd =
  let term =
    let run version_input verify_checksums =
      (* Cleanup stale temporary download directories *)
      Binary_downloader.cleanup_stale_temp_dirs () ;

      (* Resolve "latest" to actual version *)
      let version =
        if String.trim version_input = "latest" then
          match Binary_downloader.fetch_versions ~include_rc:false () with
          | Error (`Msg e) ->
              Printf.eprintf "Error: Failed to fetch latest version: %s\n" e ;
              exit 1
          | Ok [] ->
              Printf.eprintf "Error: No versions available\n" ;
              exit 1
          | Ok versions -> (
              (* Filter out versions < 23.0 *)
              let versions =
                List.filter
                  (fun (v : Binary_downloader.version_info) ->
                    Binary_registry.compare_versions v.version "23.0" >= 0)
                  versions
              in
              (* Sort versions to get the latest *)
              let sorted =
                List.sort
                  (fun (a : Binary_downloader.version_info)
                       (b : Binary_downloader.version_info)
                     -> -Version_checker.compare_versions a.version b.version)
                  versions
              in
              match sorted with
              | latest :: _ ->
                  Printf.printf "Latest version is v%s\n" latest.version ;
                  latest.version
              | [] ->
                  Printf.eprintf "Error: No versions available\n" ;
                  exit 1)
        else String.trim version_input
      in

      Printf.printf "Downloading Octez v%s...\n\n" version ;

      (* Initialize multi-line progress display *)
      let display_state =
        ref
          (Cli_progress.init_display
             ["octez-node"; "octez-client"; "octez-baker"; "octez-dal-node"])
      in

      (* Mutex to protect display_state from concurrent access *)
      let display_mutex = Mutex.create () in

      (* Render initial state *)
      let lines = Cli_progress.render_display !display_state in
      display_state := {!display_state with lines_printed = lines} ;

      (* Multi-progress callback (thread-safe for parallel downloads) *)
      let multi_progress (mp : Binary_downloader.multi_progress_state) =
        Mutex.lock display_mutex ;
        (* Update state for current file *)
        display_state :=
          Cli_progress.set_in_progress
            !display_state
            ~binary:mp.current_file
            ~downloaded:mp.downloaded
            ~total:mp.total ;
        (* Re-render *)
        let lines = Cli_progress.render_display !display_state in
        display_state := {!display_state with lines_printed = lines} ;
        Mutex.unlock display_mutex
      in

      (* Perform download *)
      match
        Binary_downloader.download_version
          ~version
          ~verify_checksums
          ~multi_progress
          ()
      with
      | Error (`Msg msg) -> Cli_helpers.cmdliner_error msg
      | Ok result ->
          (* Mark all binaries complete with their final sizes *)
          Mutex.lock display_mutex ;
          List.iter
            (fun binary ->
              (* Get file size from disk *)
              let path =
                Filename.concat
                  (Binary_registry.managed_version_path version)
                  binary
              in
              let size =
                try
                  let stats = Unix.stat path in
                  Int64.of_int stats.Unix.st_size
                with _ -> 0L
              in
              display_state :=
                Cli_progress.set_complete !display_state ~binary ~size)
            result.binaries ;

          (* Show checksum verification *)
          display_state :=
            Cli_progress.set_checksum_status
              !display_state
              "[\xe2\x86\x92] Verifying checksums..." ;
          let lines = Cli_progress.render_display !display_state in
          display_state := {!display_state with lines_printed = lines} ;
          Mutex.unlock display_mutex ;

          (* Update checksum status based on result *)
          Mutex.lock display_mutex ;
          let checksum_msg =
            match result.checksum_status with
            | Binary_downloader.Verified ->
                "[\xe2\x9c\x93] All checksums verified"
            | Binary_downloader.Skipped ->
                "[\xe2\x9a\xa0] Checksum verification skipped"
            | Binary_downloader.Failed reason ->
                Printf.sprintf
                  "[\xe2\x9c\x97] Checksum verification failed: %s"
                  reason
          in
          display_state :=
            Cli_progress.set_checksum_status !display_state checksum_msg ;
          let lines = Cli_progress.render_display !display_state in
          display_state := {!display_state with lines_printed = lines} ;
          Mutex.unlock display_mutex ;

          (* Final newline *)
          Printf.printf "\n" ;
          `Ok ()
    in
    let version_arg =
      let doc = "Version to download (e.g., 24.0 or 'latest')" in
      Arg.(required & pos 0 (some string) None & info [] ~docv:"VERSION" ~doc)
    in
    let no_verify_flag =
      let doc = "Skip checksum verification" in
      Arg.(value & flag & info ["no-verify"] ~doc)
    in
    Term.(
      ret (const (fun v nv -> run v (not nv)) $ version_arg $ no_verify_flag))
  in
  let info =
    Cmd.info
      "download"
      ~doc:"Download an Octez version"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Downloads the specified Octez version from the official \
             distribution.";
          `P "Use 'latest' to automatically download the newest stable version.";
          `P
            "Checksums are verified by default. Use --no-verify to skip \
             verification.";
        ]
  in
  Cmd.v info term

(** remove command *)
let remove_cmd =
  let term =
    let run version force =
      let bin_source = Binary_registry.Managed_version version in
      let instances = Service_registry.get_instances_using bin_source in
      if instances <> [] && not force then (
        Printf.printf
          "Version v%s is currently used by the following instances:\n"
          version ;
        List.iter (fun inst -> Printf.printf "  - %s\n" inst) instances ;
        Printf.printf
          "\nUse --force to remove anyway (may break these instances)\n" ;
        `Ok ())
      else
        match Binary_downloader.remove_version version with
        | Error (`Msg msg) -> Cli_helpers.cmdliner_error msg
        | Ok () ->
            Printf.printf "✓ Removed version v%s\n" version ;
            `Ok ()
    in
    let version_arg =
      let doc = "Version to remove (e.g., 24.0)" in
      Arg.(required & pos 0 (some string) None & info [] ~docv:"VERSION" ~doc)
    in
    let force_flag =
      let doc = "Force removal even if in use" in
      Arg.(value & flag & info ["force"; "f"] ~doc)
    in
    Term.(ret (const run $ version_arg $ force_flag))
  in
  let info =
    Cmd.info
      "remove"
      ~doc:"Remove a managed version"
      ~man:
        [
          `S Manpage.s_description;
          `P "Removes a managed binary version from disk.";
          `P
            "If the version is in use by any instances, you must use --force \
             to remove it.";
        ]
  in
  Cmd.v info term

(** register command *)
let register_cmd =
  let term =
    let run path alias =
      let alias =
        match alias with
        | Some a -> a
        | None ->
            (* Generate alias from path basename *)
            Filename.basename path
      in
      match Binary_registry.add_registered_dir ~alias ~path with
      | Error (`Msg msg) -> Cli_helpers.cmdliner_error msg
      | Ok () ->
          Printf.printf "✓ Registered directory: %s -> %s\n" alias path ;
          `Ok ()
    in
    let path_arg =
      let doc = "Path to directory containing Octez binaries" in
      Arg.(required & pos 0 (some string) None & info [] ~docv:"PATH" ~doc)
    in
    let alias_opt =
      let doc = "Alias name for this directory (default: basename of path)" in
      Arg.(
        value & opt (some string) None & info ["alias"; "a"] ~docv:"NAME" ~doc)
    in
    Term.(ret (const run $ path_arg $ alias_opt))
  in
  let info =
    Cmd.info
      "register"
      ~doc:"Register a local directory containing Octez binaries"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Registers a local directory containing Octez binaries so it can \
             be used by instances.";
          `P "This is useful for development builds or custom installations.";
        ]
  in
  Cmd.v info term

(** unregister command *)
let unregister_cmd =
  let term =
    let run alias_or_path force =
      (* Try as alias first, then as path *)
      let alias =
        match Binary_registry.find_registered_dir alias_or_path with
        | Ok (Some ld) -> ld.alias
        | Ok None | Error _ -> (
            (* Maybe it's a path - find by path *)
            match Binary_registry.load_registered_dirs () with
            | Ok dirs -> (
                match
                  List.find_opt
                    (fun ld -> ld.Binary_registry.path = alias_or_path)
                    dirs
                with
                | Some ld -> ld.alias
                | None ->
                    Printf.eprintf
                      "Error: No registered directory found with alias or \
                       path: %s\n"
                      alias_or_path ;
                    exit 1)
            | Error (`Msg msg) ->
                Printf.eprintf "Error: %s\n" msg ;
                exit 1)
      in

      let bin_source = Binary_registry.Registered_alias alias in
      let instances = Service_registry.get_instances_using bin_source in
      if instances <> [] && not force then (
        Printf.printf
          "Registered directory '%s' is currently used by the following \
           instances:\n"
          alias ;
        List.iter (fun inst -> Printf.printf "  - %s\n" inst) instances ;
        Printf.printf
          "\nUse --force to unregister anyway (may break these instances)\n" ;
        `Ok ())
      else
        match Binary_registry.remove_registered_dir alias with
        | Error (`Msg msg) -> Cli_helpers.cmdliner_error msg
        | Ok () ->
            Printf.printf "✓ Unregistered directory: %s\n" alias ;
            `Ok ()
    in
    let alias_arg =
      let doc = "Alias or path of registered directory to remove" in
      Arg.(
        required & pos 0 (some string) None & info [] ~docv:"ALIAS_OR_PATH" ~doc)
    in
    let force_flag =
      let doc = "Force unregistering even if in use" in
      Arg.(value & flag & info ["force"; "f"] ~doc)
    in
    Term.(ret (const run $ alias_arg $ force_flag))
  in
  let info =
    Cmd.info
      "unregister"
      ~doc:"Unregister a registered directory"
      ~man:
        [
          `S Manpage.s_description;
          `P "Removes a registered directory from the registry.";
          `P "This does not delete any files, only the registration.";
          `P
            "If the directory is in use by any instances, you must use --force \
             to unregister it.";
        ]
  in
  Cmd.v info term

(** prune command *)
let prune_cmd =
  let term =
    let run dry_run =
      match Binary_registry.list_managed_versions () with
      | Error (`Msg msg) -> Cli_helpers.cmdliner_error msg
      | Ok versions ->
          let unused_versions =
            List.filter
              (fun version ->
                let bin_source = Binary_registry.Managed_version version in
                Service_registry.count_instances_using bin_source = 0)
              versions
          in
          if unused_versions = [] then (
            Printf.printf "No unused versions to prune.\n" ;
            `Ok ())
          else (
            Printf.printf
              "Found %d unused version(s):\n"
              (List.length unused_versions) ;
            let total_bytes =
              List.fold_left
                (fun acc v ->
                  match Binary_downloader.get_version_size v with
                  | Ok (bytes, formatted) ->
                      Printf.printf "  - v%s (%s)\n" v formatted ;
                      Int64.add acc bytes
                  | Error _ ->
                      Printf.printf "  - v%s (size unknown)\n" v ;
                      acc)
                0L
                unused_versions
            in
            let total_formatted =
              Binary_downloader.format_size_bytes total_bytes
            in
            Printf.printf "\nTotal space to free: %s\n" total_formatted ;
            if dry_run then (
              Printf.printf "\n(Dry run - no changes made)\n" ;
              `Ok ())
            else (
              Printf.printf "\nRemoving...\n" ;
              let failures =
                List.filter_map
                  (fun version ->
                    match Binary_downloader.remove_version version with
                    | Ok () ->
                        Printf.printf "  ✓ Removed v%s\n" version ;
                        None
                    | Error (`Msg msg) ->
                        Printf.eprintf
                          "  ✗ Failed to remove v%s: %s\n"
                          version
                          msg ;
                        Some version)
                  unused_versions
              in
              if failures = [] then (
                Printf.printf
                  "\n✓ Successfully pruned %d version(s), freed %s\n"
                  (List.length unused_versions)
                  total_formatted ;
                `Ok ())
              else
                Cli_helpers.cmdliner_error
                  (Printf.sprintf
                     "%d version(s) failed to prune"
                     (List.length failures))))
    in
    let dry_run_flag =
      let doc = "Show what would be pruned without removing" in
      Arg.(value & flag & info ["dry-run"; "n"] ~doc)
    in
    Term.(ret (const run $ dry_run_flag))
  in
  let info =
    Cmd.info
      "prune"
      ~doc:"Remove all unused managed versions"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Removes all managed binary versions that are not currently used \
             by any instances.";
          `P
            "Use --dry-run to see what would be removed without making changes.";
        ]
  in
  Cmd.v info term

(** Signatory list-remote command *)
let signatory_list_remote_cmd =
  let term =
    let run include_prerelease =
      match Signatory_downloader.fetch_versions ~include_prerelease () with
      | Error (`Msg msg) -> Cli_helpers.cmdliner_error msg
      | Ok versions ->
          if versions = [] then (
            Printf.printf "No Signatory versions available.\n" ;
            `Ok ())
          else (
            Printf.printf "Available Signatory versions:\n" ;
            List.iter
              (fun (v : Signatory_downloader.version_info) ->
                let pre_marker =
                  if v.is_prerelease then " (prerelease)" else ""
                in
                let date_str =
                  match v.release_date with
                  | Some d -> Printf.sprintf " - %s" d
                  | None -> ""
                in
                Printf.printf "  %s%s%s\n" v.version pre_marker date_str)
              versions ;
            `Ok ())
    in
    let all_flag =
      let doc = "Include prerelease versions (RC, beta, alpha)" in
      Arg.(value & flag & info ["all"; "a"] ~doc)
    in
    Term.(ret (const run $ all_flag))
  in
  let info =
    Cmd.info
      "signatory-list-remote"
      ~doc:"List available Signatory versions from GitHub"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Fetches and displays available Signatory versions from GitHub \
             releases.";
          `P "By default, only stable releases are shown.";
          `P "Use --all to include prerelease versions (RC, beta, alpha).";
        ]
  in
  Cmd.v info term

(** Signatory list command *)
let signatory_list_cmd =
  let term =
    let run () =
      match Signatory_downloader.list_managed_versions () with
      | Error (`Msg msg) ->
          Printf.eprintf "Error: Failed to list Signatory versions: %s\n" msg ;
          `Error (false, msg)
      | Ok versions ->
          if versions = [] then (
            Printf.printf "No managed Signatory versions installed.\n" ;
            `Ok ())
          else (
            Printf.printf "Managed Signatory Versions:\n" ;
            List.iter
              (fun version ->
                let size_str =
                  match Signatory_downloader.get_version_size version with
                  | Ok (_bytes, formatted) -> formatted
                  | Error _ -> "unknown size"
                in
                (* TODO: Track which signatory instances use which binary *)
                Printf.printf "  v%s - %s\n" version size_str)
              versions ;
            `Ok ())
    in
    Term.(ret (const run $ const ()))
  in
  let info =
    Cmd.info
      "signatory-list"
      ~doc:"List installed Signatory versions"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Shows all managed Signatory binary versions with their disk usage.";
        ]
  in
  Cmd.v info term

(** Signatory download command *)
let signatory_download_cmd =
  let term =
    let run version_input verify_checksums =
      (* Cleanup stale temporary download directories *)
      Signatory_downloader.cleanup_stale_temp_dirs () ;

      (* Resolve "latest" to actual version *)
      let version =
        if String.trim version_input = "latest" then (
          match
            Signatory_downloader.fetch_versions ~include_prerelease:false ()
          with
          | Error (`Msg e) ->
              Printf.eprintf "Error: Failed to fetch latest version: %s\n" e ;
              exit 1
          | Ok [] ->
              Printf.eprintf "Error: No versions available\n" ;
              exit 1
          | Ok (latest :: _) ->
              Printf.printf "Latest Signatory version is v%s\n" latest.version ;
              latest.version)
        else String.trim version_input
      in

      Printf.printf "Downloading Signatory v%s...\n\n" version ;

      (* Simple progress callback *)
      let last_percent = ref (-1) in
      let progress ~downloaded ~total =
        match total with
        | Some t ->
            let percent = Int64.(to_int (div (mul 100L downloaded) t)) in
            if percent <> !last_percent && percent mod 10 = 0 then (
              last_percent := percent ;
              Printf.printf "\rProgress: %d%%" percent ;
              flush stdout)
        | None -> ()
      in

      (* Perform download *)
      match
        Signatory_downloader.download_version
          ~version
          ~verify_checksums
          ~progress
          ()
      with
      | Error (`Msg msg) ->
          Printf.printf "\n" ;
          Cli_helpers.cmdliner_error msg
      | Ok result ->
          Printf.printf "\n" ;
          let checksum_msg =
            match result.checksum_status with
            | Signatory_downloader.Verified -> "✓ Checksum verified"
            | Signatory_downloader.Skipped -> "⚠ Checksum verification skipped"
            | Signatory_downloader.Failed reason ->
                Printf.sprintf "✗ Checksum verification failed: %s" reason
          in
          Printf.printf "%s\n" checksum_msg ;
          Printf.printf
            "✓ Signatory v%s installed to: %s\n"
            version
            result.installed_path ;
          `Ok ()
    in
    let version_arg =
      let doc = "Version to download (e.g., 1.3.1 or 'latest')" in
      Arg.(required & pos 0 (some string) None & info [] ~docv:"VERSION" ~doc)
    in
    let no_verify_flag =
      let doc = "Skip checksum verification" in
      Arg.(value & flag & info ["no-verify"] ~doc)
    in
    Term.(
      ret (const (fun v nv -> run v (not nv)) $ version_arg $ no_verify_flag))
  in
  let info =
    Cmd.info
      "signatory-download"
      ~doc:"Download a Signatory version"
      ~man:
        [
          `S Manpage.s_description;
          `P "Downloads the specified Signatory version from GitHub releases.";
          `P "Use 'latest' to automatically download the newest stable version.";
          `P
            "Checksums are verified by default. Use --no-verify to skip \
             verification.";
          `P
            "Downloaded binaries are stored in \
             ~/.local/share/octez-manager/signatory-binaries/";
        ]
  in
  Cmd.v info term

(** Signatory remove command *)
let signatory_remove_cmd =
  let term =
    let run version _force =
      (* TODO: Check if version is in use by signatory instances *)
      match Signatory_downloader.remove_version version with
      | Error (`Msg msg) -> Cli_helpers.cmdliner_error msg
      | Ok () ->
          Printf.printf "✓ Removed Signatory v%s\n" version ;
          `Ok ()
    in
    let version_arg =
      let doc = "Version to remove (e.g., 1.3.1)" in
      Arg.(required & pos 0 (some string) None & info [] ~docv:"VERSION" ~doc)
    in
    let force_flag =
      let doc = "Force removal even if in use" in
      Arg.(value & flag & info ["force"; "f"] ~doc)
    in
    Term.(ret (const run $ version_arg $ force_flag))
  in
  let info =
    Cmd.info
      "signatory-remove"
      ~doc:"Remove a Signatory version"
      ~man:
        [
          `S Manpage.s_description;
          `P "Removes a managed Signatory binary version from disk.";
        ]
  in
  Cmd.v info term

(** Main binaries command group *)
let binaries_cmd =
  let info =
    Cmd.info
      "binaries"
      ~doc:"Manage Octez binaries"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Commands for managing Octez binary versions and registered \
             directories.";
          `P
            "You can download official releases, register local directories \
             (e.g., dev builds), and manage versions used by instances.";
        ]
  in
  Cmd.group
    info
    [
      list_remote_cmd;
      list_cmd;
      download_cmd;
      remove_cmd;
      register_cmd;
      unregister_cmd;
      prune_cmd;
      signatory_list_remote_cmd;
      signatory_list_cmd;
      signatory_download_cmd;
      signatory_remove_cmd;
    ]
