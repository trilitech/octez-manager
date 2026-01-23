(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Cmdliner
open Octez_manager_lib

(** Helper to count instances using a specific binary source *)
let count_instances_using bin_source =
  match Service_registry.list () with
  | Error _ -> 0
  | Ok services ->
      List.filter
        (fun svc ->
          match Service.get_bin_source svc with
          | bs when bs = bin_source -> true
          | _ -> false)
        services
      |> List.length

(** Helper to get instances using a specific binary source *)
let get_instances_using bin_source =
  match Service_registry.list () with
  | Error _ -> []
  | Ok services ->
      List.filter_map
        (fun svc ->
          match Service.get_bin_source svc with
          | bs when bs = bin_source -> Some svc.Service.instance
          | _ -> None)
        services

(** Helper to format file size *)
let format_size bytes =
  let kb = Int64.div bytes 1024L in
  let mb = Int64.div kb 1024L in
  let gb = Int64.div mb 1024L in
  if gb > 0L then Printf.sprintf "%Ld GB" gb
  else if mb > 0L then Printf.sprintf "%Ld MB" mb
  else if kb > 0L then Printf.sprintf "%Ld KB" kb
  else Printf.sprintf "%Ld bytes" bytes

(** Helper to get directory size *)
let get_dir_size path =
  try
    match Common.run_out ["du"; "-sb"; path] with
    | Ok output -> (
        match String.split_on_char '\t' output with
        | size_str :: _ -> (
            try Some (Int64.of_string (String.trim size_str)) with _ -> None)
        | _ -> None)
    | Error _ -> None
  with _ -> None

(** list-remote command *)
let list_remote_cmd =
  let term =
    let run include_rc =
      match Binary_downloader.fetch_versions ~include_rc () with
      | Error (`Msg msg) -> Cli_helpers.cmdliner_error msg
      | Ok versions ->
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
      (* List managed versions *)
      (match Binary_registry.list_managed_versions () with
      | Error (`Msg msg) ->
          Printf.eprintf "Warning: Failed to list managed versions: %s\n" msg
      | Ok versions ->
          if versions <> [] then (
            Printf.printf "Managed Versions:\n" ;
            List.iter
              (fun version ->
                let path = Binary_registry.managed_version_path version in
                let size_str =
                  match get_dir_size path with
                  | Some size -> format_size size
                  | None -> "unknown size"
                in
                let count =
                  count_instances_using
                    (Binary_registry.Managed_version version)
                in
                let usage =
                  if count = 0 then "unused"
                  else if count = 1 then "1 instance"
                  else Printf.sprintf "%d instances" count
                in
                Printf.printf "  v%s - %s (%s)\n" version size_str usage)
              versions)) ;

      (* List linked directories *)
      (match Binary_registry.load_linked_dirs () with
      | Error (`Msg msg) ->
          Printf.eprintf "Warning: Failed to load linked directories: %s\n" msg
      | Ok dirs ->
          if dirs <> [] then (
            Printf.printf "\nLinked Directories:\n" ;
            List.iter
              (fun (ld : Binary_registry.linked_dir) ->
                let count =
                  count_instances_using (Binary_registry.Linked_alias ld.alias)
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
      ~doc:"List installed managed versions and linked directories"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Shows all managed binary versions and linked directories with \
             their disk usage and instance counts.";
        ]
  in
  Cmd.v info term

(** download command *)
let download_cmd =
  let term =
    let run version verify_checksums =
      (* Cleanup stale temporary download directories *)
      Binary_downloader.cleanup_stale_temp_dirs () ;

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
      let doc = "Version to download (e.g., 24.0)" in
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
      let instances = get_instances_using bin_source in
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

(** link command *)
let link_cmd =
  let term =
    let run path alias =
      let alias =
        match alias with
        | Some a -> a
        | None ->
            (* Generate alias from path basename *)
            Filename.basename path
      in
      match Binary_registry.add_linked_dir ~alias ~path with
      | Error (`Msg msg) -> Cli_helpers.cmdliner_error msg
      | Ok () ->
          Printf.printf "✓ Linked directory: %s -> %s\n" alias path ;
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
      "link"
      ~doc:"Link a local directory containing Octez binaries"
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

(** unlink command *)
let unlink_cmd =
  let term =
    let run alias_or_path force =
      (* Try as alias first, then as path *)
      let alias =
        match Binary_registry.find_linked_dir alias_or_path with
        | Ok (Some ld) -> ld.alias
        | Ok None | Error _ -> (
            (* Maybe it's a path - find by path *)
            match Binary_registry.load_linked_dirs () with
            | Ok dirs -> (
                match
                  List.find_opt
                    (fun ld -> ld.Binary_registry.path = alias_or_path)
                    dirs
                with
                | Some ld -> ld.alias
                | None ->
                    Printf.eprintf
                      "Error: No linked directory found with alias or path: %s\n"
                      alias_or_path ;
                    exit 1)
            | Error (`Msg msg) ->
                Printf.eprintf "Error: %s\n" msg ;
                exit 1)
      in

      let bin_source = Binary_registry.Linked_alias alias in
      let instances = get_instances_using bin_source in
      if instances <> [] && not force then (
        Printf.printf
          "Linked directory '%s' is currently used by the following instances:\n"
          alias ;
        List.iter (fun inst -> Printf.printf "  - %s\n" inst) instances ;
        Printf.printf
          "\nUse --force to unlink anyway (may break these instances)\n" ;
        `Ok ())
      else
        match Binary_registry.remove_linked_dir alias with
        | Error (`Msg msg) -> Cli_helpers.cmdliner_error msg
        | Ok () ->
            Printf.printf "✓ Unlinked directory: %s\n" alias ;
            `Ok ()
    in
    let alias_arg =
      let doc = "Alias or path of linked directory to remove" in
      Arg.(
        required & pos 0 (some string) None & info [] ~docv:"ALIAS_OR_PATH" ~doc)
    in
    let force_flag =
      let doc = "Force unlinking even if in use" in
      Arg.(value & flag & info ["force"; "f"] ~doc)
    in
    Term.(ret (const run $ alias_arg $ force_flag))
  in
  let info =
    Cmd.info
      "unlink"
      ~doc:"Unlink a linked directory"
      ~man:
        [
          `S Manpage.s_description;
          `P "Removes a linked directory from the registry.";
          `P "This does not delete any files, only the link.";
          `P
            "If the directory is in use by any instances, you must use --force \
             to unlink it.";
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
                count_instances_using bin_source = 0)
              versions
          in
          if unused_versions = [] then (
            Printf.printf "No unused versions to prune.\n" ;
            `Ok ())
          else (
            Printf.printf
              "Found %d unused version(s):\n"
              (List.length unused_versions) ;
            let total_bytes = ref 0L in
            List.iter
              (fun v ->
                match Binary_downloader.get_version_size v with
                | Ok (bytes, formatted) ->
                    Printf.printf "  - v%s (%s)\n" v formatted ;
                    total_bytes := Int64.add !total_bytes bytes
                | Error _ -> Printf.printf "  - v%s (size unknown)\n" v)
              unused_versions ;
            let total_formatted =
              Binary_downloader.format_size_bytes !total_bytes
            in
            Printf.printf "\nTotal space to free: %s\n" total_formatted ;
            if dry_run then (
              Printf.printf "\n(Dry run - no changes made)\n" ;
              `Ok ())
            else (
              Printf.printf "\nRemoving...\n" ;
              let failures = ref [] in
              List.iter
                (fun version ->
                  match Binary_downloader.remove_version version with
                  | Ok () -> Printf.printf "  ✓ Removed v%s\n" version
                  | Error (`Msg msg) ->
                      Printf.eprintf
                        "  ✗ Failed to remove v%s: %s\n"
                        version
                        msg ;
                      failures := version :: !failures)
                unused_versions ;
              if !failures = [] then (
                Printf.printf
                  "\n✓ Successfully pruned %d version(s), freed %s\n"
                  (List.length unused_versions)
                  total_formatted ;
                `Ok ())
              else
                Cli_helpers.cmdliner_error
                  (Printf.sprintf
                     "%d version(s) failed to prune"
                     (List.length !failures))))
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
            "Commands for managing Octez binary versions and linked \
             directories.";
          `P
            "You can download official releases, link local directories (e.g., \
             dev builds), and manage versions used by instances.";
        ]
  in
  Cmd.group
    info
    [
      list_remote_cmd;
      list_cmd;
      download_cmd;
      remove_cmd;
      link_cmd;
      unlink_cmd;
      prune_cmd;
    ]
