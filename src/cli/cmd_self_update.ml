(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** CLI command for self-updating octez-manager *)

open Cmdliner
open Octez_manager_lib

let self_update_cmd =
  let term =
    let run check_only force =
      if check_only then (
        (* Just check for updates *)
        Printf.printf "Checking for updates...\n%!" ;
        match Self_update_checker.check_for_updates ~force () with
        | Self_update_checker.Update_available info ->
            let update_type =
              if info.is_major_update then "MAJOR" else "minor"
            in
            Printf.printf "Update available (%s):\n" update_type ;
            Printf.printf "  Current version: v%s\n" info.current_version ;
            Printf.printf "  Latest version:  v%s\n" info.latest_version ;
            Printf.printf "  Release notes:   %s\n" info.release_notes_url ;
            Printf.printf "\nRun 'octez-manager self-update' to upgrade.\n" ;
            `Ok ()
        | Self_update_checker.Up_to_date ->
            Printf.printf
              "octez-manager is up to date (v%s)\n"
              Self_update_checker.current_version ;
            `Ok ()
        | Self_update_checker.Check_disabled ->
            Printf.printf "Update checking is disabled.\n" ;
            Printf.printf
              "Enable with: octez-manager self-update --enable-checks\n" ;
            `Ok ()
        | Self_update_checker.Check_failed msg ->
            Cli_helpers.cmdliner_error (Printf.sprintf "Check failed: %s" msg))
      else
        (* Perform update *)
        let install_method = Self_update_checker.detect_install_method () in
        match install_method with
        | Self_update_checker.Deb_package ->
            Printf.printf "octez-manager was installed via package manager.\n\n" ;
            Printf.printf "To update, run:\n" ;
            Printf.printf
              "  sudo apt update && sudo apt upgrade octez-manager\n" ;
            `Ok ()
        | Self_update_checker.Manual_install ->
            Printf.printf "octez-manager appears to be manually installed.\n\n" ;
            Printf.printf "Please update manually from the releases page:\n" ;
            Printf.printf
              "  https://github.com/trilitech/octez-manager/releases\n" ;
            `Ok ()
        | Self_update_checker.Binary_install -> (
            Printf.printf "Checking for updates...\n%!" ;
            match Self_update_checker.check_for_updates ~force () with
            | Self_update_checker.Up_to_date ->
                Printf.printf
                  "octez-manager is already up to date (v%s)\n"
                  Self_update_checker.current_version ;
                `Ok ()
            | Self_update_checker.Check_disabled ->
                Printf.printf "Update checking is disabled.\n" ;
                `Ok ()
            | Self_update_checker.Check_failed msg ->
                Cli_helpers.cmdliner_error
                  (Printf.sprintf "Check failed: %s" msg)
            | Self_update_checker.Update_available info -> (
                Printf.printf
                  "Updating from v%s to v%s...\n%!"
                  info.current_version
                  info.latest_version ;
                Printf.printf "Downloading...\n%!" ;
                let on_progress ~downloaded ~total =
                  match total with
                  | Some t ->
                      let pct =
                        Int64.(to_float downloaded /. to_float t *. 100.0)
                      in
                      (* \r moves cursor to start, \x1b[K clears from cursor to end of line *)
                      Printf.printf
                        "\r\x1b[KDownloading... %.0f%% (%Ld / %Ld bytes)%!"
                        pct
                        downloaded
                        t
                  | None ->
                      Printf.printf
                        "\r\x1b[KDownloading... %Ld bytes%!"
                        downloaded
                in
                match
                  Self_update_checker.perform_upgrade
                    ~version:info.latest_version
                    ~on_progress
                    ()
                with
                | Self_update_checker.Upgrade_success
                    {new_version; needs_restart} ->
                    Printf.printf "\n\n" ;
                    if needs_restart then (
                      Printf.printf
                        "✓ Successfully updated to v%s\n"
                        new_version ;
                      Printf.printf
                        "  Please restart octez-manager to use the new version.\n" ;
                      `Ok ())
                    else (
                      Printf.printf
                        "✓ Successfully updated to v%s\n"
                        new_version ;
                      Printf.printf "  Restarting...\n%!" ;
                      Self_update_checker.exec_restart () ;
                      (* If we get here, exec failed *)
                      Printf.printf
                        "  Note: Auto-restart failed. Please restart manually.\n" ;
                      `Ok ())
                | Self_update_checker.Upgrade_needs_elevation cmd ->
                    Printf.printf "\n\n" ;
                    Printf.printf
                      "Permission denied. Please run with elevated privileges:\n" ;
                    Printf.printf "  sudo %s\n" cmd ;
                    `Ok ()
                | Self_update_checker.Upgrade_failed msg ->
                    Printf.printf "\n" ;
                    Cli_helpers.cmdliner_error
                      (Printf.sprintf "Update failed: %s" msg)))
    in
    let check_flag =
      let doc = "Only check for updates, don't install" in
      Arg.(value & flag & info ["check"; "c"] ~doc)
    in
    let force_flag =
      let doc = "Force check (bypass cache)" in
      Arg.(value & flag & info ["force"; "f"] ~doc)
    in
    Term.(ret (const run $ check_flag $ force_flag))
  in
  let info =
    Cmd.info
      "self-update"
      ~doc:"Check for and install octez-manager updates"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Check for new versions of octez-manager and optionally install \
             them.";
          `P "The update behavior depends on how octez-manager was installed:";
          `I
            ( "Package install (apt/dpkg)",
              "Shows the appropriate apt command to run." );
          `I
            ( "Binary install (install script)",
              "Downloads and installs the new version automatically." );
          `I ("Manual install", "Shows link to release page.");
          `S Manpage.s_examples;
          `P "Check for updates:";
          `Pre "  octez-manager self-update --check";
          `P "Install updates:";
          `Pre "  octez-manager self-update";
          `P "Force update check (bypass cache):";
          `Pre "  octez-manager self-update --check --force";
        ]
  in
  Cmd.v info term

(** version command - show current version and update status *)
let version_cmd =
  let term =
    let run () =
      Printf.printf "octez-manager v%s\n" Self_update_checker.current_version ;
      (* Quick check for updates *)
      match Self_update_checker.check_for_updates () with
      | Self_update_checker.Update_available info ->
          let marker = if info.is_major_update then " (MAJOR)" else "" in
          Printf.printf "\nUpdate available: v%s%s\n" info.latest_version marker ;
          Printf.printf "Run 'octez-manager self-update' to upgrade.\n" ;
          `Ok ()
      | _ -> `Ok ()
    in
    Term.(ret (const run $ const ()))
  in
  let info =
    Cmd.info "version" ~doc:"Show version information and check for updates"
  in
  Cmd.v info term
