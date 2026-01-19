(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Cmdliner
open Octez_manager_lib
module S = Service

let list_cmd =
  let term =
    let run () =
      Capabilities.register () ;
      match
        Miaou_interfaces.Capability.get
          Manager_interfaces.Service_manager_capability.key
      with
      | Some cap -> (
          let module SM = (val cap : Manager_interfaces.Service_manager) in
          match SM.list () with
          | Ok services ->
              Cli_output.print_services services ;
              `Ok ()
          | Error (`Msg msg) -> Cli_helpers.cmdliner_error msg)
      | None ->
          Cli_helpers.cmdliner_error "Service manager capability not available"
    in
    Term.(ret (const run $ const ()))
  in
  let info = Cmd.info "list" ~doc:"Show registered services" in
  Cmd.v info term

let purge_all_cmd =
  let term =
    let run () =
      Capabilities.register () ;
      match Service_registry.list () with
      | Error (`Msg msg) -> Cli_helpers.cmdliner_error msg
      | Ok services ->
          if services = [] then (
            print_endline "No services registered to purge." ;
            (* Still clear directory registry in case of stale entries *)
            (match Directory_registry.clear_all () with
            | Ok () -> Format.printf "Directory registry cleared.@."
            | Error (`Msg msg) ->
                Format.eprintf
                  "Warning: Failed to clear directory registry: %s@."
                  msg) ;
            `Ok ())
          else
            let failures = ref [] in
            List.iter
              (fun svc ->
                let instance = svc.S.instance in
                let role = svc.S.role in
                Format.printf "Purging instance '%s' (%s)...@." instance role ;
                match
                  Installer.purge_service
                    ~quiet:false
                    ~prompt_yes_no:
                      (if Cli_helpers.is_interactive () then
                         Cli_helpers.prompt_yes_no
                       else fun _ ~default:_ -> false)
                    ~instance
                    ()
                with
                | Ok () ->
                    Format.printf "  ✓ Successfully purged '%s'@." instance
                | Error (`Msg msg) ->
                    Format.eprintf "  ✗ Failed to purge '%s': %s@." instance msg ;
                    failures := (instance, msg) :: !failures)
              services ;
            if !failures = [] then (
              (* Clear directory registry after successful purge *)
              (match Directory_registry.clear_all () with
              | Ok () -> Format.printf "@.Directory registry cleared.@."
              | Error (`Msg msg) ->
                  Format.eprintf
                    "@.Warning: Failed to clear directory registry: %s@."
                    msg) ;
              Format.printf "All instances purged successfully.@." ;
              `Ok ())
            else
              let error_summary =
                Printf.sprintf
                  "@.%d instance(s) failed to purge"
                  (List.length !failures)
              in
              Cli_helpers.cmdliner_error error_summary
    in
    Term.(ret (const run $ const ()))
  in
  let info =
    Cmd.info
      "purge-all"
      ~doc:
        "Purge all registered instances. This removes each service, deletes \
         data directories, log files, and (when run as root) drops service \
         users that are no longer referenced by other services."
  in
  Cmd.v info term

let cleanup_orphans_cmd =
  let dry_run =
    Arg.(
      value & flag
      & info
          ["dry-run"; "n"]
          ~doc:"Show what would be removed without actually deleting.")
  in
  let term =
    let run dry_run =
      Capabilities.register () ;
      match Installer.find_orphan_directories () with
      | Error (`Msg msg) -> Cli_helpers.cmdliner_error msg
      | Ok (orphan_dirs, orphan_logs) -> (
          if orphan_dirs = [] && orphan_logs = [] then (
            print_endline "No orphan directories or files found." ;
            `Ok ())
          else if dry_run then (
            print_endline "Would remove the following orphan paths:" ;
            List.iter (fun d -> Format.printf "  [dir]  %s@." d) orphan_dirs ;
            List.iter (fun f -> Format.printf "  [file] %s@." f) orphan_logs ;
            `Ok ())
          else
            match Installer.cleanup_orphans ~dry_run:false with
            | Error (`Msg msg) -> Cli_helpers.cmdliner_error msg
            | Ok (removed, errors) ->
                List.iter (fun p -> Format.printf "  ✓ Removed: %s@." p) removed ;
                List.iter
                  (fun (p, msg) ->
                    Format.eprintf "  ✗ Failed to remove %s: %s@." p msg)
                  errors ;
                if errors = [] then (
                  Format.printf
                    "@.Cleanup complete. %d item(s) removed.@."
                    (List.length removed) ;
                  `Ok ())
                else
                  Cli_helpers.cmdliner_error
                    (Printf.sprintf
                       "%d item(s) failed to remove"
                       (List.length errors)))
    in
    Term.(ret (const run $ dry_run))
  in
  let info =
    Cmd.info
      "cleanup-orphans"
      ~doc:
        "Remove orphan data directories and log files not associated with any \
         registered service. Use --dry-run to preview what would be removed."
  in
  Cmd.v info term

let cleanup_dependencies_cmd =
  let term =
    let run () =
      Capabilities.register () ;
      match Installer.cleanup_dependencies () with
      | Error (`Msg msg) -> Cli_helpers.cmdliner_error msg
      | Ok 0 ->
          print_endline "No stale dependency entries found." ;
          `Ok ()
      | Ok count ->
          Format.printf "Cleaned up %d stale dependency entrie(s).@." count ;
          `Ok ()
    in
    Term.(ret (const run $ const ()))
  in
  let info =
    Cmd.info
      "cleanup-dependencies"
      ~doc:
        "Remove stale dependency entries from service configurations. This \
         cleans up references to services that have been removed."
  in
  Cmd.v info term

let list_networks_cmd =
  let output_json =
    Arg.(value & flag & info ["json"] ~doc:"Emit JSON output instead of text.")
  in
  let term =
    let run output_json =
      match Teztnets.list_networks () with
      | Ok infos ->
          (* warn_if_fallback pairs ; -- TODO: reimplement warning if needed *)
          if output_json then
            let json =
              `List
                (List.map
                   (fun (n : Teztnets.network_info) ->
                     `Assoc
                       [
                         ("alias", `String n.alias);
                         ("network_url", `String n.network_url);
                         ("human_name", `String n.human_name);
                         ( "description",
                           match n.description with
                           | Some s -> `String s
                           | None -> `Null );
                         ( "rpc_url",
                           match n.rpc_url with
                           | Some s -> `String s
                           | None -> `Null );
                       ])
                   infos)
            in
            Yojson.Safe.pretty_to_string json |> print_endline
          else
            List.iter
              (fun (n : Teztnets.network_info) ->
                Format.printf
                  "%-16s %-24s %s@."
                  n.alias
                  n.human_name
                  n.network_url)
              infos ;
          `Ok ()
      | Error (`Msg msg) -> Cli_helpers.cmdliner_error msg
    in
    Term.(ret (const run $ output_json))
  in
  let info =
    Cmd.info
      "list-available-networks"
      ~doc:"Show networks advertised on teztnets.com (with fallbacks)."
  in
  Cmd.v info term

let list_snapshots_cmd =
  let network =
    let doc = "Network alias or teztnets.json URL to inspect." in
    Arg.(value & opt string "mainnet" & info ["network"] ~doc ~docv:"NET")
  in
  let output_json =
    Arg.(value & flag & info ["json"] ~doc:"Emit JSON output instead of text.")
  in
  let term =
    let run network output_json =
      match Snapshots.slug_of_network network with
      | None -> Cli_helpers.cmdliner_error "--network cannot be empty"
      | Some slug -> (
          match Snapshots.list ~network_slug:slug with
          | Ok entries ->
              if output_json then
                let json =
                  `List (List.map Cli_output.snapshot_entry_to_json entries)
                in
                Yojson.Safe.pretty_to_string json |> print_endline
              else List.iter Cli_output.print_snapshot_entry entries ;
              `Ok ()
          | Error (`Msg msg) -> Cli_helpers.cmdliner_error msg)
    in
    Term.(ret (const run $ network $ output_json))
  in
  let info =
    Cmd.info
      "list-snapshots"
      ~doc:"List downloads published on snapshots.tzinit.org for a network."
  in
  Cmd.v info term

let ui_term =
  let open Cmdliner in
  let page_arg =
    Arg.(
      value
      & opt (some string) None
      & info ["page"] ~doc:"Start on a registered page" ~docv:"NAME")
  in
  let log_flag =
    Arg.(value & flag & info ["ui-log"] ~doc:"Enable UI debug logs")
  in
  let logfile_arg =
    Arg.(
      value
      & opt (some string) None
      & info ["ui-logfile"] ~doc:"Write UI logs to FILE" ~docv:"FILE")
  in
  Term.(
    ret
      (const (fun page log logfile ->
           Capabilities.register () ;
           (* Ignore SIGPIPE to prevent crashes when subprocesses write to closed pipes *)
           Sys.set_signal Sys.sigpipe Sys.Signal_ignore ;
           let result =
             (* Use POSIX backend to avoid io_uring resource exhaustion *)
             Eio_posix.run @@ fun env ->
             Eio.Switch.run @@ fun sw ->
             Miaou_helpers.Fiber_runtime.init ~env ~sw ;
             Octez_manager_ui.Manager_app.run ?page ~log ?logfile ()
           in
           match result with
           | Ok () -> `Ok ()
           | Error (`Msg msg) -> Cli_helpers.cmdliner_error msg)
      $ page_arg $ log_flag $ logfile_arg))

let ui_cmd =
  let open Cmdliner in
  let info =
    Cmd.info
      "ui"
      ~doc:
        "Launch the interactive terminal UI (same as running without arguments)"
  in
  Cmd.v info ui_term

let root_cmd =
  let doc = "Terminal UI for managing Octez services" in
  let info = Cmd.info "octez-manager" ~doc ~version:"0.1.1" in
  Cmd.group
    info
    ~default:ui_term
    [
      Cmd_instance.instance_cmd;
      Cmd_install_node.install_node_cmd;
      Cmd_install_baker.install_baker_cmd;
      Cmd_install_accuser.install_accuser_cmd;
      Cmd_install_dal.install_dal_node_cmd;
      list_cmd;
      purge_all_cmd;
      cleanup_orphans_cmd;
      cleanup_dependencies_cmd;
      list_networks_cmd;
      list_snapshots_cmd;
      ui_cmd;
    ]

let () =
  try exit (Cmd.eval root_cmd)
  with Sys.Break ->
    (* User pressed Ctrl-C during interactive prompts or operations; exit with
       the conventional 130 status code without a stack trace. *)
    prerr_endline "" ;
    exit 130
