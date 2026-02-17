(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

open Cmdliner
open Octez_manager_lib
module S = Service

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
  let theme_arg =
    Arg.(
      value
      & opt (some string) None
      & info
          ["theme"]
          ~doc:
            "Theme name or path (built-ins: dark, light). Can also be set via \
             OCTEZ_MANAGER_THEME."
          ~docv:"THEME")
  in
  Term.(
    ret
      (const (fun page log logfile theme ->
           Printexc.record_backtrace true ;
           Capabilities.register () ;
           (* Ignore SIGPIPE to prevent crashes when subprocesses write to closed pipes *)
           Sys.set_signal Sys.sigpipe Sys.Signal_ignore ;
           let result =
             (* Use POSIX backend to avoid io_uring resource exhaustion *)
             Eio_posix.run @@ fun env ->
             Eio.Switch.run @@ fun sw ->
             let pool =
               Octez_manager_ui.Domain_pool.create
                 ~sw
                 ~domain_mgr:(Eio.Stdenv.domain_mgr env)
                 ~num_domains:4
             in
             Octez_manager_ui.Domain_pool.set pool ;
             Eio_process.init (Eio.Stdenv.process_mgr env) ;
             Binary_downloader.set_parallel_submit
               Octez_manager_ui.Domain_pool.submit ;
             Miaou_helpers.Fiber_runtime.init ~env ~sw ;
             Octez_manager_ui.Manager_app.run ?page ~log ?logfile ?theme ()
           in
           match result with
           | Ok () -> `Ok ()
           | Error (`Msg msg) -> Cli_helpers.cmdliner_error msg)
      $ page_arg $ log_flag $ logfile_arg $ theme_arg))

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
  let info = Cmd.info "octez-manager" ~doc ~version:"0.2.1" in
  Cmd.group
    info
    ~default:ui_term
    [
      Cmd_instance.instance_cmd;
      Cmd_install_node.install_node_cmd;
      Cmd_install_baker.install_baker_cmd;
      Cmd_install_accuser.install_accuser_cmd;
      Cmd_install_dal.install_dal_node_cmd;
      Cmd_install_signatory.install_signatory_cmd;
      Cmd_import.import_cmd;
      Cmd_binaries.binaries_cmd;
      Cmd_self_update.self_update_cmd;
      Cmd_self_update.version_cmd;
      Cmd_utils.list_cmd;
      Cmd_utils.purge_all_cmd;
      Cmd_utils.cleanup_orphans_cmd;
      Cmd_utils.cleanup_dependencies_cmd;
      Cmd_utils.list_networks_cmd;
      Cmd_utils.list_snapshots_cmd;
      Cmd_rpc.rpc_cmd;
      Cmd_web.web_cmd;
      ui_cmd;
    ]

let () =
  try exit (Cmd.eval root_cmd)
  with Sys.Break ->
    (* User pressed Ctrl-C during interactive prompts or operations; exit with
       the conventional 130 status code without a stack trace. *)
    prerr_endline "" ;
    exit 130
