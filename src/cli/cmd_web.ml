(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Cmdliner

let controller_html = [%blob "../web/static/index.html"]

let viewer_html = [%blob "../web/static/viewer.html"]

let favicon_png = [%blob "../../favicon.png"]

let web_term =
  let port_arg =
    Arg.(
      value & opt int 8080
      & info ["port"; "p"] ~doc:"TCP port to listen on" ~docv:"PORT")
  in
  let password_arg =
    Arg.(
      value
      & opt (some string) None
      & info ["password"] ~doc:"Controller password" ~docv:"PASSWORD")
  in
  let viewer_password_arg =
    Arg.(
      value
      & opt (some string) None
      & info
          ["viewer-password"]
          ~doc:"Viewer password (defaults to controller password if not set)"
          ~docv:"PASSWORD")
  in
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
      (const (fun port password viewer_password page log logfile ->
           Printexc.record_backtrace true ;
           Octez_manager_lib.Capabilities.register () ;
           Sys.set_signal Sys.sigpipe Sys.Signal_ignore ;
           let result =
             Eio_posix.run @@ fun env ->
             Eio.Switch.run @@ fun sw ->
             Miaou_helpers.Fiber_runtime.init ~env ~sw ;
             Octez_manager_ui.Manager_app.register_and_init ~log ?logfile () ;
             let controller_pw =
               match password with
               | Some _ -> password
               | None -> Sys.getenv_opt "MIAOU_WEB_PASSWORD"
             in
             let viewer_pw =
               match viewer_password with
               | Some _ -> viewer_password
               | None -> (
                   match Sys.getenv_opt "MIAOU_WEB_VIEWER_PASSWORD" with
                   | Some _ as pw -> pw
                   | None -> controller_pw)
             in
             let auth =
               match (controller_pw, viewer_pw) with
               | None, None -> None
               | _ ->
                   Some
                     Miaou_driver_web.Web_driver.
                       {
                         controller_password = controller_pw;
                         viewer_password = viewer_pw;
                       }
             in
             let extra_assets =
               [
                 Miaou_driver_web.Web_driver.
                   {
                     path = "/favicon.png";
                     content_type = "image/png";
                     body = favicon_png;
                   };
               ]
             in
             let start_name =
               Option.value ~default:Octez_manager_ui.Instances.name page
             in
             let initial_page =
               match Miaou.Core.Registry.find start_name with
               | Some p -> p
               | None -> (
                   match
                     Miaou.Core.Registry.find Octez_manager_ui.Instances.name
                   with
                   | Some p ->
                       Printf.eprintf
                         "Unknown page '%s', falling back to '%s'\n%!"
                         start_name
                         Octez_manager_ui.Instances.name ;
                       p
                   | None -> failwith "Instances page missing from registry")
             in
             Printf.eprintf
               "Octez Manager web interface: http://0.0.0.0:%d\n%!"
               port ;
             ignore
               (Miaou_runner_web.Runner_web.run
                  ~enable_mouse:false
                  ~port
                  ?auth
                  ~controller_html
                  ~viewer_html
                  ~extra_assets
                  initial_page) ;
             Ok ()
           in
           match result with
           | Ok () -> `Ok ()
           | Error (`Msg msg) -> Cli_helpers.cmdliner_error msg)
      $ port_arg $ password_arg $ viewer_password_arg $ page_arg $ log_flag
      $ logfile_arg))

let web_cmd =
  let info =
    Cmd.info
      "web"
      ~doc:"Start the web interface (browser-based terminal over WebSocket)"
  in
  Cmd.v info web_term
