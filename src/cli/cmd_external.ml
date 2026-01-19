(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Cmdliner
open Octez_manager_lib

type external_action = Start | Stop | Restart | Show | Logs

let find_external_service ~instance_name =
  match External_service_detector.detect () with
  | Error msg -> Error (`Msg (Printf.sprintf "Detection failed: %s" msg))
  | Ok services -> (
      match
        List.find_opt
          (fun (ext : External_service.t) ->
            ext.suggested_instance_name = instance_name)
          services
      with
      | Some ext -> Ok ext
      | None ->
          Error
            (`Msg
               (Printf.sprintf
                  "External service '%s' not found. Use 'octez-manager list \
                   --external' to see available services."
                  instance_name)))

let show_external_service ext =
  let cfg = ext.External_service.config in
  Printf.printf "External Service: %s\n" ext.suggested_instance_name ;
  Printf.printf "Unit: %s\n" cfg.unit_name ;
  Printf.printf "State: %s\n" cfg.unit_state.active_state ;
  (match cfg.binary_path.value with
  | Some b -> Printf.printf "Binary: %s\n" b
  | None -> Printf.printf "Binary: (not detected)\n") ;
  (match cfg.data_dir.value with
  | Some d -> Printf.printf "Data dir: %s\n" d
  | None -> ()) ;
  (match cfg.rpc_addr.value with
  | Some r -> Printf.printf "RPC: %s\n" r
  | None -> ()) ;
  (match cfg.node_endpoint.value with
  | Some e -> Printf.printf "Node endpoint: %s\n" e
  | None -> ()) ;
  (match cfg.network.value with
  | Some n -> Printf.printf "Network: %s\n" n
  | None -> ()) ;
  match cfg.role.value with
  | Some r -> Printf.printf "Role: %s\n" (External_service.role_to_string r)
  | None -> ()

let show_logs ~instance_name ~unit_name =
  let cmd =
    if Common.is_root () then
      Printf.sprintf "journalctl -u %s -f -n 100" (Filename.quote unit_name)
    else
      Printf.sprintf
        "journalctl --user -u %s -f -n 100"
        (Filename.quote unit_name)
  in
  Printf.printf "Showing logs for %s (Ctrl+C to exit):\n" instance_name ;
  flush stdout ;
  match Sys.command cmd with
  | 0 -> Ok ()
  | n -> Error (`Msg (Printf.sprintf "journalctl exited with code %d" n))

let external_term =
  let instance_name =
    Arg.(
      value
      & pos 0 (some string) None
      & info
          []
          ~docv:"INSTANCE"
          ~doc:
            "Instance name (as shown in 'list --external', e.g., \
             shadownet-baker)")
  in
  let action =
    let actions =
      [
        ("start", Start);
        ("stop", Stop);
        ("restart", Restart);
        ("show", Show);
        ("logs", Logs);
      ]
    in
    Arg.(value & pos 1 (some (enum actions)) None & info [] ~docv:"ACTION")
  in
  let run instance_name action =
    match (instance_name, action) with
    | None, _ -> `Help (`Pager, None)
    | Some _, None ->
        Cli_helpers.cmdliner_error
          "ACTION required (start|stop|restart|show|logs)"
    | Some instance, Some action -> (
        Capabilities.register () ;
        match action with
        | Show -> (
            match find_external_service ~instance_name:instance with
            | Ok ext ->
                show_external_service ext ;
                `Ok ()
            | Error (`Msg e) -> Cli_helpers.cmdliner_error e)
        | Logs -> (
            match find_external_service ~instance_name:instance with
            | Ok ext -> (
                let unit_name = ext.External_service.config.unit_name in
                match show_logs ~instance_name:instance ~unit_name with
                | Ok () -> `Ok ()
                | Error (`Msg e) -> Cli_helpers.cmdliner_error e)
            | Error (`Msg e) -> Cli_helpers.cmdliner_error e)
        | Start | Stop | Restart -> (
            match find_external_service ~instance_name:instance with
            | Ok ext -> (
                let unit_name = ext.External_service.config.unit_name in
                let verb =
                  match action with
                  | Start -> "start"
                  | Stop -> "stop"
                  | Restart -> "restart"
                  | _ -> assert false
                in
                let systemd_fn =
                  match action with
                  | Start -> Systemd.start_unit
                  | Stop -> Systemd.stop_unit
                  | Restart -> Systemd.restart_unit
                  | _ -> assert false
                in
                Printf.printf
                  "%s %s...\n"
                  (String.capitalize_ascii verb)
                  instance ;
                flush stdout ;
                match systemd_fn ~unit_name with
                | Ok () ->
                    Printf.printf "Success: %s %s\n" instance verb ;
                    `Ok ()
                | Error (`Msg e) ->
                    Printf.eprintf "Error: %s\n" e ;
                    `Error (false, e))
            | Error (`Msg e) -> Cli_helpers.cmdliner_error e))
  in
  Term.(ret (const run $ instance_name $ action))

let external_info =
  Cmd.info
    "external"
    ~doc:"Manage external (unmanaged) Octez services"
    ~man:
      [
        `S Manpage.s_description;
        `P
          "Control external Octez services that were not installed by \
           octez-manager.";
        `P
          "External services are detected automatically and can be started, \
           stopped, restarted, or have their logs viewed.";
        `P
          "Use 'octez-manager list --external' to see available external \
           services.";
        `S Manpage.s_examples;
        `P "Show details about an external service:";
        `Pre "  octez-manager external shadownet-baker show";
        `P "Start an external service:";
        `Pre "  octez-manager external shadownet-baker start";
        `P "View logs:";
        `Pre "  octez-manager external shadownet-baker logs";
      ]

let external_cmd = Cmd.v external_info external_term
