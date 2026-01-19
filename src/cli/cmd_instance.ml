(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

open Cmdliner
open Octez_manager_lib
open Installer_types
module S = Service

type instance_action =
  | Start
  | Stop
  | Restart
  | Remove
  | Purge
  | Show
  | Show_service
  | Logs
  | Edit
  | Export_logs

let instance_term =
  let instance =
    Arg.(value & pos 0 (some string) None & info [] ~docv:"INSTANCE")
  in
  let action =
    let actions =
      [
        ("start", Start);
        ("stop", Stop);
        ("restart", Restart);
        ("remove", Remove);
        ("purge", Purge);
        ("show", Show);
        ("show-service", Show_service);
        ("logs", Logs);
        ("edit", Edit);
        ("export-logs", Export_logs);
      ]
    in
    Arg.(value & pos 1 (some (enum actions)) None & info [] ~docv:"ACTION")
  in
  let delete_data_dir =
    Arg.(
      value & flag
      & info
          ["delete-data-dir"]
          ~doc:"Also delete the recorded data directory when removing.")
  in
  let run instance action delete_data_dir =
    match (instance, action) with
    | None, _ -> `Help (`Pager, None)
    | Some _, None ->
        Cli_helpers.cmdliner_error
          "ACTION required \
           (start|stop|restart|remove|purge|show|show-service|logs|export-logs|edit)"
    | Some inst, Some action -> (
        match action with
        | Start -> (
            (* Check for stopped dependencies *)
            let dep_check =
              Lifecycle.get_stopped_dependencies ~instance:inst ()
            in
            match dep_check with
            | Error (`Msg e) ->
                prerr_endline ("Error checking dependencies: " ^ e) ;
                `Error (false, e)
            | Ok [] -> (
                (* No stopped dependencies, start directly *)
                let result =
                  Lifecycle.start_service ~quiet:false ~instance:inst ()
                in
                match result with
                | Ok () -> (
                    (* Check for stopped dependents *)
                    match
                      Lifecycle.get_stopped_dependents ~instance:inst ()
                    with
                    | Ok [] -> `Ok ()
                    | Ok stopped_deps when Cli_helpers.is_interactive () ->
                        let names =
                          String.concat
                            ", "
                            (List.map
                               (fun s -> s.Service.instance)
                               stopped_deps)
                        in
                        let should_start =
                          Cli_helpers.prompt_yes_no
                            (Printf.sprintf "Start dependents? (%s)" names)
                            ~default:true
                        in
                        if should_start then
                          List.iter
                            (fun dep ->
                              match
                                Lifecycle.start_service
                                  ~quiet:false
                                  ~instance:dep.Service.instance
                                  ()
                              with
                              | Ok () ->
                                  Printf.printf
                                    "Started %s\n%!"
                                    dep.Service.instance
                              | Error (`Msg e) ->
                                  Printf.eprintf
                                    "Failed to start %s: %s\n%!"
                                    dep.Service.instance
                                    e)
                            stopped_deps ;
                        `Ok ()
                    | _ -> `Ok ())
                | Error (`Msg e) -> `Error (false, e))
            | Ok stopped_deps ->
                let names =
                  String.concat
                    ", "
                    (List.map (fun s -> s.Service.instance) stopped_deps)
                in
                if Cli_helpers.is_interactive () then
                  let should_start_deps =
                    Cli_helpers.prompt_yes_no
                      (Printf.sprintf
                         "Dependencies not running (%s). Start them first?"
                         names)
                      ~default:true
                  in
                  if should_start_deps then (
                    (* Start dependencies in order *)
                    let failed = ref false in
                    List.iter
                      (fun dep ->
                        if not !failed then
                          match
                            Lifecycle.start_service
                              ~quiet:false
                              ~instance:dep.Service.instance
                              ()
                          with
                          | Ok () ->
                              Printf.printf
                                "Started %s\n%!"
                                dep.Service.instance
                          | Error (`Msg e) ->
                              Printf.eprintf
                                "Failed to start %s: %s\n%!"
                                dep.Service.instance
                                e ;
                              failed := true)
                      stopped_deps ;
                    if !failed then
                      `Error (false, "Failed to start dependencies")
                    else
                      (* Now start the target instance *)
                      let result =
                        Lifecycle.start_service ~quiet:false ~instance:inst ()
                      in
                      match result with
                      | Ok () -> (
                          (* Check for stopped dependents *)
                          match
                            Lifecycle.get_stopped_dependents ~instance:inst ()
                          with
                          | Ok [] -> `Ok ()
                          | Ok stopped_deps ->
                              let names =
                                String.concat
                                  ", "
                                  (List.map
                                     (fun s -> s.Service.instance)
                                     stopped_deps)
                              in
                              let should_start =
                                Cli_helpers.prompt_yes_no
                                  (Printf.sprintf
                                     "Start dependents? (%s)"
                                     names)
                                  ~default:true
                              in
                              if should_start then
                                List.iter
                                  (fun dep ->
                                    match
                                      Lifecycle.start_service
                                        ~quiet:false
                                        ~instance:dep.Service.instance
                                        ()
                                    with
                                    | Ok () ->
                                        Printf.printf
                                          "Started %s\n%!"
                                          dep.Service.instance
                                    | Error (`Msg e) ->
                                        Printf.eprintf
                                          "Failed to start %s: %s\n%!"
                                          dep.Service.instance
                                          e)
                                  stopped_deps ;
                              `Ok ()
                          | _ -> `Ok ())
                      | Error (`Msg e) -> `Error (false, e))
                  else (
                    prerr_endline
                      "Cancelled - dependencies must be running first." ;
                    `Ok ())
                else
                  (* Non-interactive mode - just fail like before *)
                  Cli_helpers.run_result
                    (Lifecycle.start_service ~quiet:false ~instance:inst ()))
        | Stop ->
            Cli_helpers.run_result
              (Lifecycle.stop_service ~quiet:false ~instance:inst ())
        | Restart -> (
            (* Check for stopped dependencies *)
            let dep_check =
              Lifecycle.get_stopped_dependencies ~instance:inst ()
            in
            match dep_check with
            | Error (`Msg e) ->
                prerr_endline ("Error checking dependencies: " ^ e) ;
                `Error (false, e)
            | Ok [] -> (
                (* No stopped dependencies, restart directly *)
                let result =
                  Lifecycle.restart_service ~quiet:false ~instance:inst ()
                in
                match result with
                | Ok () -> (
                    (* Check for stopped dependents *)
                    match
                      Lifecycle.get_stopped_dependents ~instance:inst ()
                    with
                    | Ok [] -> `Ok ()
                    | Ok stopped_deps when Cli_helpers.is_interactive () ->
                        let names =
                          String.concat
                            ", "
                            (List.map
                               (fun s -> s.Service.instance)
                               stopped_deps)
                        in
                        let should_restart =
                          Cli_helpers.prompt_yes_no
                            (Printf.sprintf "Restart dependents? (%s)" names)
                            ~default:true
                        in
                        if should_restart then
                          List.iter
                            (fun dep ->
                              match
                                Lifecycle.restart_service
                                  ~quiet:false
                                  ~instance:dep.Service.instance
                                  ()
                              with
                              | Ok () ->
                                  Printf.printf
                                    "Restarted %s\n%!"
                                    dep.Service.instance
                              | Error (`Msg e) ->
                                  Printf.eprintf
                                    "Failed to restart %s: %s\n%!"
                                    dep.Service.instance
                                    e)
                            stopped_deps ;
                        `Ok ()
                    | _ -> `Ok ())
                | Error (`Msg e) -> `Error (false, e))
            | Ok stopped_deps ->
                let names =
                  String.concat
                    ", "
                    (List.map (fun s -> s.Service.instance) stopped_deps)
                in
                if Cli_helpers.is_interactive () then
                  let should_start_deps =
                    Cli_helpers.prompt_yes_no
                      (Printf.sprintf
                         "Dependencies not running (%s). Start them first?"
                         names)
                      ~default:true
                  in
                  if should_start_deps then (
                    (* Start dependencies in order *)
                    let failed = ref false in
                    List.iter
                      (fun dep ->
                        if not !failed then
                          match
                            Lifecycle.start_service
                              ~quiet:false
                              ~instance:dep.Service.instance
                              ()
                          with
                          | Ok () ->
                              Printf.printf
                                "Started %s\n%!"
                                dep.Service.instance
                          | Error (`Msg e) ->
                              Printf.eprintf
                                "Failed to start %s: %s\n%!"
                                dep.Service.instance
                                e ;
                              failed := true)
                      stopped_deps ;
                    if !failed then
                      `Error (false, "Failed to start dependencies")
                    else
                      (* Now restart the target instance *)
                      let result =
                        Lifecycle.restart_service ~quiet:false ~instance:inst ()
                      in
                      match result with
                      | Ok () -> (
                          (* Check for stopped dependents *)
                          match
                            Lifecycle.get_stopped_dependents ~instance:inst ()
                          with
                          | Ok [] -> `Ok ()
                          | Ok stopped_deps ->
                              let names =
                                String.concat
                                  ", "
                                  (List.map
                                     (fun s -> s.Service.instance)
                                     stopped_deps)
                              in
                              let should_restart =
                                Cli_helpers.prompt_yes_no
                                  (Printf.sprintf
                                     "Restart dependents? (%s)"
                                     names)
                                  ~default:true
                              in
                              if should_restart then
                                List.iter
                                  (fun dep ->
                                    match
                                      Lifecycle.restart_service
                                        ~quiet:false
                                        ~instance:dep.Service.instance
                                        ()
                                    with
                                    | Ok () ->
                                        Printf.printf
                                          "Restarted %s\n%!"
                                          dep.Service.instance
                                    | Error (`Msg e) ->
                                        Printf.eprintf
                                          "Failed to restart %s: %s\n%!"
                                          dep.Service.instance
                                          e)
                                  stopped_deps ;
                              `Ok ()
                          | _ -> `Ok ())
                      | Error (`Msg e) -> `Error (false, e))
                  else (
                    prerr_endline
                      "Cancelled - dependencies must be running first." ;
                    `Ok ())
                else
                  (* Non-interactive mode - just fail like before *)
                  Cli_helpers.run_result
                    (Lifecycle.restart_service ~quiet:false ~instance:inst ()))
        | Remove -> (
            (* Check for dependents and confirm if any *)
            match Service_registry.find ~instance:inst with
            | Error (`Msg msg) -> Cli_helpers.cmdliner_error msg
            | Ok None ->
                Cli_helpers.cmdliner_error
                  (Printf.sprintf "Unknown instance '%s'" inst)
            | Ok (Some svc) ->
                let proceed =
                  if svc.S.dependents = [] then true
                  else if Cli_helpers.is_interactive () then (
                    Format.printf
                      "This will stop dependent instances: %s@."
                      (String.concat ", " svc.S.dependents) ;
                    Cli_helpers.prompt_yes_no
                      "Proceed with removal?"
                      ~default:false)
                  else (
                    Format.printf
                      "Instance has dependents: %s. Use --yes to confirm.@."
                      (String.concat ", " svc.S.dependents) ;
                    false)
                in
                if proceed then
                  Cli_helpers.run_result
                    (Removal.remove_service
                       ~quiet:false
                       ~delete_data_dir
                       ~instance:inst
                       ())
                else `Ok ())
        | Purge ->
            Cli_helpers.run_result
              (Removal.purge_service
                 ~quiet:false
                 ~prompt_yes_no:
                   (if Cli_helpers.is_interactive () then
                      Cli_helpers.prompt_yes_no
                    else fun _ ~default:_ -> false)
                 ~instance:inst
                 ())
        | Show -> (
            match Service_registry.find ~instance:inst with
            | Ok (Some svc) ->
                Cli_output.print_service_details svc ;
                `Ok ()
            | Ok None ->
                Cli_helpers.cmdliner_error
                  (Printf.sprintf "Unknown instance '%s'" inst)
            | Error (`Msg msg) -> Cli_helpers.cmdliner_error msg)
        | Show_service -> (
            match Service_registry.find ~instance:inst with
            | Error (`Msg msg) -> Cli_helpers.cmdliner_error msg
            | Ok None ->
                Cli_helpers.cmdliner_error
                  (Printf.sprintf "Unknown instance '%s'" inst)
            | Ok (Some svc) ->
                let role = svc.S.role in
                let unit = Systemd.unit_name role inst in
                let print_dropin () =
                  let path = Cli_output.dropin_path_for ~role ~instance:inst in
                  if Sys.file_exists path then
                    try
                      let contents = Cli_output.slurp_file path in
                      Format.printf "# %s@.%s@." path contents
                    with Sys_error msg -> prerr_endline msg
                in
                let () =
                  match Systemd.cat_unit ~role ~instance:inst with
                  | Ok contents ->
                      Format.printf "# systemctl cat %s@.%s@." unit contents
                  | Error (`Msg msg) ->
                      prerr_endline ("systemctl cat failed: " ^ msg) ;
                      print_dropin ()
                in
                let () =
                  match Systemd.is_enabled ~role ~instance:inst with
                  | Ok state ->
                      Format.printf
                        "@.# systemctl is-enabled %s@.%s@."
                        unit
                        state
                  | Error (`Msg msg) ->
                      prerr_endline ("systemctl is-enabled failed: " ^ msg)
                in
                let () =
                  match Systemd.status ~role ~instance:inst with
                  | Ok status ->
                      Format.printf
                        "@.# systemctl status %s --no-pager@.%s@."
                        unit
                        status
                  | Error (`Msg msg) ->
                      prerr_endline ("systemctl status failed: " ^ msg)
                in
                `Ok ())
        | Logs -> (
            match Service_registry.find ~instance:inst with
            | Error (`Msg msg) -> Cli_helpers.cmdliner_error msg
            | Ok None ->
                Cli_helpers.cmdliner_error
                  (Printf.sprintf "Unknown instance '%s'" inst)
            | Ok (Some svc) ->
                let role = svc.S.role in
                let user_flag = if Common.is_root () then "" else "--user " in
                let unit = Systemd.unit_name role inst in

                Format.printf "# Monitor logs via journald:@." ;
                Format.printf "journalctl %s-u %s -f@." user_flag unit ;

                (match Log_viewer.get_daily_log_file ~role ~instance:inst with
                | Ok path ->
                    Format.printf "@.# Monitor daily logs via tail:@." ;
                    Format.printf "tail -f %s@." path
                | Error _ -> ()) ;

                `Ok ())
        | Edit -> (
            match Service_registry.find ~instance:inst with
            | Error (`Msg msg) -> Cli_helpers.cmdliner_error msg
            | Ok None ->
                Cli_helpers.cmdliner_error
                  (Printf.sprintf "Unknown instance '%s'" inst)
            | Ok (Some svc) ->
                let role = svc.S.role in
                (* List dependents that will be stopped *)
                let () =
                  if svc.S.dependents <> [] then
                    Format.printf
                      "@[<v>@,\
                       This will stop the following dependents:@,\
                      \  %s@,\
                       @]"
                      (String.concat ", " svc.dependents)
                in
                (* Confirm before proceeding in interactive mode *)
                let proceed =
                  if not (Cli_helpers.is_interactive ()) then true
                  else
                    Cli_helpers.prompt_yes_no "Proceed with edit?" ~default:true
                in
                if not proceed then (
                  print_endline "Cancelled." ;
                  `Ok ())
                else (
                  (* Stop the instance (cascade stops dependents) *)
                  (match
                     Lifecycle.stop_service ~quiet:false ~instance:inst ()
                   with
                  | Ok () -> ()
                  | Error (`Msg msg) ->
                      Format.eprintf "Warning: failed to stop service: %s@." msg) ;
                  (* Show current values - role-specific configuration *)
                  Format.printf "@.Editing instance '%s' (role: %s)@." inst role ;
                  Format.printf "@.Current configuration:@." ;
                  (* Read env file for role-specific values *)
                  let env =
                    match Node_env.read ~inst with
                    | Ok pairs -> pairs
                    | Error _ -> []
                  in
                  let lookup key =
                    match List.assoc_opt key env with
                    | Some v -> String.trim v
                    | None -> ""
                  in
                  (* Common fields *)
                  Format.printf "  Service user: %s@." svc.service_user ;
                  Format.printf "  App bin dir: %s@." svc.app_bin_dir ;
                  (* Role-specific fields *)
                  (match role with
                  | "node" ->
                      Format.printf "  Network: %s@." svc.network ;
                      Format.printf
                        "  History mode: %s@."
                        (History_mode.to_string svc.history_mode) ;
                      Format.printf "  Data dir: %s@." svc.data_dir ;
                      Format.printf "  RPC addr: %s@." svc.rpc_addr ;
                      Format.printf "  P2P addr: %s@." svc.net_addr
                  | "baker" ->
                      let base_dir = lookup "OCTEZ_BAKER_BASE_DIR" in
                      let node_inst = lookup "OCTEZ_NODE_INSTANCE" in
                      let node_ep = lookup "OCTEZ_NODE_ENDPOINT" in
                      let dal_cfg = lookup "OCTEZ_DAL_CONFIG" in
                      let delegates = lookup "OCTEZ_BAKER_DELEGATES_CSV" in
                      let lb_vote = lookup "OCTEZ_BAKER_LB_VOTE" in
                      Format.printf "  Base dir: %s@." base_dir ;
                      if node_inst <> "" then
                        Format.printf "  Node instance: %s@." node_inst
                      else Format.printf "  Node endpoint: %s@." node_ep ;
                      (match String.lowercase_ascii dal_cfg with
                      | "disabled" -> Format.printf "  DAL config: opt-out@."
                      | "" -> Format.printf "  DAL config: auto@."
                      | ep -> Format.printf "  DAL endpoint: %s@." ep) ;
                      if delegates <> "" then
                        Format.printf "  Delegates: %s@." delegates ;
                      if lb_vote <> "" then
                        Format.printf "  LB vote: %s@." lb_vote
                  | "accuser" ->
                      let base_dir = lookup "OCTEZ_CLIENT_BASE_DIR" in
                      let node_inst = lookup "OCTEZ_NODE_INSTANCE" in
                      let node_ep = lookup "OCTEZ_NODE_ENDPOINT" in
                      Format.printf "  Base dir: %s@." base_dir ;
                      if node_inst <> "" then
                        Format.printf "  Node instance: %s@." node_inst
                      else Format.printf "  Node endpoint: %s@." node_ep
                  | "dal-node" | "dal" ->
                      let dal_data_dir = lookup "OCTEZ_DAL_DATA_DIR" in
                      let node_inst = lookup "OCTEZ_NODE_INSTANCE" in
                      let node_ep = lookup "OCTEZ_NODE_ENDPOINT" in
                      let rpc_addr = lookup "OCTEZ_DAL_RPC_ADDR" in
                      let net_addr = lookup "OCTEZ_DAL_NET_ADDR" in
                      Format.printf "  DAL data dir: %s@." dal_data_dir ;
                      if node_inst <> "" then
                        Format.printf "  Node instance: %s@." node_inst
                      else Format.printf "  Node endpoint: %s@." node_ep ;
                      if rpc_addr <> "" then
                        Format.printf "  RPC addr: %s@." rpc_addr ;
                      if net_addr <> "" then
                        Format.printf "  P2P addr: %s@." net_addr
                  | _ -> ()) ;
                  (* Extra args for all roles *)
                  let extra_args_str = String.concat " " svc.extra_args in
                  if extra_args_str <> "" then
                    Format.printf "  Extra args: %s@." extra_args_str ;
                  (* Dependencies *)
                  if svc.depends_on <> None then
                    Format.printf
                      "  Depends on: %s@."
                      (Option.value ~default:"(none)" svc.depends_on) ;
                  if svc.dependents <> [] then
                    Format.printf
                      "  Dependents: %s@."
                      (String.concat ", " svc.dependents) ;
                  (* Interactive edit based on role *)
                  Format.printf
                    "@.Enter new values (press Enter to keep current):@." ;
                  (* Prompt for new instance name first *)
                  let new_instance =
                    Cli_helpers.prompt_input
                      ~default:(inst, inst)
                      "Instance name"
                    |> Option.value ~default:inst
                  in
                  let is_rename = new_instance <> inst in
                  let result =
                    match role with
                    | "node" ->
                        (* Node: edit RPC addr, P2P addr, extra args *)
                        let new_rpc =
                          Cli_helpers.prompt_input
                            ~default:(svc.rpc_addr, svc.rpc_addr)
                            "RPC address"
                          |> Option.value ~default:svc.rpc_addr
                        in
                        let new_net =
                          Cli_helpers.prompt_input
                            ~default:(svc.net_addr, svc.net_addr)
                            "P2P address"
                          |> Option.value ~default:svc.net_addr
                        in
                        let new_extra =
                          Cli_helpers.prompt_input
                            ~default:(extra_args_str, extra_args_str)
                            "Extra args"
                          |> Option.value ~default:extra_args_str
                        in
                        let new_extra_args =
                          String.split_on_char ' ' new_extra
                          |> List.map String.trim
                          |> List.filter (( <> ) "")
                        in
                        (* Validate ports *)
                        let ( let* ) = Result.bind in
                        let* new_rpc =
                          Cli_helpers.validate_port_addr
                            ~label:"RPC address"
                            ~addr:new_rpc
                            ~default:svc.rpc_addr
                            ~exclude_instance:inst
                            ()
                        in
                        let* new_net =
                          Cli_helpers.validate_port_addr
                            ~label:"P2P address"
                            ~addr:new_net
                            ~default:svc.net_addr
                            ~exclude_instance:inst
                            ()
                        in
                        let req : Installer_types.node_request =
                          {
                            instance = new_instance;
                            network = svc.network;
                            history_mode = svc.history_mode;
                            data_dir = Some svc.data_dir;
                            rpc_addr = new_rpc;
                            net_addr = new_net;
                            service_user = svc.service_user;
                            app_bin_dir = svc.app_bin_dir;
                            extra_args = new_extra_args;
                            auto_enable = true;
                            logging_mode = svc.logging_mode;
                            bootstrap = Installer_types.Genesis;
                            preserve_data = true;
                            snapshot_no_check = false;
                            tmp_dir = None;
                            keep_snapshot = false;
                          }
                        in
                        Result.map_error
                          (fun (`Msg s) -> s)
                          (Node.install_node req)
                    | "baker" ->
                        (* Baker: edit delegates, LB vote, extra args *)
                        let delegates = lookup "OCTEZ_BAKER_DELEGATES_CSV" in
                        let lb_vote = lookup "OCTEZ_BAKER_LB_VOTE" in
                        let base_dir = lookup "OCTEZ_BAKER_BASE_DIR" in
                        (* Get known delegate addresses for completion *)
                        let known_delegates =
                          if base_dir <> "" then
                            match
                              Keys_reader.read_public_key_hashes ~base_dir
                            with
                            | Ok keys ->
                                List.map (fun k -> k.Keys_reader.value) keys
                            | Error _ -> []
                          else []
                        in
                        let new_delegates =
                          if known_delegates = [] then
                            Cli_helpers.prompt_input
                              ~default:(delegates, delegates)
                              "Delegates (comma-separated)"
                            |> Option.value ~default:delegates
                          else (
                            Format.printf
                              "  Known delegates: %s@."
                              (String.concat ", " known_delegates) ;
                            match
                              Cli_helpers.prompt_with_multi_completion
                                "Delegates (comma-separated)"
                                known_delegates
                            with
                            | Some "" | None -> delegates
                            | Some v -> String.trim v)
                        in
                        let new_lb_vote =
                          let completions = ["pass"; "on"; "off"] in
                          let default_val =
                            if lb_vote = "" then "pass" else lb_vote
                          in
                          match
                            Cli_helpers.prompt_with_completion
                              "LB vote"
                              completions
                          with
                          | Some "" | None -> default_val
                          | Some v -> String.trim v
                        in
                        let new_extra =
                          Cli_helpers.prompt_input
                            ~default:(extra_args_str, extra_args_str)
                            "Extra args"
                          |> Option.value ~default:extra_args_str
                        in
                        let new_extra_args =
                          String.split_on_char ' ' new_extra
                          |> List.map String.trim
                          |> List.filter (( <> ) "")
                        in
                        let delegates_list =
                          String.split_on_char ',' new_delegates
                          |> List.map String.trim
                          |> List.filter (( <> ) "")
                        in
                        (* Node instance selection with completion *)
                        let current_node =
                          match svc.depends_on with
                          | Some inst -> inst
                          | None -> lookup "OCTEZ_NODE_ENDPOINT"
                        in
                        let node_services =
                          match Service_registry.list () with
                          | Ok svcs ->
                              List.filter
                                (fun (s : Service.t) -> s.role = "node")
                                svcs
                          | Error _ -> []
                        in
                        let node_names =
                          List.map
                            (fun (s : Service.t) -> s.instance)
                            node_services
                        in
                        let new_node =
                          if node_names = [] then current_node
                          else (
                            Format.printf
                              "  Available nodes: %s@."
                              (String.concat ", " node_names) ;
                            match
                              Cli_helpers.prompt_with_completion
                                "Node instance"
                                node_names
                            with
                            | Some "" | None -> current_node
                            | Some v -> String.trim v)
                        in
                        let node_mode =
                          if List.mem new_node node_names then
                            Installer_types.Local_instance new_node
                          else Installer_types.Remote_endpoint new_node
                        in
                        (* DAL node selection with completion *)
                        let current_dal = lookup "OCTEZ_DAL_INSTANCE" in
                        let current_dal_config = lookup "OCTEZ_DAL_CONFIG" in
                        let dal_services =
                          match Service_registry.list () with
                          | Ok svcs ->
                              List.filter
                                (fun (s : Service.t) ->
                                  s.role = "dal-node" || s.role = "dal")
                                svcs
                          | Error _ -> []
                        in
                        let dal_names =
                          List.map
                            (fun (s : Service.t) -> s.instance)
                            dal_services
                        in
                        let dal_config, dal_node =
                          if dal_names = [] then
                            (* No DAL nodes, keep current config *)
                            let cfg =
                              match
                                String.lowercase_ascii current_dal_config
                              with
                              | "disabled" -> Installer_types.Dal_disabled
                              | "" -> Installer_types.Dal_auto
                              | ep -> Installer_types.Dal_endpoint ep
                            in
                            ( cfg,
                              if current_dal = "" then None
                              else Some current_dal )
                          else (
                            Format.printf
                              "  Available DAL nodes: %s@."
                              (String.concat ", " ("none" :: dal_names)) ;
                            match
                              Cli_helpers.prompt_with_completion
                                "DAL node"
                                ("none" :: "auto" :: dal_names)
                            with
                            | Some "" | None ->
                                if current_dal <> "" then
                                  ( Installer_types.Dal_endpoint
                                      (Config.endpoint_of_rpc
                                         (match
                                            List.find_opt
                                              (fun (s : Service.t) ->
                                                s.instance = current_dal)
                                              dal_services
                                          with
                                         | Some s -> s.rpc_addr
                                         | None -> "127.0.0.1:10732")),
                                    Some current_dal )
                                else
                                  let cfg =
                                    match
                                      String.lowercase_ascii current_dal_config
                                    with
                                    | "disabled" -> Installer_types.Dal_disabled
                                    | "" -> Installer_types.Dal_auto
                                    | ep -> Installer_types.Dal_endpoint ep
                                  in
                                  (cfg, None)
                            | Some "none" -> (Installer_types.Dal_disabled, None)
                            | Some "auto" -> (Installer_types.Dal_auto, None)
                            | Some selected ->
                                if List.mem selected dal_names then
                                  let rpc =
                                    match
                                      List.find_opt
                                        (fun (s : Service.t) ->
                                          s.instance = selected)
                                        dal_services
                                    with
                                    | Some s -> s.rpc_addr
                                    | None -> "127.0.0.1:10732"
                                  in
                                  ( Installer_types.Dal_endpoint
                                      (Config.endpoint_of_rpc rpc),
                                    Some selected )
                                else
                                  ( Installer_types.Dal_endpoint
                                      (Config.endpoint_of_rpc selected),
                                    None ))
                        in
                        let req : Installer_types.baker_request =
                          {
                            instance = new_instance;
                            node_mode;
                            dal_config;
                            dal_node;
                            base_dir = Some (lookup "OCTEZ_BAKER_BASE_DIR");
                            delegates = delegates_list;
                            liquidity_baking_vote =
                              (if new_lb_vote = "" then None
                               else Some new_lb_vote);
                            service_user = svc.service_user;
                            app_bin_dir = svc.app_bin_dir;
                            logging_mode = svc.logging_mode;
                            extra_args = new_extra_args;
                            auto_enable = true;
                            preserve_data = true;
                          }
                        in
                        Result.map_error
                          (fun (`Msg s) -> s)
                          (Baker.install_baker req)
                    | "accuser" ->
                        (* Accuser: edit node instance, extra args *)
                        (* Node instance selection with completion *)
                        let current_node =
                          match svc.depends_on with
                          | Some inst -> inst
                          | None -> lookup "OCTEZ_NODE_ENDPOINT"
                        in
                        let node_services =
                          match Service_registry.list () with
                          | Ok svcs ->
                              List.filter
                                (fun (s : Service.t) -> s.role = "node")
                                svcs
                          | Error _ -> []
                        in
                        let node_names =
                          List.map
                            (fun (s : Service.t) -> s.instance)
                            node_services
                        in
                        let new_node =
                          if node_names = [] then current_node
                          else (
                            Format.printf
                              "  Available nodes: %s@."
                              (String.concat ", " node_names) ;
                            match
                              Cli_helpers.prompt_with_completion
                                "Node instance"
                                node_names
                            with
                            | Some "" | None -> current_node
                            | Some v -> String.trim v)
                        in
                        let node_mode =
                          if List.mem new_node node_names then
                            Installer_types.Local_instance new_node
                          else Installer_types.Remote_endpoint new_node
                        in
                        let new_extra =
                          Cli_helpers.prompt_input
                            ~default:(extra_args_str, extra_args_str)
                            "Extra args"
                          |> Option.value ~default:extra_args_str
                        in
                        let new_extra_args =
                          String.split_on_char ' ' new_extra
                          |> List.map String.trim
                          |> List.filter (( <> ) "")
                        in
                        let req : Installer_types.accuser_request =
                          {
                            instance = new_instance;
                            node_mode;
                            base_dir = Some (lookup "OCTEZ_CLIENT_BASE_DIR");
                            service_user = svc.service_user;
                            app_bin_dir = svc.app_bin_dir;
                            logging_mode = svc.logging_mode;
                            extra_args = new_extra_args;
                            auto_enable = true;
                            preserve_data = true;
                          }
                        in
                        Result.map_error
                          (fun (`Msg s) -> s)
                          (Accuser.install_accuser req)
                    | "dal-node" | "dal" ->
                        (* DAL node: edit node instance, RPC addr, P2P addr, extra args *)
                        (* Node instance selection with completion *)
                        let current_node =
                          match svc.depends_on with
                          | Some inst -> inst
                          | None -> lookup "OCTEZ_NODE_ENDPOINT"
                        in
                        let node_services =
                          match Service_registry.list () with
                          | Ok svcs ->
                              List.filter
                                (fun (s : Service.t) -> s.role = "node")
                                svcs
                          | Error _ -> []
                        in
                        let node_names =
                          List.map
                            (fun (s : Service.t) -> s.instance)
                            node_services
                        in
                        let new_node =
                          if node_names = [] then current_node
                          else (
                            Format.printf
                              "  Available nodes: %s@."
                              (String.concat ", " node_names) ;
                            match
                              Cli_helpers.prompt_with_completion
                                "Node instance"
                                node_names
                            with
                            | Some "" | None -> current_node
                            | Some v -> String.trim v)
                        in
                        let new_depends_on, new_node_endpoint =
                          if List.mem new_node node_names then
                            (* Local node instance *)
                            let node_svc =
                              List.find_opt
                                (fun (s : Service.t) -> s.instance = new_node)
                                node_services
                            in
                            let ep =
                              match node_svc with
                              | Some s -> Config.endpoint_of_rpc s.rpc_addr
                              | None -> "http://127.0.0.1:8732"
                            in
                            (Some new_node, ep)
                          else
                            (* Remote endpoint *)
                            (None, Config.endpoint_of_rpc new_node)
                        in
                        let dal_rpc = lookup "OCTEZ_DAL_RPC_ADDR" in
                        let dal_net = lookup "OCTEZ_DAL_NET_ADDR" in
                        let new_rpc =
                          Cli_helpers.prompt_input
                            ~default:(dal_rpc, dal_rpc)
                            "DAL RPC address"
                          |> Option.value ~default:dal_rpc
                        in
                        let new_net =
                          Cli_helpers.prompt_input
                            ~default:(dal_net, dal_net)
                            "DAL P2P address"
                          |> Option.value ~default:dal_net
                        in
                        let new_extra =
                          Cli_helpers.prompt_input
                            ~default:(extra_args_str, extra_args_str)
                            "Extra args"
                          |> Option.value ~default:extra_args_str
                        in
                        let new_extra_args =
                          String.split_on_char ' ' new_extra
                          |> List.map String.trim
                          |> List.filter (( <> ) "")
                        in
                        (* Validate ports *)
                        let ( let* ) = Result.bind in
                        let* new_rpc =
                          Cli_helpers.validate_port_addr
                            ~label:"DAL RPC address"
                            ~addr:new_rpc
                            ~default:dal_rpc
                            ~exclude_instance:inst
                            ()
                        in
                        let* new_net =
                          Cli_helpers.validate_port_addr
                            ~label:"DAL P2P address"
                            ~addr:new_net
                            ~default:dal_net
                            ~exclude_instance:inst
                            ()
                        in
                        let dal_data_dir = lookup "OCTEZ_DAL_DATA_DIR" in
                        let req : Installer_types.daemon_request =
                          {
                            role = "dal-node";
                            instance = new_instance;
                            network = svc.network;
                            history_mode = svc.history_mode;
                            data_dir = dal_data_dir;
                            rpc_addr = new_rpc;
                            net_addr = new_net;
                            service_user = svc.service_user;
                            app_bin_dir = svc.app_bin_dir;
                            logging_mode = svc.logging_mode;
                            service_args = new_extra_args;
                            extra_env =
                              [
                                ("OCTEZ_NODE_ENDPOINT", new_node_endpoint);
                                ("OCTEZ_DAL_DATA_DIR", dal_data_dir);
                                ("OCTEZ_DAL_RPC_ADDR", new_rpc);
                                ("OCTEZ_DAL_NET_ADDR", new_net);
                              ];
                            extra_paths = [dal_data_dir];
                            auto_enable = true;
                            depends_on = new_depends_on;
                            preserve_data = true;
                          }
                        in
                        Result.map_error
                          (fun (`Msg s) -> s)
                          (Dal_node.install_daemon req)
                    | _ ->
                        Error
                          (Printf.sprintf
                             "Edit not supported for role '%s'"
                             role)
                  in
                  match result with
                  | Ok _service -> (
                      (* Handle rename: clean up old instance if name changed *)
                      let cleanup_result =
                        if is_rename then (
                          Format.printf
                            "@.Renaming instance from '%s' to '%s'...@."
                            inst
                            new_instance ;
                          Removal.cleanup_renamed_instance
                            ~quiet:false
                            ~old_instance:inst
                            ~new_instance
                            ())
                        else Ok ()
                      in
                      match cleanup_result with
                      | Ok () ->
                          Format.printf
                            "@.Instance '%s' updated successfully.@."
                            new_instance ;
                          Format.printf
                            "@.To restart the stopped instances:@.  \
                             octez-manager instance %s start@."
                            new_instance ;
                          `Ok ()
                      | Error (`Msg msg) ->
                          Format.printf
                            "@.Instance '%s' updated but rename cleanup \
                             failed: %s@."
                            new_instance
                            msg ;
                          `Ok ())
                  | Error msg ->
                      Cli_helpers.cmdliner_error
                        (Printf.sprintf "Edit failed: %s" msg)))
        | Export_logs -> (
            match Service_registry.find ~instance:inst with
            | Error (`Msg msg) -> Cli_helpers.cmdliner_error msg
            | Ok None ->
                Cli_helpers.cmdliner_error
                  (Printf.sprintf "Unknown instance '%s'" inst)
            | Ok (Some svc) -> (
                match Log_export.export_logs ~instance:inst ~svc with
                | Ok archive_path ->
                    Format.printf "Logs exported to: %s@." archive_path ;
                    `Ok ()
                | Error (`Msg msg) ->
                    Cli_helpers.cmdliner_error
                      (Printf.sprintf "Export failed: %s" msg))))
  in
  Term.(ret (const run $ instance $ action $ delete_data_dir))

let instance_cmd =
  let info = Cmd.info "instance" ~doc:"Manage existing Octez services." in
  Cmd.v info instance_term
