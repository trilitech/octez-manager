(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Cmdliner
open Octez_manager_lib
open Installer_types
module S = Service

let install_dal_node_cmd =
  let instance =
    let doc = "Instance name used for dal-node.env and systemd units." in
    Arg.(value & opt (some string) None & info ["instance"] ~doc ~docv:"NAME")
  in
  let data_dir_opt =
    Arg.(
      value
      & opt (some string) None
      & info ["data-dir"] ~doc:"DAL node data directory" ~docv:"DIR")
  in
  let rpc_addr =
    Arg.(
      value
      & opt string "127.0.0.1:10732"
      & info ["rpc-addr"] ~doc:"DAL node RPC address" ~docv:"ADDR")
  in
  let net_addr =
    Arg.(
      value & opt string "0.0.0.0:11732"
      & info ["net-addr"] ~doc:"DAL node P2P address" ~docv:"ADDR")
  in
  let node_instance =
    let doc =
      "Existing octez-manager node instance to reuse for network resolution. \
       It can also be a custom RPC endpoint for the DAL node to contact."
    in
    Arg.(
      value & opt (some string) None & info ["node-instance"] ~doc ~docv:"NODE")
  in
  let extra_args =
    Arg.(
      value & opt_all string []
      & info
          ["extra-arg"]
          ~doc:"Additional arguments appended to the dal-node command."
          ~docv:"ARG")
  in
  let default_user =
    if Common.is_root () then "octez"
    else fst (Common.current_user_group_names ())
  in
  let service_user =
    Arg.(
      value & opt string default_user
      & info ["service-user"] ~doc:"System user" ~docv:"USER")
  in
  let app_bin_dir =
    Arg.(
      value
      & opt (some string) None
      & info
          ["app-bin-dir"]
          ~doc:"Directory containing Octez binaries"
          ~docv:"DIR")
  in
  let auto_enable =
    Arg.(
      value & flag & info ["no-enable"] ~doc:"Disable automatic enable --now")
  in
  let make instance_opt data_dir_opt rpc_addr net_addr node_instance extra_args
      service_user app_bin_dir no_enable logging_mode =
    match Cli_helpers.resolve_app_bin_dir app_bin_dir with
    | Error msg -> Cli_helpers.cmdliner_error msg
    | Ok app_bin_dir -> (
        let instance_result =
          match Cli_helpers.normalize_opt_string instance_opt with
          | Some inst -> Ok inst
          | None ->
              if Cli_helpers.is_interactive () then
                Ok (Cli_helpers.prompt_required_string "Instance name")
              else Error "Instance name is required in non-interactive mode"
        in
        match instance_result with
        | Error msg -> Cli_helpers.cmdliner_error msg
        | Ok instance -> (
            let data_dir =
              match data_dir_opt with
              | Some dir when String.trim dir <> "" -> dir
              | _ -> Common.default_role_dir "dal-node" instance
            in
            match
              Cli_helpers.resolve_node_instance_or_endpoint ~node_instance
            with
            | Error (`Msg msg) -> Cli_helpers.cmdliner_error msg
            | Ok node_mode -> (
                let node_endpoint =
                  match node_mode with
                  | `Endpoint ep -> Config.endpoint_of_rpc ep
                  | `Instance inst -> (
                      match Service_registry.find ~instance:inst with
                      | Ok (Some svc) ->
                          Config.endpoint_of_rpc svc.Service.rpc_addr
                      | _ -> Config.endpoint_of_rpc "127.0.0.1:8732")
                in
                let maybe_network =
                  match node_mode with
                  | `Instance inst -> (
                      match Service_registry.find ~instance:inst with
                      | Ok (Some svc) -> Ok svc.Service.network
                      | _ ->
                          Teztnets.resolve_octez_node_chain
                            ~endpoint:node_endpoint)
                  | `Endpoint _ ->
                      Teztnets.resolve_octez_node_chain ~endpoint:node_endpoint
                in
                match maybe_network with
                | Error (`Msg msg) -> Cli_helpers.cmdliner_error msg
                | Ok network -> (
                    let depends_on =
                      match node_mode with
                      | `Instance inst -> Some inst
                      | _ -> None
                    in
                    (* Validate DAL node ports *)
                    match
                      Cli_helpers.validate_port_addr
                        ~label:"DAL RPC address"
                        ~addr:rpc_addr
                        ~default:"127.0.0.1:10732"
                        ()
                    with
                    | Error msg -> Cli_helpers.cmdliner_error msg
                    | Ok rpc_addr -> (
                        match
                          Cli_helpers.validate_port_addr
                            ~label:"DAL P2P address"
                            ~addr:net_addr
                            ~default:"0.0.0.0:11732"
                            ()
                        with
                        | Error msg -> Cli_helpers.cmdliner_error msg
                        | Ok net_addr -> (
                            let req : daemon_request =
                              {
                                role = "dal-node";
                                instance;
                                network;
                                history_mode = History_mode.default;
                                data_dir;
                                rpc_addr;
                                net_addr;
                                service_user;
                                app_bin_dir;
                                logging_mode;
                                service_args = extra_args;
                                extra_env =
                                  [
                                    ("OCTEZ_NODE_ENDPOINT", node_endpoint);
                                    ("OCTEZ_DAL_DATA_DIR", data_dir);
                                    ("OCTEZ_DAL_RPC_ADDR", rpc_addr);
                                    ("OCTEZ_DAL_NET_ADDR", net_addr);
                                  ];
                                extra_paths = [data_dir];
                                auto_enable = not no_enable;
                                depends_on;
                                preserve_data = false;
                              }
                            in
                            match Installer.install_daemon req with
                            | Ok service ->
                                Format.printf
                                  "Installed %s (%s)\n"
                                  service.S.instance
                                  service.network ;
                                `Ok ()
                            | Error (`Msg msg) -> Cli_helpers.cmdliner_error msg
                            ))))))
  in
  let term =
    Term.(
      ret
        (const make $ instance $ data_dir_opt $ rpc_addr $ net_addr
       $ node_instance $ extra_args $ service_user $ app_bin_dir $ auto_enable
       $ Cli_helpers.logging_mode_term))
  in
  let info =
    Cmd.info
      "install-dal-node"
      ~doc:"Install a DAL node service (octez-dal-node)"
  in
  Cmd.v info term
