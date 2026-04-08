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

let install_index_cmd =
  let instance =
    let doc = "Instance name used for index.env and systemd units." in
    Arg.(value & opt (some string) None & info ["instance"] ~doc ~docv:"NAME")
  in
  let base_dir_opt =
    Arg.(
      value
      & opt (some string) None
      & info ["base-dir"] ~doc:"octez-index data directory" ~docv:"DIR")
  in
  let rpc_addr =
    Arg.(
      value
      & opt string "127.0.0.1:8733"
      & info
          ["rpc-addr"]
          ~doc:"octez-index REST API listen address"
          ~docv:"ADDR")
  in
  let node_instance =
    let doc =
      "Existing octez-manager node instance to connect to. It can also be a \
       custom RPC endpoint."
    in
    Arg.(
      value & opt (some string) None & info ["node-instance"] ~doc ~docv:"NODE")
  in
  let watched_addresses =
    Arg.(
      value & opt_all string []
      & info
          ["watched-address"]
          ~doc:
            "Public key hash to watch. May be repeated. Omit to watch all \
             addresses."
          ~docv:"PKH")
  in
  let db_name =
    Arg.(
      value
      & opt (some string) None
      & info
          ["db-name"]
          ~doc:"SQLite database filename (no path separators)"
          ~docv:"NAME")
  in
  let extra_args =
    Arg.(
      value & opt_all string []
      & info
          ["extra-arg"]
          ~doc:"Additional arguments appended to the octez-index command."
          ~docv:"ARG")
  in
  let default_user =
    if Paths.is_root () then "octez"
    else fst (Paths.current_user_group_names ())
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
  let octez_version =
    let doc =
      "Use a managed Octez version. Overrides --app-bin-dir. Download versions \
       with: octez-manager binaries download VERSION"
    in
    Arg.(
      value
      & opt (some string) None
      & info ["octez-version"] ~doc ~docv:"VERSION")
  in
  let bin_dir_alias =
    let doc =
      "Use a registered directory by alias. Overrides --app-bin-dir. Create \
       aliases with: octez-manager binaries register"
    in
    Arg.(
      value & opt (some string) None & info ["bin-dir-alias"] ~doc ~docv:"ALIAS")
  in
  let auto_enable =
    Arg.(
      value & flag & info ["no-enable"] ~doc:"Disable automatic enable --now")
  in
  let make instance_opt base_dir_opt rpc_addr node_instance watched_addresses
      db_name extra_args service_user app_bin_dir octez_version bin_dir_alias
      no_enable logging_mode =
    match
      Cli_helpers.resolve_app_bin_dir ?octez_version ?bin_dir_alias app_bin_dir
    with
    | Error msg -> Cli_helpers.cmdliner_error msg
    | Ok (app_bin_dir, bin_source) -> (
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
            let base_dir =
              match base_dir_opt with
              | Some dir when String.trim dir <> "" -> dir
              | _ -> Paths.default_role_dir "index" instance
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
                          Rpc_addr.to_endpoint svc.Service.rpc_addr
                      | _ -> Config.endpoint_of_rpc "127.0.0.1:8732")
                in
                let depends_on =
                  match node_mode with `Instance inst -> Some inst | _ -> None
                in
                match
                  Cli_helpers.validate_port_addr
                    ~label:"Index RPC address"
                    ~addr:rpc_addr
                    ~default:"127.0.0.1:8733"
                    ()
                with
                | Error msg -> Cli_helpers.cmdliner_error msg
                | Ok rpc_addr -> (
                    let req : index_request =
                      {
                        instance;
                        base_dir;
                        rpc_addr = Rpc_addr.of_string rpc_addr;
                        watched_addresses;
                        db_name = Cli_helpers.normalize_opt_string db_name;
                        node_endpoint;
                        depends_on;
                        service_user;
                        app_bin_dir;
                        bin_source = Some bin_source;
                        logging_mode;
                        extra_args;
                        extra_env = [];
                        auto_enable = not no_enable;
                        preserve_data = false;
                      }
                    in
                    match Index.install req with
                    | Ok service ->
                        Format.printf "Installed %s\n" service.S.instance ;
                        `Ok ()
                    | Error (`Msg msg) -> Cli_helpers.cmdliner_error msg))))
  in
  let term =
    Term.(
      ret
        (const make $ instance $ base_dir_opt $ rpc_addr $ node_instance
       $ watched_addresses $ db_name $ extra_args $ service_user $ app_bin_dir
       $ octez_version $ bin_dir_alias $ auto_enable
       $ Cli_helpers.logging_mode_term))
  in
  let info =
    Cmd.info "install-index" ~doc:"Install an octez-index indexer service"
  in
  Cmd.v info term
