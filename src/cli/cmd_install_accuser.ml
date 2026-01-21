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

let install_accuser_cmd =
  let instance =
    let doc = "Accuser instance name" in
    Arg.(value & opt (some string) None & info ["instance"] ~doc ~docv:"NAME")
  in
  let node_instance =
    let doc =
      "Existing octez-manager node instance to reuse for endpoint; can also be \
       a custom RPC endpoint"
    in
    Arg.(
      value & opt (some string) None & info ["node-instance"] ~doc ~docv:"NODE")
  in
  let base_dir =
    Arg.(
      value
      & opt (some string) None
      & info ["base-dir"] ~doc:"Accuser base directory" ~docv:"DIR")
  in
  let extra_args =
    Arg.(
      value & opt_all string []
      & info
          ["extra-arg"]
          ~doc:"Additional arguments appended to the accuser command."
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
      "Use a linked directory by alias. Overrides --app-bin-dir. Create \
       aliases with: octez-manager binaries link"
    in
    Arg.(
      value & opt (some string) None & info ["bin-dir-alias"] ~doc ~docv:"ALIAS")
  in
  let auto_enable =
    Arg.(
      value & flag & info ["no-enable"] ~doc:"Disable automatic enable --now")
  in
  let make instance_opt node_instance base_dir extra_args service_user
      app_bin_dir octez_version bin_dir_alias no_enable logging_mode =
    let res =
      let ( let* ) = Result.bind in
      let* app_bin_dir =
        Cli_helpers.resolve_app_bin_dir
          ?octez_version
          ?bin_dir_alias
          app_bin_dir
      in
      let* instance =
        match Cli_helpers.normalize_opt_string instance_opt with
        | Some inst -> Ok inst
        | None ->
            if Cli_helpers.is_interactive () then
              Ok (Cli_helpers.prompt_required_string "Instance name")
            else Error "Instance name is required in non-interactive mode"
      in
      let* choice =
        Result.map_error (fun (`Msg s) -> s)
        @@ Cli_helpers.resolve_node_instance_or_endpoint ~node_instance
      in
      let node_mode =
        match choice with
        | `Instance ins -> Local_instance ins
        | `Endpoint endpoint -> Remote_endpoint endpoint
      in
      let req : accuser_request =
        {
          instance;
          app_bin_dir;
          node_mode;
          base_dir;
          extra_args;
          service_user;
          logging_mode;
          auto_enable = not no_enable;
          preserve_data = false;
        }
      in
      match Accuser.install_accuser req with
      | Ok svc -> Ok svc
      | Error (`Msg msg) -> Error msg
    in
    match res with
    | Ok service ->
        Format.printf "Installed  %s (%s)\n" service.S.instance service.network ;
        `Ok ()
    | Error msg -> Cli_helpers.cmdliner_error msg
  in
  let term =
    Term.(
      ret
        (const make $ instance $ node_instance $ base_dir $ extra_args
       $ service_user $ app_bin_dir $ octez_version $ bin_dir_alias
       $ auto_enable $ Cli_helpers.logging_mode_term))
  in
  let info =
    Cmd.info "install-accuser" ~doc:"Install an octez-accuser service"
  in
  Cmd.v info term
