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

let install_baker_cmd =
  let instance =
    let doc = "Instance name for the baker systemd unit." in
    Arg.(value & opt (some string) None & info ["instance"] ~doc ~docv:"NAME")
  in
  let node_instance =
    let doc =
      "Existing octez-manager node instance to reuse for data-dir and network. \
       Use 'octez-manager list' to see available node instances. It can also \
       be a custom RPC endpoint for the baker to contact. Defaults to \
       http://127.0.0.1:8732"
    in
    Arg.(
      value & opt (some string) None & info ["node-instance"] ~doc ~docv:"NODE")
  in
  let base_dir =
    let doc =
      "Baker base directory for wallets (defaults to an instance-specific \
       path)."
    in
    Arg.(value & opt (some string) None & info ["base-dir"] ~doc ~docv:"DIR")
  in
  let delegates =
    let doc = "Delegate key hash or alias passed as --delegate." in
    Arg.(value & opt_all string [] & info ["delegate"] ~doc ~docv:"KEY")
  in
  let dal_endpoint =
    let doc =
      "DAL node endpoint (e.g., http://localhost:10732). Use 'none' to opt-out \
       with --without-dal flag. Defaults to 'none'."
    in
    Arg.(
      value
      & opt (some string) None
      & info ["dal-endpoint"] ~doc ~docv:"ENDPOINT")
  in
  let liquidity_baking_vote =
    let doc =
      "Liquidity baking toggle vote (on, off or pass). Defaults to 'pass'."
    in
    Arg.(
      value
      & opt (some string) None
      & info ["liquidity-baking-vote"] ~doc ~docv:"VOTE")
  in
  let extra_args =
    let doc = "Additional arguments appended to the baker command." in
    Arg.(value & opt_all string [] & info ["extra-arg"] ~doc ~docv:"ARG")
  in
  let default_user =
    if Common.is_root () then "octez"
    else fst (Common.current_user_group_names ())
  in
  let service_user =
    Arg.(
      value & opt string default_user
      & info ["service-user"] ~doc:"System user owning the service" ~docv:"USER")
  in
  let app_bin_dir =
    let doc = "Directory containing Octez binaries." in
    Arg.(value & opt (some string) None & info ["app-bin-dir"] ~doc ~docv:"DIR")
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
      value & flag
      & info ["no-enable"] ~doc:"Disable automatic systemctl enable --now")
  in
  let make instance_opt node_instance base_dir delegates dal_endpoint_opt
      liquidity_baking_vote_opt extra_args service_user app_bin_dir
      octez_version bin_dir_alias no_enable logging_mode =
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
      let* liquidity_baking_vote =
        match Cli_helpers.normalize_opt_string liquidity_baking_vote_opt with
        | Some vote -> Ok (Some vote)
        | None ->
            if Cli_helpers.is_interactive () then
              let completions = ["on"; "off"; "pass"] in
              let rec ask () =
                match
                  Cli_helpers.prompt_with_completion
                    "Liquidity baking vote"
                    completions
                with
                | Some v -> Ok (Some v)
                | None ->
                    prerr_endline "Please choose 'on', 'off', or 'pass'." ;
                    ask ()
              in
              ask ()
            else
              Error "Liquidity baking vote is required in non-interactive mode"
      in
      (* Prompt for dal_endpoint if not provided in interactive mode *)
      (* Track both DAL config and DAL node instance name *)
      let* dal_config, dal_node =
        match Cli_helpers.normalize_opt_string dal_endpoint_opt with
        | Some ep ->
            let normalized = String.lowercase_ascii (String.trim ep) in
            if normalized = "none" then Ok (Dal_disabled, None)
            else Ok (Dal_endpoint ep, None)
        | None ->
            if Cli_helpers.is_interactive () then
              (* Get list of available DAL node instances *)
              match Service_registry.list () with
              | Error (`Msg msg) ->
                  prerr_endline ("Warning: Could not load services: " ^ msg) ;
                  Ok (Dal_disabled, None)
              | Ok services ->
                  let dal_services =
                    List.filter
                      (fun (svc : Service.t) ->
                        let role_lower = String.lowercase_ascii svc.role in
                        String.equal role_lower "dal-node"
                        || String.equal role_lower "dal")
                      services
                  in
                  if dal_services = [] then
                    let choice =
                      Cli_helpers.prompt_with_completion_inline
                        "DAL Node endpoint"
                        ["none"]
                      |> Option.map (fun choice ->
                          String.lowercase_ascii @@ String.trim choice)
                    in
                    match choice with
                    | Some "" | Some "none" | None -> Ok (Dal_disabled, None)
                    | Some endpoint -> Ok (Dal_endpoint endpoint, None)
                  else
                    let rec loop () =
                      let instance_names =
                        List.map
                          (fun (svc : Service.t) -> svc.instance)
                          dal_services
                      in
                      let instance_map =
                        List.map
                          (fun (svc : Service.t) ->
                            (svc.instance, svc.rpc_addr))
                          dal_services
                      in
                      Format.printf
                        "Available DAL node instances: %s@."
                        (String.concat
                           ", "
                           (List.map
                              (fun (inst, addr) ->
                                Printf.sprintf "%s (%s)" inst addr)
                              instance_map)) ;
                      match
                        Cli_helpers.prompt_with_completion
                          "DAL node instance"
                          ("none" :: instance_names)
                      with
                      | Some "" | None -> loop ()
                      | Some "none" -> Ok (Dal_disabled, None)
                      | Some selected -> (
                          (* Check if input matches existing DAL instance name, otherwise treat as endpoint *)
                          match
                            List.find_opt
                              (fun (svc : Service.t) ->
                                String.equal svc.instance selected)
                              dal_services
                          with
                          | Some svc ->
                              Ok
                                ( Dal_endpoint
                                    (Config.endpoint_of_rpc
                                       svc.Service.rpc_addr),
                                  Some svc.instance )
                          | None ->
                              Ok
                                ( Dal_endpoint (Config.endpoint_of_rpc selected),
                                  None ))
                    in
                    loop ()
            else Ok (Dal_disabled, None)
      in
      let req : baker_request =
        {
          instance;
          node_mode;
          base_dir;
          delegates;
          dal_config;
          dal_node;
          liquidity_baking_vote;
          extra_args;
          service_user;
          app_bin_dir;
          logging_mode;
          auto_enable = not no_enable;
          preserve_data = false;
        }
      in
      (* Baker.install_baker returns an Rresult-style error; convert it to a string-error Result *)
      match Baker.install_baker req with
      | Ok service ->
          Format.printf "Installed %s (%s)\n" service.S.instance service.network ;
          Ok ()
      | Error (`Msg s) -> Error s
    in
    match res with
    | Ok () -> `Ok ()
    | Error msg -> Cli_helpers.cmdliner_error msg
  in
  let term =
    Term.(
      ret
        (const make $ instance $ node_instance $ base_dir $ delegates
       $ dal_endpoint $ liquidity_baking_vote $ extra_args $ service_user
       $ app_bin_dir $ octez_version $ bin_dir_alias $ auto_enable
       $ Cli_helpers.logging_mode_term))
  in
  let info = Cmd.info "install-baker" ~doc:"Install an octez-baker service" in
  Cmd.v info term
