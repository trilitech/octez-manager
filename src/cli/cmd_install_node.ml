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

let install_node_cmd =
  let instance =
    let doc = "Instance name used for node.env and systemd units." in
    Arg.(value & opt (some string) None & info ["instance"] ~doc ~docv:"NAME")
  in
  let network =
    let doc = "Chain network (default: shadownet)." in
    Arg.(value & opt (some string) None & info ["network"] ~doc ~docv:"NET")
  in
  let data_dir =
    let doc = "Custom data directory (defaults to /var/lib/octez/<inst>)." in
    Arg.(value & opt (some string) None & info ["data-dir"] ~doc ~docv:"DIR")
  in
  let rpc_addr =
    Arg.(
      value
      & opt string "127.0.0.1:8732"
      & info ["rpc-addr"] ~doc:"RPC address" ~docv:"ADDR")
  in
  let net_addr =
    Arg.(
      value & opt string "0.0.0.0:9732"
      & info ["net-addr"] ~doc:"P2P address" ~docv:"ADDR")
  in
  let service_user =
    let default_user =
      if Common.is_root () then "octez"
      else fst (Common.current_user_group_names ())
    in
    let doc = "System user owning the service." in
    Arg.(
      value & opt string default_user & info ["service-user"] ~doc ~docv:"USER")
  in
  let app_bin_dir =
    let doc =
      {|Directory containing Octez binaries (defaults to the directory holding octez-node found in \$PATH).|}
    in
    Arg.(value & opt (some string) None & info ["app-bin-dir"] ~doc ~docv:"DIR")
  in
  let extra_args =
    let doc = "Additional arguments appended to the node command." in
    Arg.(value & opt_all string [] & info ["extra-arg"] ~doc ~docv:"ARG")
  in
  let snapshot_flag =
    let doc =
      "Bootstrap by importing a snapshot before enabling the node service."
    in
    Arg.(value & flag & info ["snapshot"] ~doc)
  in
  let snapshot_uri =
    let doc =
      "Snapshot URI (path, file://, or http(s)) to import when --snapshot is \
       set."
    in
    Arg.(
      value & opt (some string) None & info ["snapshot-uri"] ~doc ~docv:"URI")
  in
  let snapshot_no_check =
    let doc =
      "Pass --no-check to octez-node snapshot import during bootstrap."
    in
    Arg.(value & flag & info ["snapshot-no-check"] ~doc)
  in
  let auto_enable =
    let doc = "Disable automatic systemctl enable --now." in
    Arg.(value & flag & info ["no-enable"] ~doc)
  in
  let preserve_data =
    let doc =
      "Preserve existing data in data-dir instead of clearing it. When set, no \
       snapshot will be imported even if --snapshot is specified."
    in
    Arg.(value & flag & info ["preserve-data"] ~doc)
  in
  let tmp_dir =
    let doc =
      "Directory for temporary snapshot download. Use when /tmp has \
       insufficient space for large snapshots (e.g., mainnet full)."
    in
    Arg.(value & opt (some string) None & info ["tmp-dir"] ~doc ~docv:"DIR")
  in
  let keep_snapshot =
    let doc =
      "Keep the downloaded snapshot file after import instead of deleting it."
    in
    Arg.(value & flag & info ["keep-snapshot"] ~doc)
  in
  let make instance_opt network_opt history_mode_opt data_dir rpc_addr net_addr
      service_user app_bin_dir extra_args snapshot_flag snapshot_uri
      snapshot_no_check no_enable preserve_data tmp_dir keep_snapshot
      logging_mode =
    let res =
      let ( let* ) = Result.bind in
      let* app_bin_dir = Cli_helpers.resolve_app_bin_dir app_bin_dir in
      (* When preserve_data is set, require data_dir to be specified *)
      let* data_dir =
        match (preserve_data, data_dir) with
        | true, None ->
            if Cli_helpers.is_interactive () then
              Ok
                (Some
                   (Cli_helpers.prompt_required_string
                      "Data directory to preserve"))
            else
              Error
                "--data-dir is required when using --preserve-data in \
                 non-interactive mode"
        | _, dir -> Ok dir
      in
      let* data_dir_config =
        match data_dir with
        | None -> Ok None
        | Some data_dir ->
            let* r = Config.resolve_from_data_dir data_dir in
            Ok (Some r)
      in
      (* When preserving data, config.json must exist in the data directory *)
      let* () =
        match (preserve_data, data_dir_config) with
        | true, Some (`Path dir) ->
            Error
              (Printf.sprintf
                 "Cannot preserve data: no config.json found in '%s'"
                 dir)
        | _ -> Ok ()
      in
      let instance () =
        match Cli_helpers.normalize_opt_string instance_opt with
        | Some inst -> Ok inst
        | None ->
            if Cli_helpers.is_interactive () then
              Ok (Cli_helpers.prompt_required_string "Instance name")
            else Error "Instance name is required in non-interactive mode"
      in
      match data_dir_config with
      | Some
          (`Data_dir
             {
               network;
               history_mode;
               rpc_addr = config_rpc_addr;
               net_addr = config_net_addr;
             }) ->
          let* instance = instance () in
          let* () =
            match (history_mode, history_mode_opt) with
            | history_mode, Some history_mode'
              when history_mode <> history_mode' ->
                Error
                  (Format.asprintf
                     "History mode found in the configuration incompatible \
                      with the arguments: %a <> %a"
                     History_mode.pp
                     history_mode
                     History_mode.pp
                     history_mode')
            | _ -> Ok ()
          in
          let* () =
            match (network, network_opt) with
            | network, Some network' when network <> network' ->
                Error
                  (Format.asprintf
                     "Network found in the configuration incompatible with the \
                      arguments: %s <> %s"
                     network
                     network')
            | _ -> Ok ()
          in
          let rpc_addr =
            if rpc_addr <> "127.0.0.1:8732" then rpc_addr else config_rpc_addr
          in
          let net_addr =
            if net_addr <> "0.0.0.0:9732" then net_addr else config_net_addr
          in
          (* Validate ports - exclude self in edit/preserve-data mode *)
          let* rpc_addr =
            Cli_helpers.validate_port_addr
              ~label:"RPC address"
              ~addr:rpc_addr
              ~default:"127.0.0.1:8732"
              ~exclude_instance:instance
              ()
          in
          let* net_addr =
            Cli_helpers.validate_port_addr
              ~label:"P2P address"
              ~addr:net_addr
              ~default:"0.0.0.0:9732"
              ~exclude_instance:instance
              ()
          in
          let* () =
            if snapshot_flag || Option.is_some snapshot_uri then
              Error "Snapshot cannot be imported, the data-dir already exist"
            else Ok ()
          in
          let req : node_request =
            {
              instance;
              network;
              history_mode;
              data_dir;
              rpc_addr;
              net_addr;
              service_user;
              app_bin_dir;
              extra_args;
              auto_enable = not no_enable;
              logging_mode;
              bootstrap = Genesis;
              preserve_data;
              snapshot_no_check;
              tmp_dir;
              keep_snapshot;
            }
          in
          Result.map_error (fun (`Msg s) -> s) @@ Node.install_node req
      | Some (`Path _) | None ->
          let* instance = instance () in
          let* network =
            match Cli_helpers.normalize_opt_string network_opt with
            | Some net -> Ok net
            | None ->
                if Cli_helpers.is_interactive () then
                  match Teztnets.list_networks () with
                  | Ok infos ->
                      let aliases =
                        List.map (fun Teztnets.{alias; _} -> alias) infos
                      in
                      let rec loop () =
                        match
                          Cli_helpers.prompt_with_completion "Network" aliases
                        with
                        | Some sel -> sel
                        | None ->
                            prerr_endline "Please enter a network." ;
                            loop ()
                      in
                      Ok (loop ())
                  | Error (`Msg err) -> Error err
                else Ok "shadownet"
          in
          let history_mode =
            match history_mode_opt with
            | Some hm -> hm
            | None -> Cli_helpers.prompt_history_mode History_mode.default
          in
          let snapshot_requested_initial =
            snapshot_flag || Option.is_some snapshot_uri
          in
          let snapshot_requested =
            if snapshot_requested_initial then true
            else if Cli_helpers.is_interactive () then
              Cli_helpers.prompt_yes_no
                "Download and import a tzinit snapshot before starting?"
                ~default:true
            else false
          in
          let snapshot_uri = Cli_helpers.normalize_opt_string snapshot_uri in
          let snapshot_requested =
            snapshot_requested || Option.is_some snapshot_uri
          in
          let bootstrap =
            if preserve_data then Genesis
            else if snapshot_requested then Snapshot {src = snapshot_uri}
            else Genesis
          in
          (* Resolve actual data_dir (use default if not specified) *)
          let actual_data_dir =
            match data_dir with
            | Some dir when String.trim dir <> "" -> dir
            | _ -> Common.default_data_dir instance
          in
          (* Check snapshot size vs tmp space and data dir space *)
          let* tmp_dir =
            match bootstrap with
            | Snapshot _ -> (
                (* Get the snapshot URL - either direct or resolved from tzinit *)
                let snapshot_url =
                  match snapshot_uri with
                  | Some uri
                    when String.length uri > 4
                         && String.sub (String.lowercase_ascii uri) 0 4 = "http"
                    ->
                      Some uri
                  | Some _ -> None (* Local file, no space check needed *)
                  | None -> (
                      (* Auto-select: resolve from tzinit to get URL *)
                      match
                        Snapshot.resolve_snapshot_download
                          ~network
                          ~history_mode
                      with
                      | Ok res -> Some res.download_url
                      | Error _ -> None)
                in
                match snapshot_url with
                | Some url ->
                    (* Check data directory has enough space for imported data *)
                    let* () =
                      Cli_helpers.check_data_dir_space
                        ~snapshot_url:url
                        ~data_dir:actual_data_dir
                    in
                    Cli_helpers.resolve_tmp_dir_for_snapshot
                      ~snapshot_url:url
                      ~tmp_dir
                | None -> Ok tmp_dir)
            | Genesis -> Ok tmp_dir
          in
          (* Validate ports for new install *)
          let* rpc_addr =
            Cli_helpers.validate_port_addr
              ~label:"RPC address"
              ~addr:rpc_addr
              ~default:"127.0.0.1:8732"
              ()
          in
          let* net_addr =
            Cli_helpers.validate_port_addr
              ~label:"P2P address"
              ~addr:net_addr
              ~default:"0.0.0.0:9732"
              ()
          in
          let req : node_request =
            {
              instance;
              network;
              history_mode;
              data_dir;
              rpc_addr;
              net_addr;
              service_user;
              app_bin_dir;
              extra_args;
              auto_enable = not no_enable;
              logging_mode;
              bootstrap;
              preserve_data;
              snapshot_no_check;
              tmp_dir;
              keep_snapshot;
            }
          in
          Result.map_error (fun (`Msg s) -> s) @@ Node.install_node req
    in
    match res with
    | Ok service ->
        Format.printf "Installed %s (%s)\n" service.S.instance service.network ;
        `Ok ()
    | Error msg -> Cli_helpers.cmdliner_error msg
  in
  let term =
    Term.(
      ret
        (const make $ instance $ network $ Cli_helpers.history_mode_opt_term
       $ data_dir $ rpc_addr $ net_addr $ service_user $ app_bin_dir
       $ extra_args $ snapshot_flag $ snapshot_uri $ snapshot_no_check
       $ auto_enable $ preserve_data $ tmp_dir $ keep_snapshot
       $ Cli_helpers.logging_mode_term))
  in
  let info =
    Cmd.info "install-node" ~doc:"Install an octez-node systemd instance"
  in
  Cmd.v info term
