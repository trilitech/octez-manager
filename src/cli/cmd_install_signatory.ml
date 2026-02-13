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

let install_signatory_cmd =
  let instance =
    let doc = "Signatory instance name" in
    Arg.(value & opt (some string) None & info ["instance"] ~doc ~docv:"NAME")
  in
  let backend =
    let doc =
      "Signatory backend type. Only 'file' is currently supported. File \
       backend stores keys in the local filesystem."
    in
    Arg.(value & opt (some string) None & info ["backend"] ~doc ~docv:"BACKEND")
  in
  let keys_dir =
    let doc =
      "Directory path for storing keys (File backend only). If not specified, \
       defaults to /var/lib/octez/signatory/<instance>/keys"
    in
    Arg.(value & opt (some string) None & info ["keys-dir"] ~doc ~docv:"DIR")
  in
  let authorized_keys =
    let doc =
      "Comma-separated list of authorized Tezos public key hashes (tz1, tz2, \
       tz3, or tz4)"
    in
    Arg.(
      value
      & opt (some string) None
      & info ["authorized-keys"] ~doc ~docv:"KEYS")
  in
  let address =
    let doc = "HTTP server address (default: 127.0.0.1:6732)" in
    Arg.(
      value & opt string "127.0.0.1:6732" & info ["address"] ~doc ~docv:"ADDR")
  in
  let metrics_address =
    let doc = "Metrics endpoint address (default: 127.0.0.1:9583)" in
    Arg.(
      value
      & opt string "127.0.0.1:9583"
      & info ["metrics-address"] ~doc ~docv:"ADDR")
  in
  let watermark =
    let doc =
      "Watermark storage backend: 'memory' (default) or 'file'. Memory stores \
       in RAM, file persists to disk for multi-instance setups."
    in
    Arg.(value & opt string "memory" & info ["watermark"] ~doc ~docv:"BACKEND")
  in
  let default_user =
    if Paths.is_root () then "tezos"
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
  let version =
    let doc =
      "Use a managed Signatory version (e.g., '4.0' or 'latest'). Overrides \
       --app-bin-dir. Download versions with: octez-manager binaries download \
       VERSION"
    in
    Arg.(
      value
      & opt (some string) None
      & info ["signatory-version"] ~doc ~docv:"VERSION")
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
  let make instance_opt backend_opt keys_dir_opt authorized_keys_opt address
      metrics_address watermark_str service_user app_bin_dir version
      bin_dir_alias no_enable logging_mode =
    let res =
      let ( let* ) = Result.bind in
      (* Resolve app_bin_dir *)
      let* app_bin_dir, bin_source =
        Cli_helpers.resolve_signatory_bin_dir
          ?signatory_version:version
          ?bin_dir_alias
          app_bin_dir
      in
      (* Prompt for instance name if not provided *)
      let* instance =
        match Cli_helpers.normalize_opt_string instance_opt with
        | Some inst -> Ok inst
        | None ->
            if Cli_helpers.is_interactive () then
              Ok (Cli_helpers.prompt_required_string "Instance name")
            else Error "Instance name is required in non-interactive mode"
      in
      (* Prompt for backend if not provided *)
      let* backend_str =
        match Cli_helpers.normalize_opt_string backend_opt with
        | Some b -> Ok b
        | None ->
            if Cli_helpers.is_interactive () then
              Ok
                (Cli_helpers.prompt_required_string
                   "Backend type (only 'file' supported)")
            else Error "Backend is required in non-interactive mode"
      in
      (* Validate backend (only file is supported for now) *)
      let* () =
        match String.lowercase_ascii backend_str with
        | "file" -> Ok ()
        | _ ->
            Error
              (Printf.sprintf
                 "Unsupported backend '%s'. Only 'file' is currently supported."
                 backend_str)
      in
      (* Build keys directory path *)
      let keys_path =
        match Cli_helpers.normalize_opt_string keys_dir_opt with
        | Some dir -> dir
        | None -> Printf.sprintf "/var/lib/octez/signatory/%s/keys" instance
      in
      let backend = File keys_path in
      (* Prompt for authorized keys if not provided *)
      let* authorized_keys_str =
        match Cli_helpers.normalize_opt_string authorized_keys_opt with
        | Some keys -> Ok keys
        | None ->
            if Cli_helpers.is_interactive () then
              Ok
                (Cli_helpers.prompt_required_string
                   "Authorized keys (comma-separated tz1/tz2/tz3/tz4 addresses)")
            else Error "Authorized keys are required in non-interactive mode"
      in
      (* Parse authorized keys list *)
      let authorized_keys =
        String.split_on_char ',' authorized_keys_str
        |> List.map String.trim
        |> List.filter (fun s -> s <> "")
      in
      let* () =
        if List.length authorized_keys = 0 then
          Error "At least one authorized key is required"
        else Ok ()
      in
      (* Parse watermark backend *)
      let* watermark =
        match String.lowercase_ascii watermark_str with
        | "memory" -> Ok Memory
        | "file" ->
            let watermark_file =
              Printf.sprintf
                "/var/lib/octez/signatory/%s/watermark.json"
                instance
            in
            Ok (File_watermark watermark_file)
        | _ ->
            Error
              (Printf.sprintf
                 "Unsupported watermark backend '%s'. Supported: memory, file."
                 watermark_str)
      in
      (* Build signatory request *)
      let req : signatory_request =
        {
          instance;
          backend;
          authorized_keys;
          address;
          metrics_address;
          watermark;
          service_user;
          app_bin_dir;
          bin_source = Some bin_source;
          logging_mode;
          auto_enable = not no_enable;
          preserve_data = false;
        }
      in
      (* Install signatory *)
      match Signatory.install_signatory req with
      | Ok svc -> Ok svc
      | Error (`Msg msg) -> Error msg
    in
    match res with
    | Ok service ->
        Format.printf "Installed signatory %s\n" service.S.instance ;
        `Ok ()
    | Error msg -> Cli_helpers.cmdliner_error msg
  in
  let term =
    Term.(
      ret
        (const make $ instance $ backend $ keys_dir $ authorized_keys $ address
       $ metrics_address $ watermark $ service_user $ app_bin_dir $ version
       $ bin_dir_alias $ auto_enable $ Cli_helpers.logging_mode_term))
  in
  let info =
    Cmd.info "install-signatory" ~doc:"Install an octez-signatory service"
  in
  Cmd.v info term
