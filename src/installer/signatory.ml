(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Rresult
open Installer_types
include Helpers
include Config

let ( let* ) = Result.bind

(** Validate that an address has the format "host:port" *)
let validate_address address =
  let trimmed = String.trim address in
  if trimmed = "" then R.error_msg "Address cannot be empty"
  else
    match String.split_on_char ':' trimmed with
    | [_host; port_str] -> (
        match int_of_string_opt port_str with
        | Some port when port > 0 && port <= 65535 -> Ok trimmed
        | _ ->
            R.error_msgf
              "Invalid port in address '%s'. Port must be 1-65535."
              trimmed)
    | _ ->
        R.error_msgf
          "Invalid address format '%s'. Expected 'host:port' (e.g., \
           '127.0.0.1:6732')."
          trimmed

(** Validate that a Tezos public key hash starts with tz1/tz2/tz3/tz4 *)
let validate_tezos_key key =
  let trimmed = String.trim key in
  if String.length trimmed < 36 then
    R.error_msgf "Invalid Tezos key '%s'. Key too short." trimmed
  else if
    String.starts_with ~prefix:"tz1" trimmed
    || String.starts_with ~prefix:"tz2" trimmed
    || String.starts_with ~prefix:"tz3" trimmed
    || String.starts_with ~prefix:"tz4" trimmed
  then Ok trimmed
  else
    R.error_msgf
      "Invalid Tezos key '%s'. Must start with tz1, tz2, tz3, or tz4."
      trimmed

(** Validate authorized keys list *)
let validate_authorized_keys keys =
  if keys = [] then R.error_msg "Authorized keys list cannot be empty"
  else
    (* Validate each key *)
    List.fold_left
      (fun acc (key : authorized_key) ->
        let* acc_keys = acc in
        let* validated_pkh = validate_tezos_key key.pkh in
        Ok ({key with pkh = validated_pkh} :: acc_keys))
      (Ok [])
      keys
    |> Result.map List.rev

(** Validate File backend has a valid path *)
let validate_backend = function
  | File path ->
      let trimmed = String.trim path in
      if trimmed = "" then R.error_msg "File backend path cannot be empty"
      else Ok ()
  | _ ->
      R.error_msg
        "Only File backend is currently supported. HSM and cloud backends will \
         be added in future releases."

(** Generate watermark section of YAML config *)
let generate_watermark_section data_dir = function
  | Memory -> "watermark:\n  driver: mem\n"
  | File_watermark path ->
      let actual_path =
        if String.trim path = "" then Filename.concat data_dir "watermark.json"
        else path
      in
      Printf.sprintf
        "watermark:\n  driver: file\n  config:\n    path: %s\n"
        actual_path
  | AWS_DynamoDB _ | GCP_Firestore _ ->
      (* These are not yet implemented but are in the type system *)
      "watermark:\n  driver: mem\n"

(** Generate allow section for a key based on its permissions *)
let generate_allow_section (permissions : signatory_operation list) =
  if List.is_empty permissions then "    # No operations allowed\n"
  else
    let ops =
      List.map
        (fun op ->
          match op with
          | Block -> "      block:"
          | Attestation -> "      attestation:"
          | Preattestation -> "      preattestation:"
          | Attestation_with_dal -> "      attestation_with_dal:"
          | Generic -> "      generic:\n        - transaction")
        permissions
      |> String.concat "\n"
    in
    Printf.sprintf "    allow:\n%s\n" ops

(** Generate Signatory YAML configuration *)
let generate_signatory_yaml ~address ~metrics_address ~authorized_keys
    ~keys_path ~data_dir ~watermark =
  (* Generate tezos section with each key and its custom permissions *)
  let tezos_keys_section =
    String.concat
      "\n"
      (List.map
         (fun (key : authorized_key) ->
           let allow_section = generate_allow_section key.permissions in
           Printf.sprintf
             "  %s:\n    log_payloads: true\n%s"
             key.pkh
             allow_section)
         authorized_keys)
  in
  let watermark_section = generate_watermark_section data_dir watermark in
  (* The keys_path should point to a JSON file containing secret keys *)
  let secrets_file = Filename.concat keys_path "secret.json" in
  let utility_line =
    if metrics_address = "" then ""
    else Printf.sprintf "  utility_address: %s\n" metrics_address
  in
  Printf.sprintf
    {|server:
  address: %s
%s
vaults:
  local_secret:
    driver: file
    config:
      file: %s

tezos:
%s

%s|}
    address
    utility_line
    secrets_file
    tezos_keys_section
    watermark_section

(** Compute data directory for signatory *)
let signatory_data_dir instance =
  let base =
    (* Respect XDG_DATA_HOME override (used by tests), otherwise use standard paths *)
    match Sys.getenv_opt "XDG_DATA_HOME" with
    | Some xdg when xdg <> "" -> Filename.concat xdg "octez"
    | _ ->
        if Paths.is_root () then "/var/lib/octez"
        else Filename.concat (Paths.xdg_data_home ()) "octez"
  in
  Filename.concat (Filename.concat base "signatory") instance

(** Compute keys directory *)
let keys_dir data_dir = Filename.concat data_dir "keys"

(** Install signatory remote signer service *)
let install_signatory ?(quiet = false) (request : signatory_request) =
  (* Validation *)
  let* () =
    validate_instance_name
      ~allow_existing:request.preserve_data
      ~instance:request.instance
      ()
  in
  let* validated_address = validate_address request.address in
  let* validated_metrics =
    if request.metrics_address = "" then Ok ""
    else validate_address request.metrics_address
  in
  let* validated_keys = validate_authorized_keys request.authorized_keys in
  let* () = validate_backend request.backend in

  (* Compute paths *)
  let data_dir = signatory_data_dir request.instance in
  let keys_path =
    match request.backend with
    | File path when String.trim path <> "" -> path
    | File _ -> keys_dir data_dir
    | _ -> keys_dir data_dir
  in

  let logging_mode =
    prepare_logging
      ~instance:request.instance
      ~role:"signatory"
      ~logging_mode:request.logging_mode
  in

  (* Ensure service account *)
  let* () =
    System_user.ensure_service_account ~quiet ~name:request.service_user ()
  in
  let* () =
    if Paths.is_root () then
      System_user.ensure_system_directories
        ~user:request.service_user
        ~group:request.service_user
        ()
    else Ok ()
  in
  let* () =
    ensure_logging_destination ~service_user:request.service_user logging_mode
  in
  let* () = System_user.validate_user_for_service ~user:request.service_user in

  let owner, group =
    if Paths.is_root () then (request.service_user, request.service_user)
    else Paths.current_user_group_names ()
  in

  (* Create directories *)
  let directories = [data_dir] in
  let* () = ensure_directories ~owner ~group directories in

  (* Create keys directory with restricted permissions (0o700) *)
  let* () = File_ops.ensure_dir_path ~owner ~group ~mode:0o700 keys_path in

  (* Create template secret.json file if it doesn't exist *)
  let secrets_file = Filename.concat keys_path "secret.json" in
  let* () =
    if Sys.file_exists secrets_file then Ok ()
    else
      (* Write empty JSON array - always valid JSON *)
      let template_content = "[]" in
      File_ops.write_file
        ~mode:0o600
        ~owner
        ~group
        secrets_file
        template_content
  in

  (* Always create/update the README file with instructions *)
  let readme_file = Filename.concat keys_path "secret.json.README" in
  let readme_content =
    {|SECRET.JSON FORMAT
==================

This file stores your signing keys in JSON format.

Format:
  An array of key objects, where each object has:
  - "name": The public key hash (tz1..., tz2..., tz3..., or tz4...)
  - "value": The secret key (e.g., "unencrypted:edsk...")

Example with a single key:
[
  {
    "name": "tz1VzDhuGRB5yUHR9bLkib2kbntQAAFSr8zK",
    "value": "unencrypted:edsk3iSX5sJ375y4yu1KkyToz1mXjJqHyJR6ewtVweSc9j9cJY8bSw"
  }
]

Example with multiple keys:
[
  {
    "name": "tz1abc...",
    "value": "unencrypted:edsk..."
  },
  {
    "name": "tz3def...",
    "value": "unencrypted:edsk..."
  }
]

Exporting keys from octez-client:
  octez-client show address <alias> -S

Security:
  - This file (secret.json) has 0600 permissions (owner read/write only)
  - Never share your secret keys
  - Back up secret.json securely
|}
  in
  let* () =
    File_ops.write_file ~mode:0o644 ~owner ~group readme_file readme_content
  in

  let* () = ensure_logging_base_directory ~owner ~group logging_mode in
  let* () = ensure_runtime_log_directory ~owner ~group logging_mode in

  (* Generate YAML configuration *)
  let yaml_content =
    generate_signatory_yaml
      ~address:validated_address
      ~metrics_address:validated_metrics
      ~authorized_keys:validated_keys
      ~keys_path
      ~data_dir
      ~watermark:request.watermark
  in

  (* Write config file in the signatory data directory *)
  let config_path = Filename.concat data_dir "signatory.yaml" in
  let* () =
    File_ops.write_file ~mode:0o644 ~owner ~group config_path yaml_content
  in

  (* Write environment file *)
  let backend_kind_str =
    match request.backend with
    | File _ -> "file"
    | YubiHSM _ -> "yubihsm"
    | Azure_KMS _ -> "azure-kms"
    | AWS_KMS _ -> "aws-kms"
    | GCP_KMS _ -> "gcp-kms"
    | Vault _ -> "vault"
  in
  let env_pairs =
    [
      ("SIGNATORY_CONFIG_PATH", config_path);
      ("SIGNATORY_BACKEND_KIND", backend_kind_str);
      ("SIGNATORY_KEYS_DIR", keys_path);
      ("APP_BIN_DIR", request.app_bin_dir);
    ]
  in
  let* () =
    Node_env.write_pairs ~with_comments:true ~inst:request.instance env_pairs
  in

  (* Install systemd unit *)
  let* () =
    Systemd.install_unit
      ~quiet
      ~role:"signatory"
      ~app_bin_dir:request.app_bin_dir
      ~user:request.service_user
      ()
  in

  (* Write systemd dropin *)
  let* () =
    Systemd.write_dropin
      ~role:"signatory"
      ~inst:request.instance
      ~data_dir
      ~logging_mode
      ~extra_paths:[keys_path]
      ~app_bin_dir:request.app_bin_dir
      ()
  in

  (* Re-own runtime paths if not preserving data *)
  let* () =
    if request.preserve_data then Ok ()
    else
      reown_runtime_paths
        ~owner
        ~group
        ~paths:[data_dir; keys_path]
        ~logging_mode
  in

  (* Parse RPC address for service registry *)
  let rpc_addr = Rpc_addr.of_string validated_address in

  (* Create service record *)
  let service =
    Service.make
      ~instance:request.instance
      ~role:"signatory"
      ~network:"" (* Signatory is network-agnostic *)
      ~history_mode:History_mode.default (* N/A for signatory *)
      ~data_dir
      ~rpc_addr
      ~net_addr:"" (* N/A for signatory *)
      ~service_user:request.service_user
      ~app_bin_dir:request.app_bin_dir
      ?bin_source:request.bin_source
      ~logging_mode
      ~extra_args:[] (* Signatory args are in config file *)
      ~depends_on:None
      ~dependents:[]
      ()
  in

  (* Register service *)
  let* () = Service_registry.write service in

  (* Enable and start if requested *)
  let* () =
    if request.auto_enable then
      Systemd.enable
        ~quiet
        ~role:"signatory"
        ~instance:request.instance
        ~start_now:true
        ()
    else Ok ()
  in

  Ok service

(** Get the path to signatory.yaml config file for an instance *)
let signatory_config_path instance =
  let data_dir = signatory_data_dir instance in
  Filename.concat data_dir "signatory.yaml"

(** Read authorized keys from signatory.yaml.
    
    Parses the YAML to extract keys from the "tezos:" section.
    Returns a list of public key hashes (tz1/tz2/tz3/tz4 addresses). *)
let read_authorized_keys instance =
  let config_path = signatory_config_path instance in
  if not (Sys.file_exists config_path) then
    R.error_msgf
      "Signatory config not found: %s (instance '%s' may not exist)"
      config_path
      instance
  else
    try
      let ic = open_in config_path in
      let content =
        Fun.protect
          ~finally:(fun () -> close_in_noerr ic)
          (fun () ->
            let len = in_channel_length ic in
            really_input_string ic len)
      in
      (* Simple line-based parsing of YAML to extract keys from tezos: section *)
      let lines = String.split_on_char '\n' content in
      let rec extract_keys in_tezos_section acc = function
        | [] -> List.rev acc
        | line :: rest ->
            let trimmed = String.trim line in
            (* Detect when we enter the tezos: section *)
            if String.equal trimmed "tezos:" then extract_keys true acc rest
              (* If we're in tezos section and hit another top-level key, we're done *)
            else if
              in_tezos_section
              && String.length trimmed > 0
              && (not (String.starts_with ~prefix:" " line))
              && String.ends_with ~suffix:":" trimmed
            then
              (* Exited tezos section *)
              List.rev acc
              (* Extract keys: lines in tezos section that look like tz[1-4]... keys *)
            else if
              in_tezos_section
              && String.starts_with ~prefix:"  " line
              (* Must have 2+ spaces indent *)
              && (String.starts_with ~prefix:"tz1" trimmed
                 || String.starts_with ~prefix:"tz2" trimmed
                 || String.starts_with ~prefix:"tz3" trimmed
                 || String.starts_with ~prefix:"tz4" trimmed)
              && String.ends_with ~suffix:":" trimmed
            then
              let key =
                String.sub trimmed 0 (String.length trimmed - 1) |> String.trim
              in
              extract_keys in_tezos_section (key :: acc) rest
            else extract_keys in_tezos_section acc rest
      in
      Ok (extract_keys false [] lines)
    with
    | Sys_error msg -> R.error_msg msg
    | e ->
        R.error_msgf
          "Failed to parse signatory config: %s"
          (Printexc.to_string e)

(** Signatory configuration parsed from signatory.yaml *)
type signatory_config = {
  address : string option;
  metrics_address : string option;
  backend : string option;
  authorized_keys : string list;
}

(** Read full configuration from a signatory instance's YAML file *)
let read_config instance =
  let config_path = signatory_config_path instance in
  if not (Sys.file_exists config_path) then
    R.error_msgf
      "Signatory config not found: %s (instance '%s' may not exist)"
      config_path
      instance
  else
    try
      let ic = open_in config_path in
      let content =
        Fun.protect
          ~finally:(fun () -> close_in_noerr ic)
          (fun () ->
            let len = in_channel_length ic in
            really_input_string ic len)
      in
      (* Simple line-based YAML parsing *)
      let lines = String.split_on_char '\n' content in
      let rec parse_lines in_server in_vaults in_tezos current_indent address
          metrics backend keys = function
        | [] ->
            Ok
              {
                address;
                metrics_address = metrics;
                backend;
                authorized_keys = keys;
              }
        | line :: rest ->
            let trimmed = String.trim line in
            let indent = String.length line - String.length trimmed in
            (* Parse server section *)
            if String.equal trimmed "server:" then
              parse_lines true false false 0 address metrics backend keys rest
            else if in_server && String.starts_with ~prefix:"address:" trimmed
            then
              let addr =
                String.sub trimmed 8 (String.length trimmed - 8) |> String.trim
              in
              parse_lines
                in_server
                in_vaults
                in_tezos
                current_indent
                (Some addr)
                metrics
                backend
                keys
                rest
            else if
              in_server && String.starts_with ~prefix:"utility_address:" trimmed
            then
              let util =
                String.sub trimmed 17 (String.length trimmed - 17)
                |> String.trim
              in
              parse_lines
                in_server
                in_vaults
                in_tezos
                current_indent
                address
                (Some util)
                backend
                keys
                rest (* Exit server section when we hit a top-level key *)
            else if
              in_server && indent = 0
              && String.length trimmed > 0
              && String.ends_with ~suffix:":" trimmed
              && not (String.equal trimmed "server:")
            then
              (* Check if entering vaults section *)
              if String.equal trimmed "vaults:" then
                parse_lines false true false 0 address metrics backend keys rest
              else if String.equal trimmed "tezos:" then
                parse_lines false false true 2 address metrics backend keys rest
              else
                parse_lines
                  false
                  false
                  false
                  0
                  address
                  metrics
                  backend
                  keys
                  rest (* Parse vaults section for backend type *)
            else if String.equal trimmed "vaults:" then
              parse_lines false true false 0 address metrics backend keys rest
            else if
              in_vaults && indent = 2 && String.ends_with ~suffix:":" trimmed
            then
              (* Extract vault name, which often indicates backend type *)
              let vault_name =
                String.sub trimmed 0 (String.length trimmed - 1) |> String.trim
              in
              parse_lines
                in_server
                in_vaults
                in_tezos
                current_indent
                address
                metrics
                (Some vault_name)
                keys
                rest
            else if
              in_vaults && indent = 0
              && String.length trimmed > 0
              && String.ends_with ~suffix:":" trimmed
            then
              (* Exited vaults section *)
              if String.equal trimmed "tezos:" then
                parse_lines false false true 2 address metrics backend keys rest
              else
                parse_lines
                  false
                  false
                  false
                  0
                  address
                  metrics
                  backend
                  keys
                  rest (* Parse tezos section for authorized keys *)
            else if String.equal trimmed "tezos:" then
              parse_lines false false true 2 address metrics backend keys rest
            else if
              in_tezos && indent = 2
              && (String.starts_with ~prefix:"tz1" trimmed
                 || String.starts_with ~prefix:"tz2" trimmed
                 || String.starts_with ~prefix:"tz3" trimmed
                 || String.starts_with ~prefix:"tz4" trimmed)
              && String.ends_with ~suffix:":" trimmed
            then
              let key =
                String.sub trimmed 0 (String.length trimmed - 1) |> String.trim
              in
              parse_lines
                in_server
                in_vaults
                in_tezos
                current_indent
                address
                metrics
                backend
                (key :: keys)
                rest
            else if
              in_tezos && indent = 0
              && String.length trimmed > 0
              && String.ends_with ~suffix:":" trimmed
            then
              (* Exited tezos section *)
              parse_lines false false false 0 address metrics backend keys rest
            else
              parse_lines
                in_server
                in_vaults
                in_tezos
                current_indent
                address
                metrics
                backend
                keys
                rest
      in
      parse_lines false false false 0 None None None [] lines
    with
    | Sys_error msg -> R.error_msg msg
    | e ->
        R.error_msgf
          "Failed to parse signatory config: %s"
          (Printexc.to_string e)

(** Internal functions exposed for testing *)
module Internal_for_tests = struct
  let generate_watermark_section = generate_watermark_section
end
