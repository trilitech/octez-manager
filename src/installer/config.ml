(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Rresult
open Installer_types
open Helpers

let ensure_node_config ?(quiet = false) ~app_bin_dir ~data_dir ~network
    ~history_mode () =
  let config_path = Filename.concat data_dir "config.json" in
  if Sys.file_exists config_path then Ok ()
  else
    let octez_node = Filename.concat app_bin_dir "octez-node" in
    Common.run
      ~quiet
      [
        octez_node;
        "config";
        "init";
        "--network";
        network;
        "--data-dir";
        data_dir;
        "--history-mode";
        History_mode.to_string history_mode;
      ]

let normalize_data_dir instance = function
  | Some dir when dir <> "" -> dir
  | _ -> Common.default_data_dir instance

let endpoint_of_rpc rpc_addr =
  let trimmed = String.trim rpc_addr in
  if trimmed = "" then "http://127.0.0.1:8732"
  else if
    String.starts_with ~prefix:"http://" (String.lowercase_ascii trimmed)
    || String.starts_with ~prefix:"https://" (String.lowercase_ascii trimmed)
  then trimmed
  else "http://" ^ trimmed

let lookup_node_service instance =
  let* svc_opt = Service_registry.find ~instance in
  match svc_opt with
  | Some svc when String.equal (String.lowercase_ascii svc.Service.role) "node"
    ->
      Ok svc
  | Some svc ->
      R.error_msgf
        "Instance '%s' is a %s service, expected a node"
        instance
        svc.role
  | None -> R.error_msgf "Unknown instance '%s'" instance

let build_run_args ~network ~history_mode ~rpc_addr ~net_addr ~extra_args
    ~logging_mode:_ =
  let base =
    [
      "--network=" ^ network;
      "--history-mode=" ^ History_mode.to_string history_mode;
      "--rpc-addr=" ^ rpc_addr;
      "--net-addr=" ^ net_addr;
    ]
  in
  (* Logging is via journald - octez binaries handle their own file logging *)
  String.concat " " (base @ extra_args)

let prepare_logging ~instance:_ ~role:_ ~logging_mode:_ =
  (* Always use journald *)
  Logging_mode.default

(* Logging is via journald - no file setup needed *)
let ensure_logging_destination ~service_user:_ _logging_mode = Ok ()

let ensure_logging_base_directory ~owner:_ ~group:_ _logging_mode = Ok ()

let remove_logging_artifacts _logging_mode = Ok ()

let should_drop_service_user ~user ~remaining_services =
  let trimmed = String.trim user in
  if trimmed = "" then false
  else
    not
      (List.exists
         (fun (svc : Service.t) ->
           String.equal trimmed (String.trim svc.Service.service_user))
         remaining_services)

(* Logging is via journald - no file setup needed *)
let ensure_runtime_log_directory ~owner:_ ~group:_ _logging_mode = Ok ()

let ensure_directories ~owner ~group paths =
  let filtered =
    paths
    |> List.filter (fun p -> String.trim p <> "")
    |> List.sort_uniq String.compare
  in
  List.fold_left
    (fun acc dir ->
      let* () = acc in
      Common.ensure_dir_path ~owner ~group ~mode:0o755 dir)
    (Ok ())
    filtered

let reown_runtime_paths ~owner ~group ~paths ~logging_mode:_ =
  let normalize =
    paths
    |> List.filter (fun p -> String.trim p <> "")
    |> List.sort_uniq String.compare
  in
  (* Logging is via journald - no file ownership needed *)
  List.fold_left
    (fun acc dir ->
      let* () = acc in
      Common.ensure_tree_owner ~owner ~group dir)
    (Ok ())
    normalize

let is_valid_instance_char c =
  match c with
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' | '.' -> true
  | _ -> false

let validate_instance_name_chars ~instance =
  if String.length instance = 0 then
    R.error_msgf "Instance name cannot be empty."
  else if not (String.for_all is_valid_instance_char instance) then
    R.error_msgf
      "Instance name '%s' contains invalid characters. %s"
      instance
      invalid_instance_name_chars_msg
  else Ok ()

let validate_instance_name_unique ~instance =
  let* services = Service_registry.list () in
  let existing =
    List.find_opt
      (fun svc -> String.equal svc.Service.instance instance)
      services
  in
  match existing with
  | Some svc ->
      R.error_msgf
        "Instance name '%s' is already in use by a %s service. Please choose a \
         different name."
        instance
        svc.Service.role
  | None -> Ok ()

let validate_instance_name ?(allow_existing = false) ~instance () =
  let* () = validate_instance_name_chars ~instance in
  if allow_existing then Ok () else validate_instance_name_unique ~instance

let resolve_from_data_dir data_dir =
  let ( let* ) = Result.bind in
  let config_path = Filename.concat data_dir "config.json" in
  if not (Sys.file_exists config_path) then Ok (`Path data_dir)
  else
    let json = Yojson.Safe.from_file config_path in
    let open Yojson.Safe.Util in
    let fail field =
      Error (Format.sprintf "Cannot read %S from %S" field config_path)
    in
    let* network =
      try
        match member "network" json |> member "chain_name" with
        | `String s ->
            let* network =
              Result.map_error (fun (`Msg s) -> s)
              @@ Teztnets.resolve_network_from_node_chain s
            in
            Ok network.alias
        | _ -> fail ".network.chain_name"
      with _ -> Ok "mainnet"
    in
    let* history_mode =
      try
        match member "shell" json |> member "history_mode" with
        | `String s ->
            Result.map_error (fun (`Msg s) -> s) @@ History_mode.of_string s
        | `Assoc [("full", _)] -> Ok History_mode.Full
        | `Assoc [("rolling", _)] -> Ok History_mode.Rolling
        | `Assoc [("archive", _)] -> Ok History_mode.Archive
        | _ -> fail ".p2p.shell.history_mode"
      with _ -> Ok History_mode.default
    in
    let* rpc_addr =
      try
        match member "rpc" json |> member "listen-addrs" with
        | `List [`String s] -> Ok s
        | `List _ ->
            Error
              (Format.sprintf
                 "Multiple rpc addresses listed in %S are not supported"
                 config_path)
        | _ -> Ok "127.0.0.1:8732"
      with _ -> Ok "127.0.0.1:8732"
    in
    let net_addr =
      match member "p2p" json |> member "listen-addr" with
      | `String s -> s
      | _ -> "0.0.0.0:9732"
    in
    Ok (`Data_dir {network; history_mode; rpc_addr; net_addr})

(** Update endpoint references in dependent services when a service's RPC address changes.
    For nodes: updates OCTEZ_NODE_ENDPOINT in baker/accuser/dal-node env files.
    For DAL nodes: updates OCTEZ_DAL_CONFIG in baker env files. *)
let update_dependent_endpoints ~instance ~role ~new_rpc_addr () =
  let new_endpoint = endpoint_of_rpc new_rpc_addr in
  let* svc_opt = Service_registry.find ~instance in
  match svc_opt with
  | None -> Ok ()
  | Some svc ->
      List.fold_left
        (fun acc dep_inst ->
          let* () = acc in
          match Node_env.read ~inst:dep_inst with
          | Ok pairs ->
              let updated_pairs =
                List.map
                  (fun (k, v) ->
                    (* For node: update OCTEZ_NODE_ENDPOINT in dependents *)
                    if role = "node" && k = "OCTEZ_NODE_ENDPOINT" then
                      (k, new_endpoint)
                      (* For DAL node: update OCTEZ_DAL_CONFIG in baker dependents *)
                    else if
                      (role = "dal-node" || role = "dal")
                      && k = "OCTEZ_DAL_CONFIG"
                    then (k, new_endpoint)
                    else (k, v))
                  pairs
              in
              Node_env.write_pairs ~inst:dep_inst updated_pairs
          | Error _ -> Ok ())
        (Ok ())
        svc.dependents
