(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

(** Parse signatory.yaml configuration to extract authorized keys. *)

open Rresult

type key_info = {pkh : string; allows : string list}

(** Get the path to signatory.yaml for an instance *)
let config_path ~instance =
  let base =
    if Paths.is_root () then "/etc/octez/instances"
    else Filename.concat (Paths.xdg_data_home ()) "octez/instances"
  in
  Filename.concat (Filename.concat base instance) "signatory.yaml"

(** Extract allowed operations from a key's config *)
let extract_allows yaml_value =
  match yaml_value with
  | `O assoc -> (
      match List.assoc_opt "allow" assoc with
      | Some (`O allow_assoc) ->
          (* Extract operation names from the allow section *)
          List.filter_map
            (fun (op_name, _) ->
              (* Filter out operations that might be disabled or have values *)
              match op_name with
              | "generic" -> None (* generic has sub-items, skip for now *)
              | op -> Some op)
            allow_assoc
      | _ -> [])
  | _ -> []

(** Parse the tezos section and extract keys *)
let parse_tezos_section yaml =
  match yaml with
  | `O assoc -> (
      match List.assoc_opt "tezos" assoc with
      | Some (`O tezos_keys) ->
          (* Each key under tezos: is a public key hash *)
          List.map
            (fun (pkh, key_config) ->
              let allows = extract_allows key_config in
              {pkh; allows})
            tezos_keys
      | _ -> [])
  | _ -> []

(** Read and parse signatory.yaml to get authorized keys *)
let get_authorized_keys ~instance =
  let path = config_path ~instance in
  (* Check if file exists first *)
  if not (Sys.file_exists path) then
    Error (`Msg (Printf.sprintf "Configuration file not found: %s" path))
  else
    try
      (* Read file contents *)
      let ic = open_in path in
      Fun.protect
        ~finally:(fun () -> close_in ic)
        (fun () ->
          let content = really_input_string ic (in_channel_length ic) in
          (* Parse YAML *)
          match Yaml.of_string content with
          | Ok yaml ->
              let keys = parse_tezos_section yaml in
              Ok keys
          | Error (`Msg err) ->
              Error (`Msg (Printf.sprintf "Failed to parse YAML: %s" err)))
    with
    | Sys_error err ->
        Error (`Msg (Printf.sprintf "Failed to read file: %s" err))
    | e ->
        Error
          (`Msg (Printf.sprintf "Unexpected error: %s" (Printexc.to_string e)))
