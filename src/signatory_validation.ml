(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Validation functions for Signatory configuration *)

open Rresult

(** Validate a Tezos public key hash (tz1/tz2/tz3/tz4 format).
    
    @param pkh Public key hash string
    @return Ok () if valid, Error with message otherwise *)
let validate_public_key_hash pkh =
  let len = String.length pkh in
  if len < 36 then
    Error (`Msg (Printf.sprintf "Invalid public key hash '%s': too short" pkh))
  else
    let prefix = String.sub pkh 0 3 in
    match prefix with
    | "tz1" | "tz2" | "tz3" | "tz4" ->
        (* Basic validation: correct prefix and reasonable length *)
        if len >= 36 && len <= 40 then Ok ()
        else
          Error
            (`Msg
               (Printf.sprintf
                  "Invalid public key hash '%s': expected 36-40 characters, \
                   got %d"
                  pkh
                  len))
    | _ ->
        Error
          (`Msg
             (Printf.sprintf
                "Invalid public key hash '%s': must start with tz1, tz2, tz3, \
                 or tz4"
                pkh))

(** Validate a list of authorized keys.
    
    @param keys List of public key hashes
    @return Ok () if all valid, Error with message for first invalid key *)
let validate_authorized_keys keys =
  if keys = [] then Error (`Msg "At least one authorized key is required")
  else
    List.fold_left
      (fun acc key ->
        match acc with Error _ -> acc | Ok () -> validate_public_key_hash key)
      (Ok ())
      keys

(** Validate an HTTP address (host:port format).
    
    @param addr Address string
    @param name Field name for error messages
    @return Ok () if valid, Error with message otherwise *)
let validate_http_address ~addr ~name =
  match String.split_on_char ':' addr with
  | [host; port_str] -> (
      if host = "" then
        Error (`Msg (Printf.sprintf "%s: host cannot be empty" name))
      else
        try
          let port = int_of_string port_str in
          if port < 1 || port > 65535 then
            Error
              (`Msg (Printf.sprintf "%s: port must be between 1 and 65535" name))
          else Ok ()
        with Failure _ ->
          Error
            (`Msg (Printf.sprintf "%s: invalid port number '%s'" name port_str))
      )
  | _ ->
      Error
        (`Msg
           (Printf.sprintf
              "%s: must be in format 'host:port', got '%s'"
              name
              addr))

(** Validate Signatory backend configuration.
    
    @param backend Backend configuration
    @return Ok () if valid, Error with message otherwise *)
let validate_backend = function
  | Installer_types.File path ->
      if path = "" then Error (`Msg "Backend: file path cannot be empty")
      else Ok ()
  | Installer_types.YubiHSM {connector_url} ->
      if connector_url = "" then
        Error (`Msg "Backend: YubiHSM connector URL cannot be empty")
      else Ok ()
  | Installer_types.Azure_KMS {vault_name; tenant_id} ->
      if vault_name = "" then
        Error (`Msg "Backend: Azure KMS vault name cannot be empty")
      else if tenant_id = "" then
        Error (`Msg "Backend: Azure KMS tenant ID cannot be empty")
      else Ok ()
  | Installer_types.AWS_KMS {region} ->
      if region = "" then Error (`Msg "Backend: AWS KMS region cannot be empty")
      else Ok ()
  | Installer_types.GCP_KMS {project_id; location} ->
      if project_id = "" then
        Error (`Msg "Backend: GCP KMS project ID cannot be empty")
      else if location = "" then
        Error (`Msg "Backend: GCP KMS location cannot be empty")
      else Ok ()
  | Installer_types.Vault {address; role} ->
      if address = "" then Error (`Msg "Backend: Vault address cannot be empty")
      else if role = "" then Error (`Msg "Backend: Vault role cannot be empty")
      else Ok ()

(** Validate watermark backend configuration.
    
    @param watermark Watermark backend configuration
    @return Ok () if valid, Error with message otherwise *)
let validate_watermark = function
  | Installer_types.Memory -> Ok ()
  | Installer_types.File_watermark path ->
      if path = "" then Error (`Msg "Watermark: file path cannot be empty")
      else Ok ()
  | Installer_types.AWS_DynamoDB {table_name; region} ->
      if table_name = "" then
        Error (`Msg "Watermark: DynamoDB table name cannot be empty")
      else if region = "" then
        Error (`Msg "Watermark: DynamoDB region cannot be empty")
      else Ok ()
  | Installer_types.GCP_Firestore {project_id; collection} ->
      if project_id = "" then
        Error (`Msg "Watermark: Firestore project ID cannot be empty")
      else if collection = "" then
        Error (`Msg "Watermark: Firestore collection cannot be empty")
      else Ok ()

(** Validate a complete Signatory request.
    
    @param req Signatory installation request
    @return Ok () if all fields valid, Error with first validation failure *)
let validate_request (req : Installer_types.signatory_request) =
  let open Rresult.R.Infix in
  validate_authorized_keys req.authorized_keys >>= fun () ->
  validate_http_address ~addr:req.address ~name:"address" >>= fun () ->
  validate_http_address ~addr:req.metrics_address ~name:"metrics_address"
  >>= fun () ->
  validate_backend req.backend >>= fun () -> validate_watermark req.watermark
