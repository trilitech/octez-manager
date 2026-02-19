(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Signatory remote signer installation. *)

(** Install a Signatory remote signer service.

    This function:
    - Validates the signatory configuration
    - Creates service user and directories
    - Generates Signatory YAML configuration
    - Writes environment file
    - Installs systemd unit
    - Registers service in registry
    - Optionally enables and starts the service

    @param quiet suppress output
    @param request signatory installation request
    @return service record or error *)
val install_signatory :
  ?quiet:bool ->
  Installer_types.signatory_request ->
  (Service.t, Rresult.R.msg) result

(** Get the path to signatory.yaml configuration file for an instance.
    
    @param instance the signatory instance name
    @return absolute path to signatory.yaml *)
val signatory_config_path : string -> string

(** Read authorized keys from a signatory instance's YAML configuration.
    
    Parses the signatory.yaml file to extract the list of public key hashes
    from the "tezos:" section. These are the keys that the signatory is
    configured to sign for.
    
    @param instance the signatory instance name
    @return list of public key hashes (tz1/tz2/tz3/tz4 addresses), or error *)
val read_authorized_keys : string -> (string list, Rresult.R.msg) result

(** Signatory configuration parsed from signatory.yaml *)
type signatory_config = {
  address : string option;  (** Server address (e.g., "127.0.0.1:6732") *)
  metrics_address : string option;  (** Metrics/utility address *)
  backend : string option;  (** Backend type (e.g., "file") *)
  authorized_keys : string list;  (** List of authorized public key hashes *)
}

(** Read full configuration from a signatory instance's YAML file.
    
    Parses the signatory.yaml file to extract:
    - Server address from server.address field
    - Metrics address from server.utility_address field
    - Backend type from vaults section
    - Authorized keys from tezos section
    
    @param instance the signatory instance name
    @return parsed configuration or error *)
val read_config : string -> (signatory_config, Rresult.R.msg) result
