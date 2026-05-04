(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Parse signatory.yaml configuration to extract authorized keys. *)

(** Information about an authorized signing key. *)
type key_info = {
  pkh : string;  (** Public key hash (tz1/tz2/tz3/tz4...) *)
  allows : string list;  (** Allowed operations (block, attestation, etc.) *)
}

(** Convert signatory_operation to string representation.
    
    @param op The operation to convert
    @return String representation (e.g., "block", "attestation") *)
val operation_to_string : Installer_types.signatory_operation -> string

(** Convert string to signatory_operation.
    
    @param s The string to parse
    @return Some operation if valid, None otherwise *)
val operation_of_string : string -> Installer_types.signatory_operation option

(** All available signatory operations. *)
val all_operations : Installer_types.signatory_operation list

(** Default permissions for new keys (all operations). *)
val default_permissions : unit -> Installer_types.signatory_operation list

(** Get the path to signatory.yaml for an instance.
    
    @param instance The signatory instance name
    @return Absolute path to the configuration file *)
val config_path : instance:string -> string

(** Parse signatory.yaml and extract authorized keys.
    
    Reads the YAML configuration file for a signatory instance and extracts
    the list of authorized keys from the 'tezos:' section.
    
    @param instance The signatory instance name
    @return List of authorized keys, or error if file cannot be read/parsed *)
val get_authorized_keys :
  instance:string -> (key_info list, [`Msg of string]) result
