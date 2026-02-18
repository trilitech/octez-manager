(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

(** Parse signatory.yaml configuration to extract authorized keys. *)

(** Information about an authorized signing key. *)
type key_info = {
  pkh : string;  (** Public key hash (tz1/tz2/tz3/tz4...) *)
  allows : string list;  (** Allowed operations (block, attestation, etc.) *)
}

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
