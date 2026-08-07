(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Validation functions for Signatory configuration *)

(** Validate a Tezos public key hash (tz1/tz2/tz3/tz4 format).
    
    @param pkh Public key hash string
    @return [Ok ()] if valid, [Error] with message otherwise *)
val validate_public_key_hash : string -> (unit, [> `Msg of string]) result

(** Validate a list of authorized keys.
    
    @param keys List of authorized_key records
    @return [Ok ()] if all valid, [Error] with message for first invalid key *)
val validate_authorized_keys :
  Installer_types.authorized_key list -> (unit, [> `Msg of string]) result

(** Validate an HTTP address (host:port format).
    
    @param addr Address string
    @param name Field name for error messages
    @return [Ok ()] if valid, [Error] with message otherwise *)
val validate_http_address :
  addr:string -> name:string -> (unit, [> `Msg of string]) result

(** Validate Signatory backend configuration.
    
    @param backend Backend configuration
    @return [Ok ()] if valid, [Error] with message otherwise *)
val validate_backend :
  Installer_types.signatory_backend -> (unit, [> `Msg of string]) result

(** Validate watermark backend configuration.
    
    @param watermark Watermark backend configuration
    @return [Ok ()] if valid, [Error] with message otherwise *)
val validate_watermark :
  Installer_types.watermark_backend -> (unit, [> `Msg of string]) result

(** Validate a complete Signatory request.
    
    @param req Signatory installation request
    @return [Ok ()] if all fields valid, [Error] with first validation failure *)
val validate_request :
  Installer_types.signatory_request -> (unit, [> `Msg of string]) result
