(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Validation functions for remote signer URIs and configuration *)

(** Validate a remote signer URI format.
    
    Accepts:
    - http://host:port
    - https://host:port
    - unix:/path/to/socket
    
    Does NOT validate connectivity, DNS resolution, or whether the service
    is actually running. This is format-only validation.
    
    @param uri The URI string to validate
    @return Ok uri if valid format, Error with message if invalid *)
val validate_uri : string -> (string, Rresult.R.msg) result

(** Resolve a Signatory instance name to its HTTP address.
    
    Looks up the managed Signatory instance in the service registry and
    constructs an http:// URI from its RPC address.
    
    @param instance The Signatory instance name
    @return Ok "http://host:port" if instance exists, Error if not found *)
val resolve_signatory_instance : string -> (string, Rresult.R.msg) result

(** Validate signer_mode configuration and return resolved URI.
    
    - For Local_keys: returns Ok None
    - For Remote_signer with instance = Some name: validates instance exists,
      resolves to URI, returns Ok (Some uri)
    - For Remote_signer with instance = None: validates URI format,
      returns Ok (Some uri)
    
    @param signer_mode The signer mode configuration
    @return Ok (Some uri) for remote signers, Ok None for local keys *)
val validate_and_resolve :
  Signer_types.signer_mode -> (string option, Rresult.R.msg) result
