(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Node installation functionality *)

open Installer_types

(** Install or update a node service.
    
    This orchestrates the complete node installation process:
    - Validates instance name
    - Resolves network configuration
    - Creates service account and directories
    - Configures snapshot bootstrap (if requested)
    - Installs systemd unit and creates service registry entry
    - Optionally enables and starts the service
    
    @param quiet Suppress command output
    @param on_log Optional callback for streaming log messages
    @param request Node installation request with all configuration
    @return Created/updated service record *)
val install_node :
  ?quiet:bool ->
  ?on_log:(string -> unit) ->
  node_request ->
  (Service.t, Rresult.R.msg) result
