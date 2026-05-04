(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** DAL node installation functionality *)

open Installer_types

(** Install or update a DAL node service.
    
    This is a generic daemon installer that handles:
    - Instance name validation
    - Service account and directory setup
    - Environment variable configuration
    - Systemd unit installation
    - Service registry management
    - Parent-child dependency registration
    - Automatic enablement and startup
    
    Used for DAL nodes and potentially other daemon-type services.
    
    @param quiet Suppress command output
    @param request Daemon installation request with all configuration
    @return Created/updated service record *)
val install_daemon :
  ?quiet:bool -> daemon_request -> (Service.t, Rresult.R.msg) result
