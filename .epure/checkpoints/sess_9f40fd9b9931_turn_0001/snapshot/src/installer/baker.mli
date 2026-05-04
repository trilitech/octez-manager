(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Baker installation functionality *)

open Installer_types

(** Install or update a baker service.
    
    This orchestrates the complete baker installation process:
    - Resolves node mode (local instance or remote endpoint)
    - Validates instance name
    - Resolves network configuration from node
    - Configures base directory and DAL settings
    - Creates service account and directories
    - Installs systemd unit and creates service registry entry
    - Registers as dependent on node (if using local instance)
    - Optionally enables and starts the service
    
    @param quiet Suppress command output
    @param request Baker installation request with all configuration
    @return Created/updated service record *)
val install_baker :
  ?quiet:bool -> baker_request -> (Service.t, Rresult.R.msg) result
