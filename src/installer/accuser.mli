(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Accuser installation functionality *)

open Installer_types

(** Install or update an accuser service.
    
    This orchestrates the complete accuser installation process:
    - Resolves node mode (local instance or remote endpoint)
    - Validates instance name
    - Resolves network configuration from node
    - Configures base directory
    - Creates service account and directories
    - Installs systemd unit and creates service registry entry
    - Registers as dependent on node (if using local instance)
    - Optionally enables and starts the service
    
    @param quiet Suppress command output
    @param request Accuser installation request with all configuration
    @return Created/updated service record *)
val install_accuser :
  ?quiet:bool -> accuser_request -> (Service.t, Rresult.R.msg) result
