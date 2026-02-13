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
