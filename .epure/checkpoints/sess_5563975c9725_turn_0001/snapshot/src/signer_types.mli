(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Remote signer mode for bakers *)
type signer_mode =
  | Local_keys  (** Use local key files from base directory *)
  | Remote_signer of {
      instance : string option;
          (** Managed Signatory instance name for dependency tracking *)
      uri : string;
          (** Full URI: http://host:port, https://host:port, or unix:/path *)
    }
      (**
   - If instance = Some name: managed instance, track as dependency
   - If instance = None: external signer, no dependency tracking
*)
