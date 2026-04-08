(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

(** octez-index installation functionality *)

open Installer_types

(** Install or update an octez-index service.

    Writes OCTEZ_INDEXER_DIR, OCTEZ_NODE_ENDPOINT (full URI via
    Config.endpoint_of_rpc), OCTEZ_INDEX_RPC_ADDR, OCTEZ_SERVICE_ARGS
    (--watched-address and --db-name flags), and any extra_env entries.

    @param quiet Suppress command output
    @param request Index installation request *)
val install : ?quiet:bool -> index_request -> (Service.t, Rresult.R.msg) result
