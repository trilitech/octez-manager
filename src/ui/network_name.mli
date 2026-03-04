(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Network name normalization utilities.

    Provides functions to convert network identifiers between URL format
    and canonical names, ensuring consistent network identification across
    the application. *)

(** Normalize network identifier to canonical name.

    Uses {!Public_nodes_cache.extract_network_from_url} for known networks,
    preserving custom URLs that aren't recognized.

    Examples:
    - ["https://teztnets.com/tallinnnet"] -> ["tallinnnet"]
    - ["tallinnnet"] -> ["tallinnnet"]
    - ["https://custom-rpc.com/privatenet"] -> ["https://custom-rpc.com/privatenet"] (preserved)

    @param network The network identifier (URL or canonical name)
    @return The canonical network name, or the original string if not recognized *)
val normalize : string -> string
