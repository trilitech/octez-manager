(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

let normalize (network : string) : string =
  match Public_nodes_cache.extract_network_from_url network with
  | Some canonical -> canonical
  | None -> network
(* Keep custom URLs as-is *)
