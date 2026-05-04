(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Background scheduler for polling octez-index status.

    Reads [OCTEZ_INDEX_RPC_ADDR] from each index instance's environment file
    and polls [/explorer/status] every {!refresh_interval} seconds to
    populate the {!Index_metrics} cache. *)

(** [start ()] spawns the background polling domain and worker queue. *)
val start : unit -> unit

(** [stop ()] requests scheduler shutdown and stops the worker queue. *)
val stop : unit -> unit
