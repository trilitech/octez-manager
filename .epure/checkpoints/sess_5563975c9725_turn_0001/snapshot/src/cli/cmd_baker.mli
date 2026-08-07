(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** CLI commands for baker wallet operations.

    Provides [octez-manager baker] subcommand with wallet operations:
    list, status, register, stake, unstake, finalize-unstake, transfer,
    set-delegate-params, update-consensus-key, and vote. *)

(** Top-level [octez-manager baker] command group. *)
val baker_cmd : unit Cmdliner.Cmd.t
