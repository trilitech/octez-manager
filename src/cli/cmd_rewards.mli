(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** CLI commands for rewards & payouts.

    Provides [octez-manager rewards] subcommand group with:
    status, generate, and history. *)

(** Top-level [octez-manager rewards] command group. *)
val rewards_cmd : unit Cmdliner.Cmd.t
