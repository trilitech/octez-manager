(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

(** CLI commands for managing instance groups. *)

(** Top-level [om group] command with subcommands for group CRUD and lifecycle. *)
val group_cmd : unit Cmdliner.Cmd.t
