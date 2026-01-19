(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

(** Instance management command *)

(** The instance command with subcommands (start, stop, restart, remove, etc.) *)
val instance_cmd : unit Cmdliner.Cmd.t
