(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

(** Import command for importing existing Octez services from the filesystem. *)

(** The import command *)
val import_cmd : unit Cmdliner.Cmd.t
