(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** CLI command for installing Octez Signatory services. *)

(** The [install-signatory] subcommand. *)
val install_signatory_cmd : unit Cmdliner.Cmd.t
