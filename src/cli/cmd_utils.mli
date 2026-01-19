(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                            *)
(******************************************************************************)

(** Utility commands *)

(** List all managed services *)
val list_cmd : unit Cmdliner.Cmd.t

(** Purge all services *)
val purge_all_cmd : unit Cmdliner.Cmd.t

(** Clean up orphaned systemd units *)
val cleanup_orphans_cmd : unit Cmdliner.Cmd.t

(** Clean up orphaned service dependencies *)
val cleanup_dependencies_cmd : unit Cmdliner.Cmd.t

(** List available networks *)
val list_networks_cmd : unit Cmdliner.Cmd.t

(** List available snapshots *)
val list_snapshots_cmd : unit Cmdliner.Cmd.t
