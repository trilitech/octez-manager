(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Write (create or overwrite) a sandbox config. *)
val write : Sandbox_config.t -> (unit, [> `Msg of string]) result

(** Find a sandbox config by group name. Returns [None] if not found. *)
val find : name:string -> (Sandbox_config.t option, [> `Msg of string]) result

(** Remove a sandbox config. No-op if not found. *)
val remove : name:string -> (unit, [> `Msg of string]) result
