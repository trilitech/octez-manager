(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Per-sandbox network and binary configuration.
    Stored separately from [Group.t] so that groups are network-agnostic.
    Only sandbox groups have an associated [Sandbox_config.t]. *)

type t = {
  group_name : string;
  network : string;
  bin_source : Binary_registry.bin_source;
  created_at : string;
}

val make :
  group_name:string ->
  network:string ->
  bin_source:Binary_registry.bin_source ->
  unit ->
  t

val to_yojson : t -> Yojson.Safe.t

val of_yojson : Yojson.Safe.t -> (t, [> `Msg of string]) result
