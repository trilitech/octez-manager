(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Instance groups — shared configuration for related services. *)

type t = {
  name : string;  (** Unique group identifier *)
  network : string;  (** Shared network (e.g. "mainnet") *)
  bin_source : Binary_registry.bin_source;  (** Shared binary version/path *)
  service_user : string;  (** Shared service user (e.g. "tezos") *)
  app_bin_dir : string;  (** Resolved binary directory *)
  created_at : string;
  sandbox : bool;  (** Whether this group is a sandbox *)
}

(** Create a group configuration record. *)
val make :
  name:string ->
  network:string ->
  bin_source:Binary_registry.bin_source ->
  service_user:string ->
  app_bin_dir:string ->
  ?sandbox:bool ->
  unit ->
  t

(** Serialize a group configuration to JSON. *)
val to_yojson : t -> Yojson.Safe.t

(** Deserialize a group configuration from JSON. *)
val of_yojson : Yojson.Safe.t -> (t, [> `Msg of string]) result
