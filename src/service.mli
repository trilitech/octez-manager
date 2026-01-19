(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

type t = {
  instance : string;
  role : string;
  network : string;
  history_mode : History_mode.t;
  data_dir : string;
  rpc_addr : string;
  net_addr : string;
  service_user : string;
  app_bin_dir : string;
  bin_source : Binary_registry.bin_source option;
      (** How binaries are referenced. None for legacy configs. *)
  created_at : string;
  logging_mode : Logging_mode.t;
  snapshot_auto : bool;
  snapshot_uri : string option;
  snapshot_network_slug : string option;
  snapshot_no_check : bool;
  extra_args : string list;
  depends_on : string option;
  dependents : string list;
}

val make :
  instance:string ->
  role:string ->
  network:string ->
  history_mode:History_mode.t ->
  data_dir:string ->
  rpc_addr:string ->
  net_addr:string ->
  service_user:string ->
  app_bin_dir:string ->
  ?bin_source:Binary_registry.bin_source ->
  logging_mode:Logging_mode.t ->
  ?snapshot_auto:bool ->
  ?snapshot_uri:string option ->
  ?snapshot_network_slug:string option ->
  ?snapshot_no_check:bool ->
  ?extra_args:string list ->
  ?depends_on:string option ->
  ?dependents:string list ->
  unit ->
  t

(** Get the bin_source, falling back to Raw_path of app_bin_dir for legacy configs *)
val get_bin_source : t -> Binary_registry.bin_source

val now : unit -> string

val to_yojson : t -> Yojson.Safe.t

val of_yojson : Yojson.Safe.t -> (t, [> `Msg of string]) result
