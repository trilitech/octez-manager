(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
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
  created_at : string;
  logging_mode : Logging_mode.t;
  snapshot_auto : bool;
  snapshot_uri : string option;
  snapshot_network_slug : string option;
  snapshot_no_check : bool;
  extra_args : string list;
  depends_on : string option;
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
  logging_mode:Logging_mode.t ->
  ?snapshot_auto:bool ->
  ?snapshot_uri:string option ->
  ?snapshot_network_slug:string option ->
  ?snapshot_no_check:bool ->
  ?extra_args:string list ->
  ?depends_on:string option ->
  unit ->
  t

val now : unit -> string

val to_yojson : t -> Yojson.Safe.t

val of_yojson : Yojson.Safe.t -> (t, [> `Msg of string]) result
