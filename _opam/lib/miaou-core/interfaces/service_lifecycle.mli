(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)
type service_status = [`Active | `Inactive | `Failed of string]

type t

val create :
  start:(role:string -> service:string -> (unit, string) result) ->
  stop:(role:string -> service:string -> (unit, string) result) ->
  restart:(role:string -> service:string -> (unit, string) result) ->
  status:(role:string -> service:string -> (service_status, string) result) ->
  install_unit:
    (role:string ->
    app_bin_dir:string option ->
    user:string ->
    (unit, string) result) ->
  write_dropin_node:
    (inst:string ->
    data_dir:string ->
    app_bin_dir:string option ->
    (unit, string) result) ->
  enable_start:(role:string -> inst:string -> (unit, string) result) ->
  enable:(role:string -> inst:string -> (unit, string) result) ->
  disable:(role:string -> inst:string -> (unit, string) result) ->
  remove_instance_files:
    (inst:string -> remove_data:bool -> (unit, string) result) ->
  t

val key : t Capability.key

val register : t -> unit

val get : unit -> t option

val require : unit -> t

val start : t -> role:string -> service:string -> (unit, string) result

val stop : t -> role:string -> service:string -> (unit, string) result

val restart : t -> role:string -> service:string -> (unit, string) result

val get_status :
  t -> role:string -> service:string -> (service_status, string) result

val install_unit :
  t ->
  role:string ->
  app_bin_dir:string option ->
  user:string ->
  (unit, string) result

val write_dropin_node :
  t ->
  inst:string ->
  data_dir:string ->
  app_bin_dir:string option ->
  (unit, string) result

val enable_start : t -> role:string -> inst:string -> (unit, string) result

val enable : t -> role:string -> inst:string -> (unit, string) result

val disable : t -> role:string -> inst:string -> (unit, string) result

val remove_instance_files :
  t -> inst:string -> remove_data:bool -> (unit, string) result
