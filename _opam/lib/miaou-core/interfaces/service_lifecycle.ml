(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)
type service_status = [`Active | `Inactive | `Failed of string]

type t = {
  start : role:string -> service:string -> (unit, string) result;
  stop : role:string -> service:string -> (unit, string) result;
  restart : role:string -> service:string -> (unit, string) result;
  status : role:string -> service:string -> (service_status, string) result;
  install_unit :
    role:string ->
    app_bin_dir:string option ->
    user:string ->
    (unit, string) result;
  write_dropin_node :
    inst:string ->
    data_dir:string ->
    app_bin_dir:string option ->
    (unit, string) result;
  enable_start : role:string -> inst:string -> (unit, string) result;
  enable : role:string -> inst:string -> (unit, string) result;
  disable : role:string -> inst:string -> (unit, string) result;
  remove_instance_files :
    inst:string -> remove_data:bool -> (unit, string) result;
}

module Capability = Capability

let key : t Capability.key = Capability.create ~name:"service_lifecycle"

let create ~start ~stop ~restart ~status ~install_unit ~write_dropin_node
    ~enable_start ~enable ~disable ~remove_instance_files =
  {
    start;
    stop;
    restart;
    status;
    install_unit;
    write_dropin_node;
    enable_start;
    enable;
    disable;
    remove_instance_files;
  }

let register v = Capability.set key v

let get () = Capability.get key

let require () = Capability.require key

let start t ~role ~service = t.start ~role ~service

let stop t ~role ~service = t.stop ~role ~service

let restart t ~role ~service = t.restart ~role ~service

let get_status t ~role ~service = t.status ~role ~service

let install_unit t ~role ~app_bin_dir ~user =
  t.install_unit ~role ~app_bin_dir ~user

let write_dropin_node t ~inst ~data_dir ~app_bin_dir =
  t.write_dropin_node ~inst ~data_dir ~app_bin_dir

let enable_start t ~role ~inst = t.enable_start ~role ~inst

let enable t ~role ~inst = t.enable ~role ~inst

let disable t ~role ~inst = t.disable ~role ~inst

let remove_instance_files t ~inst ~remove_data =
  t.remove_instance_files ~inst ~remove_data
