(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)
(* (c) 2025 Nomadic Labs <contact@nomadic-labs.com> *)

[@@@warning "-32-34-37-69"]

[@@@warning "-32-34-37-69"]

type status = Running | Stopped | Failed of string

type t = {
  start : role:string -> service:string -> (unit, string) result;
  stop : role:string -> service:string -> (unit, string) result;
  restart : role:string -> service:string -> (unit, string) result;
  get_status : role:string -> service:string -> (status, string) result;
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
    role:string -> inst:string -> remove_data:bool -> (unit, string) result;
}

module Capability = Miaou_interfaces.Capability

(* Use the same capability key instance as the interface so registration by the
   Systemd-backed implementation (which registers the interface key) is visible
   here. Coerce the interface key to this module's type with Obj.magic. *)
let key : t Capability.key = Obj.magic Miaou_interfaces.Service_lifecycle.key

let set v = Capability.set key v

let get () =
  match Capability.get key with
  | Some v -> Some v
  | None -> (
      (* If another module registered the interface-level capability under
         the interface's key, consult it and coerce to our type. This covers
         registration ordering races where the interface registration may
         be visible via the interface module but not via this module's
         local key instance. *)
      match Miaou_interfaces.Service_lifecycle.get () with
      | Some v -> Some (Obj.magic v)
      | None -> None)

let require () =
  match get () with
  | Some v -> v
  | None ->
      (* Register a fallback stub that returns Errors. *)
      let start ~role:_ ~service:_ = Error "service lifecycle not available" in
      let stop ~role:_ ~service:_ = Error "service lifecycle not available" in
      let restart ~role:_ ~service:_ =
        Error "service lifecycle not available"
      in
      let get_status ~role:_ ~service:_ =
        Error "service lifecycle not available"
      in
      let install_unit ~role:_ ~app_bin_dir:_ ~user:_ =
        Error "service lifecycle not available"
      in
      let write_dropin_node ~inst:_ ~data_dir:_ ~app_bin_dir:_ =
        Error "service lifecycle not available"
      in
      let enable_start ~role:_ ~inst:_ =
        Error "service lifecycle not available"
      in
      let enable ~role:_ ~inst:_ = Error "service lifecycle not available" in
      let disable ~role:_ ~inst:_ = Error "service lifecycle not available" in
      let remove_instance_files ~role:_ ~inst:_ ~remove_data:_ =
        Error "service lifecycle not available"
      in
      let stub =
        {
          start;
          stop;
          restart;
          get_status;
          install_unit;
          write_dropin_node;
          enable_start;
          enable;
          disable;
          remove_instance_files;
        }
      in
      (* Do NOT persistently register the stub; return it transiently so a
         later real registration (e.g., Systemd_service_lifecycle.register)
         can still override and be observed via the interface key. *)
      stub

let install_unit v ~role ~app_bin_dir ~user =
  v.install_unit ~role ~app_bin_dir ~user

let write_dropin_node v ~inst ~data_dir ~app_bin_dir =
  v.write_dropin_node ~inst ~data_dir ~app_bin_dir

let enable_start v ~role ~inst = v.enable_start ~role ~inst

let enable v ~role ~inst = v.enable ~role ~inst

let disable v ~role ~inst = v.disable ~role ~inst

let start v ~role ~service = v.start ~role ~service

let stop v ~role ~service = v.stop ~role ~service

let restart v ~role ~service = v.restart ~role ~service

let get_status v ~role ~service = v.get_status ~role ~service

let remove_instance_files v ~role ~inst ~remove_data =
  v.remove_instance_files ~role ~inst ~remove_data
