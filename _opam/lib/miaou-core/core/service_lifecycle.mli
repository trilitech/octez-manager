(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)
(* (c) 2025 Nomadic Labs <contact@nomadic-labs.com> *)

(** Service lifecycle capability.

    This capability abstracts starting, stopping and querying status of
    long-lived external services managed by the application (for example
    background daemons). The core uses it to request lifecycle operations
    and to display current status to the user.

    Implementations
    - Should return quickly and avoid blocking the UI thread. If an
      operation performs heavy work, it should schedule it in the
      background and return an acknowledgement soon after.
    - Should return [Error msg] for user-friendly failure messages.

    Example
    {[
      let start ~service = (* spawn systemd unit or run command *) in
      let stop ~service = (* stop unit *) in
      let get_status ~service = Ok Miaou_core.Service_lifecycle.Running in
      Miaou_core.Service_lifecycle.set { start; stop; restart=(fun ~service -> Ok ()); get_status }
    ]}
*)

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

val key : t Miaou_interfaces.Capability.key

val set : t -> unit

val get : unit -> t option

val require : unit -> t

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
  t -> role:string -> inst:string -> remove_data:bool -> (unit, string) result
