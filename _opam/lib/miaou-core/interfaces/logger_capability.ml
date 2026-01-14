(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)
(* (c) 2025 Nomadic Labs <contact@nomadic-labs.com> *)

[@@@warning "-32-34-37-69"]

type level = Debug | Info | Warning | Error

type t = {
  logf : level -> string -> unit;
  set_enabled : bool -> unit;
  set_logfile : string option -> (unit, string) result;
}

module Capability = Capability

let key : t Capability.key = Capability.create ~name:"Logger"

let set v = Capability.set key v

let get () = Capability.get key

let require () = Capability.require key
