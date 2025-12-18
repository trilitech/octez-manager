(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

module type S = sig
  val start :
    role:string -> instance:string -> (unit, [> `Msg of string]) result

  val stop : role:string -> instance:string -> (unit, [> `Msg of string]) result

  val restart :
    role:string -> instance:string -> (unit, [> `Msg of string]) result

  val enable :
    role:string ->
    instance:string ->
    start_now:bool ->
    (unit, [> `Msg of string]) result

  val disable :
    role:string ->
    instance:string ->
    stop_now:bool ->
    (unit, [> `Msg of string]) result

  val is_active :
    role:string -> instance:string -> (bool, [> `Msg of string]) result

  val is_enabled :
    role:string -> instance:string -> (string, [> `Msg of string]) result

  val status :
    role:string -> instance:string -> (string, [> `Msg of string]) result
end
