(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

module type S = sig
  val start :
    ?quiet:bool ->
    role:string ->
    instance:string ->
    unit ->
    (unit, [> `Msg of string]) result

  val stop :
    ?quiet:bool ->
    role:string ->
    instance:string ->
    unit ->
    (unit, [> `Msg of string]) result

  val restart :
    ?quiet:bool ->
    role:string ->
    instance:string ->
    unit ->
    (unit, [> `Msg of string]) result

  val enable :
    ?quiet:bool ->
    role:string ->
    instance:string ->
    start_now:bool ->
    unit ->
    (unit, [> `Msg of string]) result

  val disable :
    ?quiet:bool ->
    role:string ->
    instance:string ->
    stop_now:bool ->
    unit ->
    (unit, [> `Msg of string]) result

  val is_active :
    role:string -> instance:string -> (bool, [> `Msg of string]) result

  val is_enabled :
    role:string -> instance:string -> (string, [> `Msg of string]) result

  val status :
    role:string -> instance:string -> (string, [> `Msg of string]) result
end
