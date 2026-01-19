(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

val register_pages : unit -> unit

val run :
  ?page:string ->
  ?log:bool ->
  ?logfile:string ->
  unit ->
  (unit, [> `Msg of string]) result
