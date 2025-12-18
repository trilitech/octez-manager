(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

val registry_root : unit -> string

val services_dir : unit -> string

val write : Service.t -> (unit, Rresult.R.msg) result

val list : unit -> (Service.t list, Rresult.R.msg) result

val remove : instance:string -> (unit, Rresult.R.msg) result

val find : instance:string -> (Service.t option, Rresult.R.msg) result
