(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com> *)

type 'a key

val create : name:string -> 'a key

val set : 'a key -> 'a -> unit

val register : 'a key -> 'a -> unit

val get : 'a key -> 'a option

val require : 'a key -> 'a

val mem : 'a key -> bool

val clear : unit -> unit

val list : unit -> (string * bool) list

type any = Any : 'a key -> any

val any : 'a key -> any

val check_all : any list -> string list
