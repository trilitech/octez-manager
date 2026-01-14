(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(* Sidebar + main content layout. Pure state + renderer. *)

type t = {
  sidebar : string;
  main : string;
  sidebar_open : bool;
  sidebar_width : int option;
}

val create :
  ?sidebar_width:int ->
  sidebar:string ->
  main:string ->
  sidebar_open:bool ->
  unit ->
  t

val toggle : t -> t

val with_main : t -> string -> t

val with_sidebar : t -> string -> t

(* Render the layout within [cols] columns, auto-collapsing when too narrow. *)
val render : t -> cols:int -> string
