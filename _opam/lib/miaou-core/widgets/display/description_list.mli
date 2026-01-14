(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type t

val create :
  ?title:string -> ?key_width:int -> ?items:(string * string) list -> unit -> t

val set_items : t -> (string * string) list -> t

(** Render a description list.
    - [cols] total width budget (default 80) used to wrap values.
    - [wrap] when true (default), values wrap to the available width and
      subsequent lines are indented under the key. *)
val render : ?cols:int -> ?wrap:bool -> t -> focus:bool -> string
