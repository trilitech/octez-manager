(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type severity = Info | Success | Warn | Error

type position = [`Top_left | `Top_right | `Bottom_left | `Bottom_right]

type toast = private {
  id : int;
  message : string;
  severity : severity;
  created_at : float;
  ttl : float;
}

type t = private {queue : toast list; next_id : int; position : position}

val empty : ?position:position -> unit -> t

val enqueue : ?ttl:float -> ?now:float -> t -> severity -> string -> t

val dismiss : t -> id:int -> t

val tick : ?now:float -> t -> t

val with_position : t -> position -> t

val to_list : t -> toast list

(* Render stacked toasts within [cols] columns. *)
val render : t -> cols:int -> string
