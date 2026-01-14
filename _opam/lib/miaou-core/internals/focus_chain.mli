(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type t

val create : total:int -> t

val with_total : t -> int -> t

val current : t -> int option

val move : t -> [`Next | `Prev] -> t * [`Handled | `Bubble]

(** Tab/Backtab rotate focus across [total] slots; other keys bubble. *)
val handle_key : t -> key:string -> t * [`Handled | `Bubble]
