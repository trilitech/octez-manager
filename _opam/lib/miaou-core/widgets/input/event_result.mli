(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type t = [`Handled | `Bubble]

val handled : t

val bubble : t

(** [to_bool] returns [true] when handled, [false] when bubbling. Useful to
    bridge existing [t * bool] APIs. *)
val to_bool : t -> bool
