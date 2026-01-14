(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Max width specification for dynamic modal sizing *)
type max_width_spec =
  | Fixed of int
  | Ratio of float
  | Clamped of {ratio : float; min : int; max : int}

(** Resolve a max_width_spec to actual columns given terminal width *)
val resolve_max_width : max_width_spec -> cols:int -> int option

val set_provider :
  (unit ->
  (string
  * int option
  * max_width_spec option
  * bool
  * (LTerm_geom.size -> string))
  list) ->
  unit

val get_stack_snapshot :
  unit ->
  (string
  * int option
  * max_width_spec option
  * bool
  * (LTerm_geom.size -> string))
  list
