(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** State management for instances page *)

module Service_state = Data.Service_state

module StringSet : Set.S with type elt = string

(** Recent failure tracking *)
val recent_failure_ttl : float

val record_failure : instance:string -> error:string -> unit

val clear_failure : instance:string -> unit

val get_recent_failure : instance:string -> string option

(** Layout constants *)
val menu_item_count : int

val services_start_idx : int

(** Instances page state *)
type state = {
  services : Service_state.t list;
  selected : int;
  folded : StringSet.t;
  last_updated : float;
  num_columns : int;
  active_column : int;
  column_scroll : int array;
}

type msg = unit

type pstate = state Miaou.Core.Navigation.t

(** Clamp selection index to valid range *)
val clamp_selection : Service_state.t list -> int -> int

(** Get currently selected service, if any *)
val current_service : state -> Service_state.t option
