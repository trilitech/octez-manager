(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** State management for instances page *)

module Service_state = Data.Service_state

module StringSet : Set.S with type elt = string

(** Recent failure tracking *)
val recent_failure_ttl : float

val record_failure : instance:string -> error:string -> unit

val clear_failure : instance:string -> unit

val get_recent_failure : instance:string -> string option

(** Layout functions - counts depend on whether update is available *)
val menu_item_count : unit -> int

val services_start_idx : unit -> int

(** Instances page state *)
type state = {
  services : Service_state.t list;
  external_services : Octez_manager_lib.External_service.t list;
  selected : int;
  folded : StringSet.t; (* managed instance names that are folded *)
  external_folded : StringSet.t; (* external instance names that are folded *)
  last_updated : float;
  num_columns : int;
  active_column : int;
  column_scroll : int array;
}

type msg = unit

type pstate = state Miaou.Core.Navigation.t

(** Clamp selection index to valid range *)
val clamp_selection :
  Service_state.t list ->
  Octez_manager_lib.External_service.t list ->
  int ->
  int

(** Get currently selected service, if any *)
val current_service : state -> Service_state.t option
