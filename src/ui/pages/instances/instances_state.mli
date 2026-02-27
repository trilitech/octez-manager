(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** State management for instances page *)

module Service_state = Data.Service_state

module StringSet : Set.S with type elt = string

(** View mode for instances page layout *)
type view_mode =
  | By_role  (** Group services by role (node, baker, etc.) *)
  | By_group  (** Group services by instance group *)

(** Recent failure tracking *)
val recent_failure_ttl : float

(** Record an action failure for [instance] with the given [error] message.
    The failure is displayed temporarily (see {!recent_failure_ttl}). *)
val record_failure : instance:string -> error:string -> unit

(** Clear the recorded failure for [instance]. *)
val clear_failure : instance:string -> unit

(** Get the most recent failure message for [instance], if still within TTL. *)
val get_recent_failure : instance:string -> string option

(** Layout constants *)
val menu_item_count : int

val services_start_idx : int

(** Instances page state *)
type state = {
  services : Service_state.t list;
  external_services : Octez_manager_lib.External_service.t list;
  selected : int;
  folded : StringSet.t; (* managed instance names that are folded *)
  external_folded : StringSet.t; (* external instance names that are folded *)
  external_section_folded : bool;
  last_updated : float;
  num_columns : int;
  active_column : int;
  column_scroll : int array;
  view_mode : view_mode;
  groups : Octez_manager_lib.Group.t list;
  create_menu_open : bool;
  create_menu_cursor : int;
}

type msg = unit

type pstate = state Miaou.Core.Navigation.t

(** Load groups from registry, returning empty list on error. *)
val load_groups : unit -> Octez_manager_lib.Group.t list

(** Clamp selection index to valid range *)
val clamp_selection :
  Service_state.t list ->
  Octez_manager_lib.External_service.t list ->
  int ->
  int

(** Services in display order (respects view_mode grouping) *)
val display_ordered_services : state -> Service_state.t list

(** Get currently selected service, if any *)
val current_service : state -> Service_state.t option
