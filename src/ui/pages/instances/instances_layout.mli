(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Layout and column calculation for instances page *)

module Service_state = Data.Service_state
open Instances_state

(** Layout configuration constants *)
val min_column_width : int

(** String used to visually separate columns. *)
val column_separator : string

(** Role ordering for grouping *)
val role_order : string -> int

(** Role section headers *)
val role_header : string -> string

(** Sort services by role then instance name *)
val sort_services : Service_state.t list -> Service_state.t list

(** Load services from Data module *)
val load_services : unit -> Service_state.t list

(** Load services from Data module, bypassing the cache. *)
val load_services_fresh : unit -> Service_state.t list

(** Calculate number of columns based on terminal width *)
val calc_num_columns :
  cols:int -> min_column_width:int -> column_separator:string -> int

(** Group services by role, returning display items with ghosts *)
val group_by_role : Service_state.t list -> (string * display_item list) list

(** Group services by their instance group, returning display items with ghosts.
    Returns [(group_display_name, display_items)] pairs where grouped services
    come first (sorted by group name, services within sorted by role),
    followed by ungrouped services in an "Ungrouped" section. *)
val group_by_group :
  groups:Octez_manager_lib.Group.t list ->
  Service_state.t list ->
  (string * display_item list) list

(** Distribute role groups across columns *)
val distribute_to_columns :
  num_columns:int ->
  (string * display_item list) list ->
  (string * display_item list) list array

(** Column item type for rendering *)
type column_item = Header of string | Item of int * display_item

(** Get flat list of items for a column *)
val column_items :
  column_groups:(string * display_item list) list ->
  global_display_items:display_item list ->
  column_item list

(** Get list of service indices in a column *)
val column_service_indices :
  column_groups:(string * display_item list) list ->
  global_display_items:display_item list ->
  int list

(** Compute layout sections based on view_mode *)
val sections_of_state : state -> (string * display_item list) list

(** Get first service index in a column *)
val first_service_in_column :
  num_columns:int ->
  sections:(string * display_item list) list ->
  display_items:display_item list ->
  int ->
  int

(** Get all service indices in a column *)
val services_in_column :
  num_columns:int ->
  sections:(string * display_item list) list ->
  display_items:display_item list ->
  int ->
  int list

(** Find which column contains a service index *)
val column_for_service :
  num_columns:int ->
  sections:(string * display_item list) list ->
  display_items:display_item list ->
  int ->
  int

(** Calculate line position of a service within its column *)
val service_line_position :
  num_columns:int ->
  sections:(string * display_item list) list ->
  display_items:display_item list ->
  folded:StringSet.t ->
  int ->
  int ->
  int * int

(** Adjust column scroll to keep selection visible *)
val adjust_column_scroll :
  column_scroll:int array ->
  col:int ->
  line_start:int ->
  line_count:int ->
  visible_height:int ->
  unit

(** Mutable reference for visible height tracking *)
val last_visible_height_ref : int ref

(** Find first non-empty column *)
val find_non_empty_column :
  num_columns:int ->
  sections:(string * display_item list) list ->
  display_items:display_item list ->
  int option

(** Ensure active column points to a non-empty column *)
val ensure_valid_column : state -> state
