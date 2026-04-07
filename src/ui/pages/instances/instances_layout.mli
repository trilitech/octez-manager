(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Layout and column calculation for instances page *)

module Service_state = Data.Service_state
open Instances_state
module StringMap = Instances_state.StringMap

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

(** Group services by role *)
val group_by_role : Service_state.t list -> (string * Service_state.t list) list

(** Group services by their instance group.
    Returns [(group_display_name, services)] pairs where grouped services
    come first (sorted by group name, services within sorted by role),
    followed by ungrouped services in an "Ungrouped" section. *)
val group_by_group :
  groups:Octez_manager_lib.Group.t list ->
  Service_state.t list ->
  (string * Service_state.t list) list

(** Distribute role groups across columns *)
val distribute_to_columns :
  num_columns:int ->
  (string * Service_state.t list) list ->
  (string * Service_state.t list) list array

(** Column item type for rendering *)
type column_item = Header of string | Instance of int * Service_state.t

(** Get flat list of items for a column *)
val column_items :
  column_groups:(string * Service_state.t list) list ->
  index_by_instance:int StringMap.t ->
  column_item list

(** Get list of service indices in a column *)
val column_service_indices :
  column_groups:(string * Service_state.t list) list ->
  index_by_instance:int StringMap.t ->
  int list

(** Compute layout sections based on view_mode *)
val sections_of_state : state -> (string * Service_state.t list) list

(** Get first service index in a column *)
val first_service_in_column :
  num_columns:int ->
  sections:(string * Service_state.t list) list ->
  services:Service_state.t list ->
  index_by_instance:int StringMap.t ->
  int ->
  int

(** Get all service indices in a column *)
val services_in_column :
  num_columns:int ->
  sections:(string * Service_state.t list) list ->
  services:Service_state.t list ->
  index_by_instance:int StringMap.t ->
  int ->
  int list

(** Find which column contains a service index *)
val column_for_service :
  num_columns:int ->
  sections:(string * Service_state.t list) list ->
  services:Service_state.t list ->
  index_by_instance:int StringMap.t ->
  int ->
  int

(** Calculate line position of a service within its column *)
val service_line_position :
  num_columns:int ->
  sections:(string * Service_state.t list) list ->
  index_by_instance:int StringMap.t ->
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
  sections:(string * Service_state.t list) list ->
  services:Service_state.t list ->
  index_by_instance:int StringMap.t ->
  int option

(** Ensure active column points to a non-empty column *)
val ensure_valid_column : state -> state
