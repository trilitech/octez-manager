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

val column_separator : string

(** Role ordering for grouping *)
val role_order : string -> int

(** Role section headers *)
val role_header : string -> string

(** Sort services by role then instance name *)
val sort_services : Service_state.t list -> Service_state.t list

(** Load services from Data module *)
val load_services : unit -> Service_state.t list

val load_services_fresh : unit -> Service_state.t list

(** Load external (unmanaged) services *)
val load_external_services : unit -> Octez_manager_lib.External_service.t list

(** Calculate number of columns based on terminal width *)
val calc_num_columns :
  cols:int -> min_column_width:int -> column_separator:string -> int

(** Group services by role *)
val group_by_role : Service_state.t list -> (string * Service_state.t list) list

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
  global_services:Service_state.t list ->
  column_item list

(** Get list of service indices in a column *)
val column_service_indices :
  column_groups:(string * Service_state.t list) list ->
  global_services:Service_state.t list ->
  int list

(** Get first service index in a column *)
val first_service_in_column :
  num_columns:int -> services:Service_state.t list -> int -> int

(** Get all service indices in a column *)
val services_in_column :
  num_columns:int -> services:Service_state.t list -> int -> int list

(** Find which column contains a service index *)
val column_for_service :
  num_columns:int -> services:Service_state.t list -> int -> int

(** Calculate line position of a service within its column *)
val service_line_position :
  num_columns:int ->
  services:Service_state.t list ->
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
  num_columns:int -> services:Service_state.t list -> int option

(** Ensure active column points to a non-empty column *)
val ensure_valid_column : state -> state
