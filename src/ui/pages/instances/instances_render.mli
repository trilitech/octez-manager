(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Rendering functions for the instances page *)

open Octez_manager_lib
open Instances_state

(** Render status icon for a service *)
val status_icon : Service_state.t -> string

(** Render enabled badge for a service *)
val enabled_badge : Service_state.t -> string

(** Render RPC status line *)
val rpc_status_line : service_status:Service_state.status -> Service.t -> string

(** Shorten network name for display *)
val network_short : string -> string

(** Render a single service line *)
val line_for_service : int -> int -> folded:bool -> Service_state.t -> string

(** Role header for grouping *)
val role_header : string -> string

(** Truncate string to visible width *)
val truncate_visible : max_width:int -> string -> string

(** Pad line to column width *)
val pad_line : col_width:int -> string -> string

(** Render a single column *)
val render_column :
  col_width:int ->
  state:state ->
  column_groups:(string * Service_state.t list) list ->
  string list

(** Dim inactive column content *)
val dim_inactive_column : string -> string

(** Merge multiple columns into rows *)
val merge_columns :
  col_width:int ->
  visible_height:int ->
  column_scroll:int array ->
  active_column:int ->
  columns_content:string list array ->
  string list

(** Render table in single column mode *)
val table_lines_single : state -> string list

(** Render table in matrix/multi-column mode *)
val table_lines_matrix :
  cols:int ->
  visible_height:int ->
  column_scroll:int array ->
  state ->
  string list

(** Main table rendering function *)
val table_lines : ?cols:int -> ?visible_height:int -> state -> string list

(** Render external service with fold/unfold and selection *)
val render_external_service :
  selected_idx:int ->
  current_idx:int ->
  folded:bool ->
  Octez_manager_lib.External_service.t ->
  string list

(** Render external services section *)
val render_external_services_section : state -> string list

(** Render summary line showing total instances *)
val summary_line : state -> string
