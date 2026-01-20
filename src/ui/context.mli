(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

val set_pending_instance_detail : string -> unit

val take_pending_instance_detail : unit -> string option

(** Set pending external service for details/logs viewing *)
val set_pending_external_service : Octez_manager_lib.External_service.t -> unit

(** Take pending external service (clears after retrieval) *)
val take_pending_external_service :
  unit -> Octez_manager_lib.External_service.t option

(** Edit mode context *)
type edit_context = {
  service : Octez_manager_lib.Service.t;
  stopped_dependents : string list;
}

val set_pending_edit_service :
  service:Octez_manager_lib.Service.t -> stopped_dependents:string list -> unit

val take_pending_edit_service : unit -> edit_context option

val has_pending_edit_service : unit -> bool

(** Pending restart: dependents stopped during edit that need restart *)
val set_pending_restart_dependents : string list -> unit

val take_pending_restart_dependents : unit -> string list

val mark_instances_dirty : unit -> unit

val consume_instances_dirty : unit -> bool

val navigate : string -> unit

val consume_navigation : unit -> string option

(** Skip navigation back once - used when form exits to prevent back-navigation loop *)
val set_skip_back_once : unit -> unit

val consume_skip_back_once : unit -> bool

(** Toast notifications *)
val toast_info : string -> unit

val toast_success : string -> unit

val toast_warn : string -> unit

val toast_error : string -> unit

val tick_toasts : unit -> unit

val render_toasts : cols:int -> string

(** Spinner for loading states *)
val tick_spinner : unit -> unit

val render_spinner : string -> string

(** Progress bar for long-running tasks *)
val progress_start : label:string -> estimate_secs:float -> width:int -> unit

val progress_finish : unit -> unit

val progress_set : ?label:string -> progress:float -> unit -> unit

val render_progress : cols:int -> string
