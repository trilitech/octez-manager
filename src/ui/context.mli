(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

val set_pending_instance_detail : string -> unit

val take_pending_instance_detail : unit -> string option

(** Edit mode context *)
type edit_context = {
  service : Octez_manager_lib.Service.t;
  stopped_dependents : string list;
}

val set_pending_edit_service :
  service:Octez_manager_lib.Service.t -> stopped_dependents:string list -> unit

val take_pending_edit_service : unit -> edit_context option

val has_pending_edit_service : unit -> bool

val mark_instances_dirty : unit -> unit

val consume_instances_dirty : unit -> bool

val navigate : string -> unit

val consume_navigation : unit -> string option

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
