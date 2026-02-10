(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

(** Store an instance name to navigate to the instance details page. *)
val set_pending_instance_detail : string -> unit

(** Consume the pending instance name (returns [Some] once, then [None]). *)
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

(** Store a service and its stopped dependents for the edit wizard. *)
val set_pending_edit_service :
  service:Octez_manager_lib.Service.t -> stopped_dependents:string list -> unit

(** Consume the pending edit context (returns [Some] once, then [None]). *)
val take_pending_edit_service : unit -> edit_context option

(** Return [true] if an edit context is pending. *)
val has_pending_edit_service : unit -> bool

(** Pending restart: dependents stopped during edit that need restart *)
val set_pending_restart_dependents : string list -> unit

(** Consume the list of dependents that need restarting after an edit completes. *)
val take_pending_restart_dependents : unit -> string list

(** Mark the instances list as needing a refresh (thread-safe via [Atomic]). *)
val mark_instances_dirty : unit -> unit

(** Consume and clear the dirty flag. Returns [true] if it was set. *)
val consume_instances_dirty : unit -> bool

(** Request navigation to the named page on the next tick. *)
val navigate : string -> unit

(** Request navigation to the instances home page on the next tick. *)
val navigate_instances : unit -> unit

(** Request back navigation on the next tick. *)
val navigate_back : unit -> unit

(** Request app quit on the next tick. *)
val navigate_quit : unit -> unit

(** Typed pending navigation request consumed by pages. *)
type pending_navigation = Goto of string | Back | Quit

(** Consume the pending navigation target (returns [Some] once, then [None]). *)
val consume_navigation : unit -> pending_navigation option

(** Toast notifications *)
val toast_info : string -> unit

(** Show a green success toast message. *)
val toast_success : string -> unit

(** Show a yellow warning toast message. *)
val toast_warn : string -> unit

(** Show a red error toast message. *)
val toast_error : string -> unit

(** Advance toast timers, expiring old messages. Called once per tick. *)
val tick_toasts : unit -> unit

(** Render all active toast messages into a string for display.
    @param cols Terminal width for alignment. *)
val render_toasts : cols:int -> string

(** Spinner for loading states *)
val tick_spinner : unit -> unit

(** Render a spinning animation next to [label].
    Advances on each call to {!tick_spinner}. *)
val render_spinner : string -> string

(** Progress bar for long-running tasks *)
val progress_start : label:string -> estimate_secs:float -> width:int -> unit

(** Mark the current progress bar as complete. *)
val progress_finish : unit -> unit

(** Update the progress bar position and optional label.
    @param progress A float between [0.0] and [1.0]. *)
val progress_set : ?label:string -> progress:float -> unit -> unit

(** Render the progress bar into a string for display.
    @param cols Terminal width for sizing. *)
val render_progress : cols:int -> string

(** Multi-file progress for binary downloads *)

(** Start multi-file progress display
    @param version Version being downloaded
    @param binaries List of binary names to track *)
val multi_progress_start : version:string -> binaries:string list -> unit

(** Update progress for a specific binary
    @param binary Binary name
    @param downloaded Bytes downloaded
    @param total Total file size (if known) *)
val multi_progress_update :
  binary:string -> downloaded:int64 -> total:int64 option -> unit

(** Mark a binary as complete
    @param binary Binary name
    @param size Final file size *)
val multi_progress_complete : binary:string -> size:int64 -> unit

(** Set checksum verification message *)
val multi_progress_checksum : string -> unit

(** Finish multi-progress (will linger for a few seconds before clearing) *)
val multi_progress_finish : unit -> unit

(** Render multi-progress display (returns multi-line string) *)
val render_multi_progress : cols:int -> string
