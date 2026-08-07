(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** RPC Browser list mode rendering.

    Pure rendering functions for the list/navigation mode of the RPC Browser.
    All functions read from cached state only - no I/O during render. *)

open Octez_manager_lib

(** {1 Path Rendering} *)

(** Render path as breadcrumb navigation string.
    Empty path renders as "/" (root). *)
val render_breadcrumb : string list -> string

(** {1 Instance Rendering} *)

(** Render instance selector showing current target instance.
    @param target Currently active target instance *)
val render_instance_selector : target:Service.t option -> string

(** {1 Entry Rendering} *)

(** Render entry kind badge: [GET], [SUB], or [DYN:type]. *)
val render_entry_kind : Rpc_browser_state.entry_kind -> string

(** Render single entry line with cursor and kind badge.
    @param cursor Current cursor position
    @param idx Entry index
    @param focus Whether the panel has focus (affects cursor highlighting)
    @param entry Entry to render *)
val render_entry :
  cursor:int -> idx:int -> focus:bool -> Rpc_browser_state.entry -> string

(** {1 Status Rendering} *)

(** Render loading spinner with optional message. *)
val render_loading : ?msg:string -> unit -> string

(** Render error message if present. *)
val render_error : string option -> string list

(** {1 Main Rendering} *)

(** Render complete list view from state.
    @param focus Whether the panel has focus (affects cursor highlighting)
    @param state Current RPC Browser state
    @param cols Terminal width (for truncation)
    @return List of rendered lines *)
val render :
  focus:bool -> state:Rpc_browser_state.state -> cols:int -> string list

(** {1 Help Line} *)

(** Render keyboard help line for list mode. *)
val render_help : unit -> string
