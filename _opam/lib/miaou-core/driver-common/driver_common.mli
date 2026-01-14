(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Common driver utilities shared between terminal and SDL drivers.
    
    This module provides shared logic for:
    - Event loop orchestration
    - Modal rendering and management  
    - Page transition handling
    - Key press routing
*)

open Miaou_core.Tui_page

(** Modal rendering utilities *)
module Modal_utils : sig
  (** [render_with_modal_overlay ~view ~rows ~cols] renders the base view
      with a modal overlay if one is active. Returns the original view if
      no modal is active. *)
  val render_with_modal_overlay : view:string -> rows:int -> cols:int -> string
end

(** Page transition utilities *)
module Page_transition_utils : module type of Page_transition_utils

(** Pager notification utilities for debounced background updates *)
module Pager_notify : module type of Pager_notify

(** Signature for backend-specific driver implementations *)
module type DRIVER_BACKEND = sig
  type size = {rows : int; cols : int}

  type event = Quit | Refresh | Key of string

  (** Poll for the next event. Should block until an event is available. *)
  val poll_event : unit -> event

  (** Render the given view to the display *)
  val render : view:string -> size:size -> unit

  (** Get the current display size *)
  val detect_size : unit -> size

  (** Initialize the backend *)
  val init : unit -> unit

  (** Cleanup backend resources *)
  val cleanup : unit -> unit
end

(** Functor to create a complete driver from a backend implementation *)
module Make (Backend : DRIVER_BACKEND) : sig
  (** Run the application with the given initial page *)
  val run : (module PAGE_SIG) -> [`Quit | `SwitchTo of string]
end
