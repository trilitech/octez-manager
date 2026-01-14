(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Global render notification system for widgets.

    This module provides a simple mechanism for widgets to request a UI
    re-render when they need to update asynchronously (e.g., after a
    debounce delay, animation frame, or background task completion).

    {2 For Widgets}

    Call {!request_render} when you need the UI to refresh:
    {[
      (* After setting up a delayed validation *)
      Render_notify.request_render ()
    ]}

    {2 For Drivers}

    Check {!should_render} in your event loop and perform a render if needed:
    {[
      if Render_notify.should_render () then
        perform_render ()
    ]}
*)

(** Request a UI re-render.

    This is a non-blocking call that sets a flag indicating the UI should
    refresh. Multiple calls before the next render are coalesced.

    Thread-safe: can be called from any thread or fiber. *)
val request_render : unit -> unit

(** Check if a render was requested and clear the flag.

    Returns [true] if {!request_render} was called since the last check.
    The flag is automatically cleared, so subsequent calls return [false]
    until another {!request_render} is made.

    Thread-safe: can be called from any thread. *)
val should_render : unit -> bool

(** Clear any pending render request without performing the render.

    Useful for cleanup or when switching pages. *)
val clear : unit -> unit
