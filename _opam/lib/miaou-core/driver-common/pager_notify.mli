(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Debounced notification system for background widget updates.

    This module provides a thread-safe notification mechanism that prevents
    excessive re-rendering when background threads (e.g., pager appenders)
    request UI updates. Notifications are debounced to coalesce bursts of
    updates into a single render cycle.

    Typical usage:
    {[
      (* Create notifier with 80ms debounce *)
      let notifier = Pager_notify.create ~debounce_s:0.08 () in

      (* Background thread requests render *)
      Pager_notify.notify notifier ;

      (* Main loop checks if enough time has passed *)
      if Pager_notify.should_refresh notifier then (
        (* Perform render *)
        Pager_notify.mark_refreshed notifier ;
        ...
      )
    ]}
*)

(** Opaque notifier state *)
type t

(** Create a new pager notifier.

    @param debounce_s Minimum time in seconds between refreshes (default: 0.08)
*)
val create : ?debounce_s:float -> unit -> t

(** Request a render from a background thread.

    Sets the notification timestamp to the current time. Multiple calls
    within the debounce window are coalesced into a single refresh.

    Thread-safe: can be called from any thread.
*)
val notify : t -> unit

(** Check if a refresh should be performed.

    Returns [true] if:
    - A notification was received AND
    - Enough time has passed since the last notification (>= debounce_s)

    This does NOT consume the notification - call {!mark_refreshed} after
    performing the actual render.

    Thread-safe: can be called from any thread.
*)
val should_refresh : t -> bool

(** Mark that a refresh was performed.

    Resets the notification timestamp to 0, indicating that the pending
    notification has been serviced.

    Thread-safe: can be called from any thread.
*)
val mark_refreshed : t -> unit

(** Get the current debounce interval in seconds. *)
val get_debounce : t -> float
