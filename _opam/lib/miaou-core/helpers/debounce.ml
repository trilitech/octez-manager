(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Generic debounce timer for deferring operations during rapid input.

    Usage pattern:
    1. Call [mark] when an event occurs that should trigger a deferred action
    2. Call [is_ready] periodically (e.g., in service_cycle) to check if the
       debounce period has elapsed
    3. When [is_ready] returns true, perform the deferred action and call [clear]

    Thread-safe: uses Atomic operations for concurrent access. *)

type t = {
  last_event_time : float Atomic.t;
  pending : bool Atomic.t;
  debounce_ms : int;
}

let create ?(debounce_ms = 250) () =
  {last_event_time = Atomic.make 0.0; pending = Atomic.make false; debounce_ms}

(** Mark that an event occurred. Resets the debounce timer. *)
let mark t =
  Atomic.set t.last_event_time (Unix.gettimeofday ()) ;
  Atomic.set t.pending true

(** Check if the debounce period has elapsed since the last event.
    Returns true if there's a pending event and enough time has passed. *)
let is_ready t =
  if not (Atomic.get t.pending) then false
  else
    let now = Unix.gettimeofday () in
    let last = Atomic.get t.last_event_time in
    let elapsed_ms = (now -. last) *. 1000.0 in
    elapsed_ms >= float_of_int t.debounce_ms

(** Clear the pending state after handling the debounced event. *)
let clear t = Atomic.set t.pending false

(** Check if there's a pending event (regardless of whether debounce elapsed). *)
let has_pending t = Atomic.get t.pending

(** Get the configured debounce period in milliseconds. *)
let debounce_ms t = t.debounce_ms

(** Convenience: check if ready and automatically clear if so.
    Returns true if the action should be performed now. *)
let check_and_clear t =
  if is_ready t then (
    clear t ;
    true)
  else false
