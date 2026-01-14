(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)
(** Simple shared Eio runtime helpers.

    The application must call {!init} once from inside [Eio_main.run] to
    provide the standard environment and a long-lived switch. *)

val init : env:Eio_unix.Stdenv.base -> sw:Eio.Switch.t -> unit

val env_opt : unit -> Eio_unix.Stdenv.base option

val switch_opt : unit -> Eio.Switch.t option

val page_switch_opt : unit -> Eio.Switch.t option

(** @raise Invalid_argument if the runtime is not initialized *)
val require_runtime : unit -> Eio_unix.Stdenv.base * Eio.Switch.t

val require_env_and_switch : unit -> Eio_unix.Stdenv.base * Eio.Switch.t

val require_current_switch : unit -> Eio.Switch.t

(** Run [f] inside [with_page_switch] when a runtime is active; otherwise run
    [f] directly. Useful for drivers that may be constructed before the runtime
    is initialized but should still scope fibers when possible. *)
val with_page_scope : (unit -> 'a) -> 'a

(** Run [f] inside a fresh page-scoped switch.
    All fibers spawned via {!spawn} (or other helpers using {!require_current_switch})
    while [f] executes are attached to that switch and are cancelled when it closes.
    Nested calls replace the current page switch until they return. *)
val with_page_switch : (Eio_unix.Stdenv.base -> Eio.Switch.t -> 'a) -> 'a

val with_env : (Eio_unix.Stdenv.base -> 'a) -> 'a

(** Spawn a fiber on the current page switch if one is active, otherwise on the
    long-lived runtime switch. *)
val spawn : (Eio_unix.Stdenv.base -> unit) -> unit

val sleep : float -> unit

(** Check if shutdown has been requested. Fibers should check this periodically. *)
val is_shutdown : unit -> bool

(** Signal all fibers to stop. Call this before exiting the application. *)
val shutdown : unit -> unit
