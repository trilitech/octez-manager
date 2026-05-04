(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Service lifecycle actions: start, restart, and edit with cascade logic.

    Handles starting and restarting services with dependency awareness:
    - Checks for stopped dependencies before starting/restarting
    - Offers to start/restart dependent services after the main service
    - Provides edit navigation with dependent-stop confirmation *)

open Octez_manager_lib

(** Start a single service.
    @return [Ok ()] on success, [Error] with message on failure *)
val do_start_service :
  instance:string -> role:string -> (unit, Rresult.R.msg) result

(** Offer to start stopped dependents after starting a service.
    Shows a modal if there are stopped dependents, allowing the user
    to start them all at once or dismiss. *)
val offer_start_dependents : instance:string -> unit

(** Start a service with dependency cascade.
    If dependencies are stopped, offers to start them first.
    After starting, offers to start stopped dependents. *)
val start_with_cascade : instance:string -> role:string -> unit

(** Restart a single service.
    @return [Ok ()] on success, [Error] with message on failure *)
val do_restart_service :
  instance:string -> role:string -> (unit, Rresult.R.msg) result

(** Offer to restart all dependents after restarting a service.
    Unlike {!offer_start_dependents}, this shows ALL dependents (not just
    stopped ones) because running dependents need to reconnect. *)
val offer_restart_dependents : instance:string -> unit

(** Restart a service with dependency cascade.
    If dependencies are stopped, offers to start them first.
    After restarting, offers to restart all dependents. *)
val restart_with_cascade : instance:string -> role:string -> unit

(** Navigate to the edit form for a service.
    Sets up the edit context and navigates to the appropriate install form
    based on the service role. *)
val do_edit_instance : Service.t -> unit

(** Show edit confirmation modal if service has dependents.
    If no dependents, navigates directly to the edit form. *)
val confirm_edit_modal : Service.t -> unit
