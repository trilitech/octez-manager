(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Shared helpers for instances action modules.

    These functions are used by multiple action modules
    (lifecycle, external, update, remove). *)

open Octez_manager_lib
open Instances_state

(** Helper to ensure a service is selected before executing handler *)
val with_service : state -> (Service_state.t -> state) -> state

(** Run a unit action (start/stop/restart/remove/purge) in the background.
    Submits to the job manager, shows toasts, and records failures. *)
val run_unit_action :
  verb:string ->
  instance:string ->
  (unit -> (unit, Rresult.R.msg) result) ->
  unit

(** Get installer capability *)
val require_installer :
  unit -> ((module Manager_interfaces.Installer), Rresult.R.msg) result
