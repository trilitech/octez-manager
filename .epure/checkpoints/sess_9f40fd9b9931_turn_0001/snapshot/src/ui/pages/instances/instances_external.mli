(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Actions for external (unmanaged) services.

    Handles interaction with services detected on the system that are not
    managed by octez-manager. Provides details viewing, import guidance,
    and basic systemd lifecycle actions. *)

open Octez_manager_lib
open Instances_state

(** Get the currently selected external service from state, if any. *)
val current_external_service : state -> External_service.t option

(** Show actions modal for an external service.
    For standalone processes: shows Details and Import options.
    For systemd services: shows Details, Import, Start, Stop, Restart, Logs. *)
val external_service_actions_modal : state -> External_service.t -> state
