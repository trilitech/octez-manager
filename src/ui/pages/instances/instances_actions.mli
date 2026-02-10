(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Action handlers for the instances page *)

open Octez_manager_lib
open Instances_state

(** Remove a service *)
val do_remove :
  instance:string ->
  delete_data_dir:bool ->
  unit ->
  (unit, Rresult.R.msg) result

(** Purge a service (remove + delete data) *)
val do_purge : instance:string -> unit -> (unit, Rresult.R.msg) result

(** Confirm removal with dependent services *)
val remove_with_dependents_confirm :
  instance:string -> dependents:string list -> delete_data_dir:bool -> unit

(** Confirm purge with dependent services *)
val purge_with_dependents_confirm :
  instance:string -> dependents:string list -> unit

(** Show remove/purge modal *)
val remove_modal : state -> state

(** Build journalctl arguments for a unit *)
val journalctl_args : string -> string list

(** Legacy log viewer (commented out) *)
val _view_logs_old : state -> state

(** Show instance actions modal *)
val instance_actions_modal : state -> state

(** Show create/install menu modal *)
val create_menu_modal : state -> state

(** Navigate to diagnostics page *)
val go_to_diagnostics : state -> state

(** Navigate to network topology page *)
val go_to_topology : state -> state

(** Navigate to binaries management page *)
val go_to_binaries : state -> state

(** Navigate to RPC browser page *)
val go_to_rpc_browser : state -> state

(** Activate the current selection (enter key) *)
val activate_selection : state -> state

(** Dismiss failure status for selected instance *)
val dismiss_failure : state -> state

(** Functions exposed for testing. *)
module For_tests : sig
  (** Build journalctl arguments for a unit. *)
  val journalctl_args : string -> string list

  (** Get current external service from state. *)
  val current_external_service : state -> External_service.t option
end
