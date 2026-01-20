(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Action handlers for the instances page *)

open Octez_manager_lib
open Instances_state

(** Helper to ensure a service is selected before executing handler *)
val with_service : state -> (Service_state.t -> state) -> state

(** Run a unit action (start/stop/restart) in the background *)
val run_unit_action :
  verb:string ->
  instance:string ->
  (unit -> (unit, Rresult.R.msg) result) ->
  unit

(** Get installer capability *)
val require_installer :
  unit -> ((module Manager_interfaces.Installer), Rresult.R.msg) result

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

(** Start a service *)
val do_start_service :
  instance:string -> role:string -> (unit, Rresult.R.msg) result

(** Offer to start dependent services *)
val offer_start_dependents : instance:string -> unit

(** Start service with dependent cascade *)
val start_with_cascade : instance:string -> role:string -> unit

(** Restart a service *)
val do_restart_service :
  instance:string -> role:string -> (unit, Rresult.R.msg) result

(** Offer to restart dependent services *)
val offer_restart_dependents : instance:string -> unit

(** Restart service with dependent cascade *)
val restart_with_cascade : instance:string -> role:string -> unit

(** Edit an instance configuration *)
val do_edit_instance : Service.t -> unit

(** Confirm edit modal *)
val confirm_edit_modal : Service.t -> unit

(** Show instance actions modal *)
val instance_actions_modal : state -> state

(** Show create/install menu modal *)
val create_menu_modal : state -> state

(** Navigate to diagnostics page *)
val go_to_diagnostics : state -> state

(** Navigate to binaries management page *)
val go_to_binaries : state -> state

(** Activate the current selection (enter key) *)
val activate_selection : state -> state

(** Dismiss failure status for selected instance *)
val dismiss_failure : state -> state
