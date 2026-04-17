(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Import wizard for importing existing Octez services. *)

open Octez_manager_lib
module Navigation = Miaou.Core.Navigation

(** Steps in the import wizard flow. *)
type step =
  | SelectService  (** Step 1: choose an external service to import. *)
  | ConfigureImport  (** Step 2: configure import strategy and options. *)
  | ReviewImport  (** Step 3: review and confirm the import. *)
  | Importing  (** Terminal state while the import job runs. *)

(** Wizard state tracking the current step, selected service, and options. *)
type state = {
  step : step;  (** Current wizard step. *)
  external_services : External_service.t list;
      (** Detected external Octez services. *)
  selected_idx : int;  (** Index of the highlighted service in step 1. *)
  selected_service : External_service.t option;
      (** The service chosen for import. *)
  strategy : Import.import_strategy;
      (** Import strategy ([Takeover] or [Clone]). *)
  custom_name : string option;  (** Optional custom instance name override. *)
  network_override : string option;  (** Optional network name override. *)
  error : string option;  (** Error message from the last operation. *)
  cascade : bool;  (** Whether to import dependencies/dependents. *)
  cascade_chain : External_service.t list;
      (** Services to import when cascade is enabled. *)
  cascade_analysis : Import_cascade.dependency_analysis option;
      (** Dependency analysis for cascade import preview. *)
}

(** Navigation-wrapped state. *)
type pstate = state Navigation.t

(** Page name for the page registry. *)
val name : string

(** Pre-built page value for registration. *)
val page : Miaou.Core.Registry.page

(** Register this page with the global page registry. *)
val register : unit -> unit

(** Page implementation satisfying the Miaou TUI page signature. *)
module Page : Miaou.Core.Tui_page.PAGE_SIG

(** Move the service selection cursor by [delta] positions (wraps around). *)
val move_selection : pstate -> int -> pstate

(** Toggle the import strategy between [Takeover] and [Clone]. *)
val toggle_strategy : pstate -> pstate

(** Toggle cascade import on/off and recompute dependency chain. *)
val toggle_cascade : pstate -> pstate

(** Return the header lines for the current wizard step. *)
val header : state -> string list

(** Return the list of key events this page handles. *)
val handled_keys : unit -> Miaou.Core.Keys.t list

(** Return the keymap description for the help bar. *)
val keymap : state -> state Miaou.Core.Tui_page.key_binding_desc list
