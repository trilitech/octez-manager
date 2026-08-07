(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Pure rendering functions for the import wizard page. No Eio calls. *)

type step = SelectService | ConfigureImport | ReviewImport | Importing

type state = {
  step : step;
  external_services : Octez_manager_lib.External_service.t list;
  selected_idx : int;
  selected_service : Octez_manager_lib.External_service.t option;
  strategy : Octez_manager_lib.Import.import_strategy;
  custom_name : string option;
  network_override : string option;
  error : string option;
  cascade : bool;
  cascade_chain : Octez_manager_lib.External_service.t list;
  cascade_analysis : Octez_manager_lib.Import_cascade.dependency_analysis option;
}

val header : state -> string list

val view : state -> focus:bool -> size:LTerm_geom.size -> string
