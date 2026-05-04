(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Pure rendering functions for the sandbox management page.
    No Eio calls. All inputs are pre-computed by the page module. *)

open Octez_manager_lib

type node_info = {
  svc : Service.t;
  state : Data.Service_state.t option;
  peers : string list;
}

type baker_info = {
  svc : Service.t;
  state : Data.Service_state.t option;
  delegate_count : int;
  baker_ck_aliases : string list;
}

type accuser_info = {svc : Service.t; state : Data.Service_state.t option}

type sandbox_info = {
  group : Group.t;
  nodes : node_info list;
  bakers : baker_info list;
  accusers : accuser_info list;
}

type state = {sandboxes : sandbox_info list; cursor : int}

(** Render the full sandbox page view.

    @param toast pre-rendered toast string (may be empty).
    @param node_metrics association list: instance name -> metrics option.
    @param stake_pct_for function mapping group name to cached stake percentage.
    No Context, no Eio, no Data I/O calls are made inside this function. *)
val key_hint_pairs : (string * string) list

val view :
  state ->
  toast:string ->
  node_metrics:(string * Rpc_metrics.rpc_metrics option) list ->
  stake_pct_for:(string -> float option) ->
  focus:bool ->
  size:LTerm_geom.size ->
  string
