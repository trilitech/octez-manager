(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Pure rendering functions for the snapshots page. No Eio calls. *)

type state = {
  network : string;
  entries : Octez_manager_lib.Snapshots.entry list;
  selected : int;
  error : string option;
}

val header : state -> string list

val view : state -> focus:bool -> size:LTerm_geom.size -> string
