(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Pure rendering functions for the topology page. No Eio calls. *)

type state = {services : Data.Service_state.t list}

val view : state -> focus:bool -> size:LTerm_geom.size -> string
