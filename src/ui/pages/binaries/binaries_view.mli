(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

(** Rendering functions for the binaries management page. *)

open Binaries_types

(** Main view function for the binaries page. Assembles all sections. *)
val view : pstate -> focus:bool -> size:LTerm_geom.size -> string
