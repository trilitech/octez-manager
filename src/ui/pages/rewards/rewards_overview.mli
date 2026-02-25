(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

(** Overview tab rendering for the Rewards page. *)

(** Render the overview tab content. No I/O — reads from scheduler caches. *)
val render : state:Rewards_state.state -> cols:int -> string
