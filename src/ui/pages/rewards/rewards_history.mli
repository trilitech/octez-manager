(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

(** History tab rendering for the Rewards page. *)

(** Number of cycles available in the history data. *)
val cycle_count : Octez_manager_rewards.Rewards.cycle_rewards list -> int

(** Render the history tab content. No I/O — reads from scheduler caches. *)
val render : state:Rewards_state.state -> cols:int -> rows:int -> string
