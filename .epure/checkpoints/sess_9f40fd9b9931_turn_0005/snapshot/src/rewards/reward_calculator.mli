(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Reward calculation engine.

    Computes per-delegator reward breakdown from cycle data and
    payout configuration. Handles fee application, overdelegation
    protection, eligibility checks, and below-minimum redistribution. *)

(** Generate a payout blueprint for a cycle.

    Given cycle rewards data and a payout configuration, computes
    the proportional share for each delegator, applies fees and
    eligibility filters, and produces a complete payout preview. *)
val generate_blueprint :
  config:Payout_config.t ->
  network:string ->
  cycle_rewards:Rewards.cycle_rewards ->
  Rewards.payout_blueprint
