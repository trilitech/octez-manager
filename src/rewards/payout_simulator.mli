(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Payout simulator: runs the full payout pipeline in dry-run mode.

    Simulates all transfers using [octez-client transfer --dry-run],
    collects results, and writes reports to the [dry/] report directory.
    No on-chain transactions are broadcast. *)

(** {1 Simulation result} *)

type simulation_result = {
  results : Rewards.payout_result list;
  summary : Rewards.cycle_summary;
  wallet_balance : Int64.t option;
  total_needed : Int64.t;
  sufficient_balance : bool option;
}

(** {1 Simulation} *)

(** Simulate payout for a cycle.

    Generates a blueprint, checks wallet balance, then runs all transfers
    with [--dry-run]. Reports are saved to [reports/dry/<cycle>/].

    @param on_progress Optional callback invoked after each simulated transfer.
    @return Simulation result with balance sufficiency info. *)
val simulate :
  ctx:Payout_executor.context ->
  blueprint:Rewards.payout_blueprint ->
  ?on_progress:(Payout_executor.progress -> unit) ->
  unit ->
  (simulation_result, string) result
