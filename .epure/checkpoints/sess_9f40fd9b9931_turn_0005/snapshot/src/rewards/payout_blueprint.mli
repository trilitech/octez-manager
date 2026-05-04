(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Payout blueprint orchestrator.

    Coordinates data fetching, config loading, and reward calculation
    to produce a complete payout preview. *)

(** {1 Blueprint generation} *)

(** Generate a payout blueprint for a specific cycle.

    Fetches cycle data from TzKT (using the config's [tzkt_url]),
    loads the baker's payout config, and runs the reward calculator.

    Returns [Error] if the cycle has already been paid, unless
    [~force:true] is passed. *)
val generate :
  instance:string ->
  baker:string ->
  network:string ->
  cycle:int ->
  ?force:bool ->
  unit ->
  (Rewards.payout_blueprint, string) result

(** Generate a payout blueprint from pre-loaded data.

    Same as {!generate} but uses the provided config and cycle data
    instead of fetching from disk/network. Useful when data is already
    cached (e.g., in the TUI scheduler). *)
val generate_from_data :
  config:Payout_config.t ->
  network:string ->
  cycle_rewards:Rewards.cycle_rewards ->
  instance:string ->
  ?force:bool ->
  unit ->
  (Rewards.payout_blueprint, string) result

(** {1 Double-payment prevention} *)

(** Check whether a cycle has already been paid for an instance. *)
val is_already_paid : instance:string -> cycle:int -> bool
