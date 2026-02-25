(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Payout executor: broadcasts transfer operations for a payout blueprint.

    Builds individual [octez-client transfer] commands, broadcasts them
    sequentially, collects operation hashes, and writes reports.
    Supports both real execution and dry-run mode. *)

(** {1 Execution context} *)

(** Parameters needed to execute payouts. *)
type context = {
  octez_client_bin : string;
  endpoint : string;
  base_dir : string option;
  password_file : string option;
  payout_key_alias : string;
  instance : string;
}

(** {1 Callbacks} *)

(** Progress callback invoked after each transfer operation. *)
type progress = {
  current : int;  (** Current operation index (1-based) *)
  total : int;  (** Total operations to execute *)
  delegator : string;  (** Delegator address for current operation *)
  result : Rewards.payout_result;  (** Result of the current transfer *)
}

(** {1 Execution} *)

(** Execute all payouts from a blueprint.

    Iterates over eligible delegator rewards plus bond/fee payouts,
    executes each as an individual [octez-client transfer] operation,
    and writes reports to disk.

    The execution acquires a file lock on the cycle's report directory
    to prevent concurrent payout attempts.

    @param dry_run If true, uses [--dry-run] flag (no real broadcast).
    @param on_progress Optional callback invoked after each operation.
    @return The list of per-delegator results and a summary. *)
val execute :
  ctx:context ->
  blueprint:Rewards.payout_blueprint ->
  ?dry_run:bool ->
  ?on_progress:(progress -> unit) ->
  unit ->
  (Rewards.payout_result list * Rewards.cycle_summary, string) result

(** {1 Wallet balance} *)

(** Fetch the balance of the payout key. Returns mutez. *)
val fetch_wallet_balance : ctx:context -> (Int64.t, string) result
