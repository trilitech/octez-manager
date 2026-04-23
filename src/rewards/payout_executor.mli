(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Payout executor: broadcasts transfer operations for a payout blueprint.

    Groups transfers into batches using [octez-client multiple transfers],
    collects operation hashes, and writes reports.
    Aborts early after 2 consecutive failed batches.
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

    Groups eligible delegator rewards plus bond/fee payouts into batches
    of [batch_size] and executes each batch as an
    [octez-client multiple transfers] operation.
    Writes reports to disk after all batches complete.

    Returns [Error] immediately if [payout_key_alias] is empty.
    Aborts remaining batches after 2 consecutive batch failures.

    The execution acquires a file lock on the cycle's report directory
    to prevent concurrent payout attempts.

    @param dry_run If true, uses [--dry-run] flag (no real broadcast).
    @param on_progress Optional callback invoked after each transfer.
    @param batch_size Number of transfers per batch (default 80). *)
val execute :
  ctx:context ->
  blueprint:Rewards.payout_blueprint ->
  ?dry_run:bool ->
  ?on_progress:(progress -> unit) ->
  ?batch_size:int ->
  unit ->
  (Rewards.payout_result list * Rewards.cycle_summary, string) result

(** {1 Wallet balance} *)

(** Fetch the balance of the payout key. Returns mutez. *)
val fetch_wallet_balance : ctx:context -> (Int64.t, string) result

(** {1 Multi-cycle payout aggregation} *)

(** Extract payouts from a blueprint as [(delegator, recipient, amount)] tuples.
    
    Includes:
    - Eligible delegator rewards with net_reward > 0
    - Bond payouts with amount > 0
    - Fee payouts with amount > 0
    
    The [recipient] may differ from [delegator] due to redirect overrides. *)
val collect_payouts :
  Rewards.payout_blueprint -> (string * string * Int64.t) list

(** Merge payouts from multiple blueprints by summing amounts per
    [(delegator, recipient)] pair.
    
    Takes a list of payout lists (one per cycle) and returns a single
    merged list where amounts are summed for each unique
    [(delegator, recipient)] pair.
    
    @param payouts List of payout lists from multiple cycles *)
val merge_payouts :
  (string * string * Int64.t) list list -> (string * string * Int64.t) list

(** Execute a merged payout list (not tied to a single blueprint).
    
    Similar to {!execute} but:
    - Takes a flat payout list instead of a blueprint
    - Does NOT write reports (caller handles that)
    - Does NOT create a cycle_summary (caller handles that)
    - Still does batching, progress callbacks, and abort-on-failure logic
    
    @param payouts List of [(delegator, recipient, amount)] tuples
    @param dry_run If true, uses [--dry-run] flag (no real broadcast)
    @param on_progress Optional callback invoked after each transfer
    @param batch_size Number of transfers per batch (default 80) *)
val execute_merged :
  ctx:context ->
  payouts:(string * string * Int64.t) list ->
  ?dry_run:bool ->
  ?on_progress:(progress -> unit) ->
  ?batch_size:int ->
  unit ->
  (Rewards.payout_result list, string) result

(**/**)

module Internal_for_tests : sig
  val extract_op_hash : string -> string option

  val collect_payouts :
    Rewards.payout_blueprint -> (string * string * Int64.t) list
end

(**/**)
