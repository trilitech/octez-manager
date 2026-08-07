(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Baker wallet data fetching and caching.

    Fetches wallet state (balances, staking parameters, unstake requests,
    consensus key, voting info) from node RPC and caches it for use by
    the TUI wallet modal and CLI baker commands. *)

(** {2 Types} *)

(** Staking policy parameters.
    Values come from the protocol in millionths / billionths. *)
type staking_parameters = {
  limit_of_staking_over_baking : int;
      (** In millionths. 0 = reject all external staking, max 9000000. *)
  edge_of_baking_over_staking : int;
      (** In billionths. 1000000000 = 100% of staker rewards to baker. *)
}

(** A single unstake request that is ready to be finalized. *)
type finalizable_request = {
  cycle : int;  (** Cycle when unstake was requested *)
  amount : string;  (** Amount in mutez *)
}

(** A single unstake request that is still frozen. *)
type unfinalizable_request = {
  cycle : int;  (** Cycle when unstake was requested *)
  amount : string;  (** Amount in mutez *)
}

(** Pending unstake request details. *)
type unstake_requests = {
  finalizable : finalizable_request list;  (** Requests ready to finalize *)
  unfinalizable : unfinalizable_request list;  (** Requests still frozen *)
}

(** Voting period kind. *)
type voting_period_kind =
  | Proposal
  | Exploration
  | Cooldown
  | Promotion
  | Adoption

(** Ballot vote choice. *)
type ballot_vote = Yay | Nay | Pass

(** Voting information, shared per-node (not per-delegate). *)
type voting_info = {
  period_kind : voting_period_kind;
  period_position : int;
  period_remaining : int;
  proposals : (string * int) list;
      (** (protocol_hash, supporter_count) pairs *)
  current_proposal : string option;  (** Protocol under evaluation *)
  ballots : (string * string) list;  (** (pkh, ballot) pairs from ballot_list *)
}

(** Complete wallet state for a single delegate address. *)
type t = {
  pkh : string;
  spendable_balance : string;  (** Mutez *)
  staked_balance : string;  (** Mutez *)
  unstaked_frozen : string;  (** Mutez *)
  full_balance : string;  (** Mutez *)
  is_registered : bool;
  deactivated : bool;
  active_consensus_key : string;
  pending_consensus_keys : (int * string) list;  (** (cycle, pkh) pairs *)
  staking_parameters : staking_parameters option;
  pending_staking_parameters : staking_parameters option;
  unstake_requests : unstake_requests;
  fetched_at : float;
}

(** {2 JSON Parsing} *)

(** Parse staking parameters from a JSON response.
    Expects fields [limit_of_staking_over_baking_millionth] and
    [edge_of_baking_over_staking_billionth]. *)
val parse_staking_parameters : Yojson.Safe.t -> staking_parameters option

(** {2 Fetching} *)

(** Fetch wallet data from node RPC for a delegate address.
    Returns [None] if the node is unreachable or parsing fails.
    @param node_endpoint  Base URL of the Tezos node RPC
    @param pkh            Public key hash of the delegate *)
val fetch_wallet_data : node_endpoint:string -> pkh:string -> t option

(** Fetch voting info from node RPC.
    Returns [None] if the node is unreachable or parsing fails.
    Voting info is shared per-node (not per-delegate).
    @param node_endpoint  Base URL of the Tezos node RPC *)
val fetch_voting_info : node_endpoint:string -> voting_info option

(** {2 Wallet Data Cache} *)

(** Get cached wallet data by public key hash. *)
val get : pkh:string -> t option

(** Store wallet data in cache. *)
val set : t -> unit

(** Get all cached wallet data entries. *)
val get_all : unit -> t list

(** Remove cached wallet data for a specific delegate.
    Use after a wallet operation to force a fresh fetch on the next
    scheduler poll. *)
val remove : pkh:string -> unit

(** Clear all cached wallet data. *)
val clear : unit -> unit

(** Check if wallet data is stale (older than [max_age] seconds). *)
val is_stale : max_age:float -> t -> bool

(** {2 Voting Info Cache} *)

(** Get cached voting info by node endpoint. *)
val get_voting_info : node_endpoint:string -> voting_info option

(** Store voting info in cache. *)
val set_voting_info : node_endpoint:string -> voting_info -> unit

(** {2 Format Helpers} *)

(** Format a mutez string as tez with full precision.
    Example: ["1234567890"] -> ["1,234.567890 ꜩ"] *)
val format_tez : string -> string

(** Format staking limit from millionths to human-readable ratio.
    Example: [5000000] -> ["5.0x"] *)
val format_staking_limit : int -> string

(** Format baking edge from billionths to percentage.
    Example: [100000000] -> ["10.0%"] *)
val format_baking_edge : int -> string

(** Format a ballot vote as a lowercase string.
    Example: [Yay] -> ["yay"] *)
val string_of_ballot_vote : ballot_vote -> string

(** Format a voting period kind as a lowercase string.
    Example: [Proposal] -> ["proposal"] *)
val string_of_voting_period_kind : voting_period_kind -> string

(** {2 Testing} *)

(**/**)

module For_tests : sig
  val parse_delegate_aggregate : pkh:string -> Yojson.Safe.t -> t option

  val parse_spendable : Yojson.Safe.t -> string option

  val parse_staking_parameters : Yojson.Safe.t -> staking_parameters option

  val parse_unstake_requests : Yojson.Safe.t -> unstake_requests

  val parse_voting_info :
    period_json:Yojson.Safe.t ->
    proposals_json:Yojson.Safe.t ->
    ballot_list_json:Yojson.Safe.t ->
    current_proposal_json:Yojson.Safe.t ->
    voting_info option
end

(**/**)
