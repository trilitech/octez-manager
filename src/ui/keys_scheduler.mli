(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Background scheduler for key balance and delegation data.

    Polls all visible keys every 30 seconds, fetching balance and delegation
    information from managed nodes. Baker-linked keys use the baker's
    configured endpoint; standalone keys query all managed networks with
    running nodes.

    Usage:
    - Call {!start} once at application startup
    - Call {!get_wallet_data} from render functions (fast, no I/O)
    - Call {!force_refresh} for immediate re-fetch of a specific key
    - Call {!stop} at application shutdown *)

(** Wallet data for a single key on a single network. *)
type wallet_data = {
  pkh : string;
  network : string;
  spendable_balance : string;  (** In mutez *)
  staked_balance : string;  (** In mutez *)
  full_balance : string;  (** In mutez *)
  delegate : string option;  (** Delegate PKH, if delegating *)
  is_registered : bool;  (** Is a registered delegate *)
  active_consensus_key : string option;  (** Current consensus key PKH *)
  delegate_staking_params : Baker_wallet_data.staking_parameters option;
      (** Delegate's active staking parameters, if delegating *)
  delegate_apy : float option;
      (** Estimated annual yield percentage for the delegate *)
  fetched_at : float;  (** Unix timestamp *)
}

(** Get cached wallet data for a key.
    Returns data for all networks where the key has been seen.
    Fast, no I/O — reads from cache. *)
val get_wallet_data : pkh:string -> wallet_data list

(** Request a fetch for a specific PKH. Deduplication and staleness checks
    are handled internally: the request is dropped if the PKH is already
    pending in the worker queue or its cached data is fresh (< 30s old). *)
val request_fetch : pkh:string -> unit

(** Force an immediate re-fetch for a specific key, bypassing staleness.
    The data will be available on the next cache read. *)
val force_refresh : pkh:string -> unit

(** Register keys to be polled. Call when the key list changes. *)
val set_keys : (string * string list) list -> unit

(** Start the background scheduler. Polls every 30 seconds. *)
val start : unit -> unit

(** Stop the background scheduler. *)
val stop : unit -> unit

(** Get running node endpoints for a specific network.
    Returns a list of endpoint URLs. Fast, reads from service state cache. *)
val get_endpoints_for_network : network:string -> string list
