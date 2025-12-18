(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Delegate data fetching and caching.

    Fetches delegate participation data from node RPC and caches it
    for use by baker, DAL node, and other components. *)

(** Participation info from RPC *)
type participation = {
  expected_cycle_activity : int;
  minimal_cycle_activity : int;
  missed_slots : int;
  missed_levels : int;
  remaining_allowed_missed_slots : int;
  expected_attesting_rewards : string;
}

(** DAL participation info *)
type dal_participation = {
  expected_assigned_shards_per_slot : int;
  delegate_attested_dal_slots : int;
  delegate_attestable_dal_slots : int;
  expected_dal_rewards : string;
  sufficient_dal_participation : bool;
  denounced : bool;
}

(** Core delegate data *)
type t = {
  pkh : string;
  deactivated : bool;
  is_forbidden : bool;
  participation : participation;
  dal_participation : dal_participation;
  baking_power : string;
  total_staked : string;
  total_delegated : string;
  own_full_balance : string;
  fetched_at : float;
}

(** {2 Fetching} *)

(** Fetch delegate data from node RPC.
    Returns [None] if the request fails or parsing fails. *)
val fetch : node_endpoint:string -> pkh:string -> t option

(** {2 Caching} *)

(** Get cached delegate data by public key hash. *)
val get : pkh:string -> t option

(** Store delegate data in cache. *)
val set : t -> unit

(** Get all cached delegates. *)
val get_all : unit -> t list

(** Clear all cached data. *)
val clear : unit -> unit

(** Check if data is stale (older than max_age seconds). *)
val is_stale : max_age:float -> t -> bool

(** {2 Status Helpers} *)

(** Missed slots status *)
type missed_status =
  | Good        (** No missed slots *)
  | Warning     (** Missed >= remaining/2 *)
  | Critical    (** Missed > remaining *)

(** Get missed slots status based on participation data. *)
val missed_slots_status : t -> missed_status

(** Format mutez amount as tez with K/M suffix. *)
val format_tez : string -> string
