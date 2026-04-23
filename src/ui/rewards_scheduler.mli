(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Background scheduler for polling baker reward data from TzKT.

    Polls cycle rewards data every 60 seconds and caches results
    in a [Hashtbl] + [Mutex] store for safe access from the render loop.

    All public [get_*] functions are safe to call from view functions
    (no I/O, no blocking). *)

(** {1 Lifecycle} *)

(** Start the background polling loop. Idempotent. *)
val start : unit -> unit

(** Request a graceful shutdown of the polling loop. *)
val shutdown : unit -> unit

(** Clear all cached data. *)
val clear : unit -> unit

(** {1 Cache accessors (no I/O)} *)

(** Get cached cycle rewards for a specific baker and cycle. *)
val get_cycle_data :
  baker:string ->
  cycle:int ->
  Octez_manager_rewards.Rewards.cycle_rewards option

(** Get cached recent cycles for a baker (sorted descending by cycle). *)
val get_recent_cycles :
  baker:string -> Octez_manager_rewards.Rewards.cycle_rewards list

(** Get the cached current cycle number for a specific instance. *)
val get_current_cycle : instance:string -> int option

(** Get the cached end time for a specific cycle.
    Returns [None] if the cycle end time hasn't been fetched yet.
    Safe for view functions. *)
val get_cycle_end_time : cycle:int -> float option

(** Returns the cached payout summary for [instance] and [cycle], or [None] if
    no summary has been loaded yet. *)
val get_payout_summary :
  instance:string ->
  cycle:int ->
  Octez_manager_rewards.Rewards.cycle_summary option

(** Get payout status for a baker + cycle (paid/unpaid/partial/in_progress).
    Pure cache read — safe for view functions. *)
val get_payout_status :
  instance:string -> cycle:int -> Octez_manager_rewards.Rewards.payout_status

(** Refresh payout status from disk for a specific cycle.
    Reads summary.json to determine Paid vs Partial. Does I/O. *)
val refresh_payout_status : instance:string -> cycle:int -> unit

(** Mark a cycle as having a payout in progress. *)
val mark_in_progress : instance:string -> cycle:int -> unit

(** Clear the in-progress marker for a cycle. *)
val clear_in_progress : instance:string -> cycle:int -> unit

(** Get the auto-detected baker address for an instance.
    Returns [None] if the scheduler hasn't detected the baker yet. *)
val get_baker_for_instance : instance:string -> string option

(** Get the cached network name for an instance.
    Useful for test bakers that don't have a service registry entry. *)
val get_network_for_instance : instance:string -> string option

(** Get the cached payout timer active status for an instance.
    Returns [false] if not cached yet. Safe for view functions. *)
val get_payout_timer_active : instance:string -> bool

(** Get the cached continual interval for an instance.
    Returns [None] if not configured or not cached yet. Safe for view functions. *)
val get_continual_interval : instance:string -> int option

(** Update the payout timer active status cache for an instance.
    Called after install/remove operations to immediately update the TUI. *)
val set_payout_timer_active : instance:string -> active:bool -> unit

(** Update the continual interval cache for an instance.
    Called after install operations to immediately update the TUI. *)
val set_continual_interval : instance:string -> interval:int -> unit

(** {1 Refresh} *)

(** Ensure delegator details are loaded for a specific cycle.
    If the cycle data is missing or lacks delegators, fetches it
    in the background via [Domain_pool]. Safe to call from key handlers. *)
val ensure_cycle_detail : instance:string -> baker:string -> cycle:int -> unit

(** Trigger an immediate refresh for a specific baker instance.
    Called after config changes or payout execution. *)
val refresh_baker : instance:string -> unit
