(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Configuration tab rendering and editing for the Rewards page. *)

(** Identifiers for editable configuration fields. *)
type field_id =
  | BakerFee
  | PayoutMode
  | PayoutKeyAlias
  | MinPayout
  | MinBalance
  | BelowMinDest
  | OverdelegationProtect
  | BakerPaysTxFee
  | BakerPaysAllocFee
  | IgnoreContracts
  | ContinualEnabled
  | ContinualInterval
  | ContinualOffset

(** All editable fields in display order. *)
val all_fields : field_id list

(** Number of editable fields. *)
val field_count : int

(** Set a config value from a modal callback (consumed by [consume_pending_config]). *)
val set_pending_config : Octez_manager_rewards.Payout_config.t -> unit

(** Consume and return the pending config if set, resetting it to [None]. *)
val consume_pending_config :
  unit -> Octez_manager_rewards.Payout_config.t option

(** Open a modal to edit a configuration field. *)
val edit_field : Octez_manager_rewards.Payout_config.t -> field_id -> unit

(** Save the configuration to disk. *)
val save_config :
  instance:string -> Octez_manager_rewards.Payout_config.t -> unit

(** Reset the configuration to defaults for the given baker. *)
val reset_config : baker_pkh:string -> unit

(** Render the configuration tab content. No I/O — reads from page state. *)
val render : state:Rewards_state.state -> cols:int -> _rows:int -> string
