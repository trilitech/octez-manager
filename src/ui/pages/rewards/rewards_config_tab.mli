(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Configuration tab rendering and editing for the Rewards page. *)

(** Identifiers for editable configuration fields. *)
type field_id =
  | CustomBakerPkh  (** Custom-baker registry: PKH (read-only). *)
  | CustomNetwork  (** Custom-baker registry: network (read-only). *)
  | CustomLabel  (** Custom-baker registry: human-readable label. *)
  | CustomEndpoint  (** Custom-baker registry: RPC endpoint. *)
  | CustomBaseDir  (** Custom-baker registry: client base directory. *)
  | CustomPayoutKey  (** Custom-baker registry: payout key alias. *)
  | BakerFee
  | PayoutMode
  | PayoutKeyAlias
  | IndexerUrl
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

(** Fields visible for the currently selected baker. Custom-baker rows are
    only present when the selected baker comes from {!Custom_baker_registry}. *)
val fields_for_state : Rewards_state.state -> field_id list

(** Number of fields returned by [fields_for_state]. *)
val field_count_for_state : Rewards_state.state -> int

(** Whether [field] cannot be edited (e.g. PKH/network on custom bakers). *)
val is_read_only : field_id -> bool

(** Set a config value from a modal callback (consumed by [consume_pending_config]). *)
val set_pending_config : Octez_manager_rewards.Payout_config.t -> unit

(** Consume and return the pending config if set, resetting it to [None]. *)
val consume_pending_config :
  unit -> Octez_manager_rewards.Payout_config.t option

(** Set a config value that doesn't trigger the dirty flag (for install/remove
    operations where config is already saved to disk). *)
val set_pending_config_clean : Octez_manager_rewards.Payout_config.t -> unit

(** Consume and return the pending clean config if set, resetting it to [None]. *)
val consume_pending_config_clean :
  unit -> Octez_manager_rewards.Payout_config.t option

(** Open a modal to edit a configuration field.

    @param network used by [IndexerUrl] to filter local octez-index services
    to those matching the baker's network.
    @param custom required when editing a [Custom*] field; ignored otherwise.
    Read-only custom fields are no-ops. *)
val edit_field :
  ?network:string ->
  ?custom:Octez_manager_rewards.Custom_baker_registry.entry ->
  Octez_manager_rewards.Payout_config.t ->
  field_id ->
  unit

(** Save the configuration to disk. *)
val save_config :
  instance:string -> Octez_manager_rewards.Payout_config.t -> unit

(** Reset the configuration to defaults for the given baker. *)
val reset_config : baker_pkh:string -> unit

(** Open the payout service action menu (Details, Logs, Install/Remove). *)
val open_payout_service_actions :
  instance:string ->
  baker_pkh:string ->
  config:Octez_manager_rewards.Payout_config.t option ->
  unit

(** Render the configuration tab content. No I/O — reads from page state. *)
val render : state:Rewards_state.state -> cols:int -> _rows:int -> string
