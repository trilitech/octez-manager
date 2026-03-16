(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Payout configuration: per-baker settings for reward distribution.

    Stored at [~/.octez-manager/rewards/<baker-instance>/config.json].
    One configuration per baker instance. *)

(** {1 Configuration type} *)

type t = {
  version : int;
  baker_pkh : string;
  payout_key_alias : string;
  payout_mode : Rewards.payout_mode;
  baker_fee : float;
  min_payout : Int64.t;
  min_balance : Int64.t;
  below_min_dest : Rewards.below_min_destination;
  overdelegation_protect : bool;
  baker_pays_tx_fee : bool;
  baker_pays_alloc_fee : bool;
  ignore_contracts : bool;
  gas_buffer : int;
  kt_gas_buffer : int;
  deser_gas_buffer : int;
  fee_buffer : int;
  kt_fee_buffer : int;
  sim_batch_size : int;
  min_delay_blocks : int;
  max_delay_blocks : int;
  whitelist : string list;
  blacklist : string list;
  delegator_overrides : (string * Rewards.delegator_override) list;
  bond_recipients : (string * float) list;
  fee_recipients : (string * float) list;
  rpc_fallback_pool : string list;
  tzkt_url : string;
  explorer_url : string;
  notifications : Rewards.notification_channel list;
  continual_enabled : bool;
  continual_interval : int;
  continual_offset : int;
}

(** {1 Defaults and construction} *)

(** Default configuration for a given baker address.
    @param network  Network name or URL (default ["mainnet"]).
      Used to derive the correct TzKT base URL. *)
val default : ?network:string -> baker_pkh:string -> unit -> t

(** Return the effective TzKT base URL for [config] on [network].
    If the stored [tzkt_url] is the generic mainnet default but the baker
    is on a different network, returns the correct URL for that network.
    Otherwise returns [config.tzkt_url] unchanged. *)
val effective_tzkt_url : network:string -> t -> string

(** {1 Validation} *)

(** Validate a configuration. Returns [Ok ()] or [Error msg]. *)
val validate : t -> (unit, string) result

(** {1 Persistence} *)

(** Directory path for a baker's rewards data. *)
val rewards_dir : instance:string -> string

(** Load configuration from disk for a given baker instance. *)
val load : instance:string -> (t, string) result

(** Save configuration to disk for a given baker instance. *)
val save : instance:string -> t -> (unit, string) result

(** {1 Serialization} *)

(** Serialize a configuration to JSON. *)
val to_json : t -> Yojson.Safe.t

(** Deserialize a configuration from JSON. *)
val of_json : Yojson.Safe.t -> (t, string) result
