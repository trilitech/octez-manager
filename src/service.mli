(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

type t = {
  instance : string;
  role : string;
  network : string;
  history_mode : History_mode.t;
  data_dir : string;
  rpc_addr : Rpc_addr.t;
  net_addr : string;
  service_user : string;
  app_bin_dir : string;
  bin_source : Binary_registry.bin_source option;
      (** How binaries are referenced. None for legacy configs. *)
  created_at : string;
  logging_mode : Logging_mode.t;
  snapshot_auto : bool;
  snapshot_uri : string option;
  snapshot_network_slug : string option;
  snapshot_no_check : bool;
  extra_args : string list;
  depends_on : string option;
  dependents : string list;
  signer_mode : Signer_types.signer_mode option;
      (** Remote signer configuration for bakers (None = Local_keys for backward compat) *)
  signer_uri : string option;  (** Resolved URI for display/metrics *)
  group : string option;  (** Group this service belongs to, if any *)
  enabled_on_boot : bool option;
      (** Last enable-on-boot state octez-manager applied to the systemd unit.
          [None] means unknown (legacy service.json predating this field);
          callers should treat [None] as [true] to preserve prior behavior. *)
}

(** Create a service configuration record.
    Most optional parameters default to sensible values
    (e.g. [snapshot_auto] defaults to [false], [dependents] to [[]]). *)
val make :
  instance:string ->
  role:string ->
  network:string ->
  history_mode:History_mode.t ->
  data_dir:string ->
  rpc_addr:Rpc_addr.t ->
  net_addr:string ->
  service_user:string ->
  app_bin_dir:string ->
  ?bin_source:Binary_registry.bin_source ->
  logging_mode:Logging_mode.t ->
  ?snapshot_auto:bool ->
  ?snapshot_uri:string option ->
  ?snapshot_network_slug:string option ->
  ?snapshot_no_check:bool ->
  ?extra_args:string list ->
  ?depends_on:string option ->
  ?dependents:string list ->
  ?signer_mode:Signer_types.signer_mode option ->
  ?signer_uri:string option ->
  ?group:string option ->
  ?enabled_on_boot:bool option ->
  unit ->
  t

(** Get the bin_source, falling back to Raw_path of app_bin_dir for legacy configs *)
val get_bin_source : t -> Binary_registry.bin_source

(** Serialize a service configuration to JSON. *)
val to_yojson : t -> Yojson.Safe.t

(** Deserialize a service configuration from JSON. *)
val of_yojson : Yojson.Safe.t -> (t, [> `Msg of string]) result
