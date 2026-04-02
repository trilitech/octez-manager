(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

type bootstrap = Genesis | Snapshot of {src : string option}

type resolved_data_dir_config = {
  network : string;
  history_mode : History_mode.t;
  rpc_addr : Rpc_addr.t;
  net_addr : string;
}

let pp_resolved_data_dir_config fmt {network; history_mode; rpc_addr; net_addr}
    =
  Format.fprintf
    fmt
    "Network: %s, History-mode: %s, Rpc-addr: %s, Net-addr: %s"
    network
    (History_mode.to_string history_mode)
    (Rpc_addr.to_string rpc_addr)
    net_addr

type node_request = {
  instance : string;
  network : string;
  history_mode : History_mode.t;
  data_dir : string option;
  rpc_addr : Rpc_addr.t;
  net_addr : string;
  service_user : string;
  app_bin_dir : string;
  bin_source : Binary_registry.bin_source option;
  logging_mode : Logging_mode.t;
  extra_args : string list;
  extra_env : (string * string) list;
  auto_enable : bool;
  bootstrap : bootstrap;
  preserve_data : bool;
  snapshot_no_check : bool;
  tmp_dir : string option;
  keep_snapshot : bool;
}

type daemon_request = {
  role : string;
  instance : string;
  network : string;
  history_mode : History_mode.t;
  data_dir : string;
  rpc_addr : Rpc_addr.t;
  net_addr : string;
  service_user : string;
  app_bin_dir : string;
  bin_source : Binary_registry.bin_source option;
  logging_mode : Logging_mode.t;
  service_args : string list;
  extra_env : (string * string) list;
  extra_paths : string list;
  auto_enable : bool;
  depends_on : string option;
  preserve_data : bool;
}

type baker_node_mode =
  | Local_instance of string  (** managed instance name *)
  | Local_datadir of string * string
      (** (endpoint, data_dir) - unmanaged local node *)
  | Remote_endpoint of string  (** endpoint - truly remote node *)

type resolved_baker_node_mode =
  | Local of Service.t  (** managed local instance *)
  | Local_unmanaged of string * string  (** (endpoint, data_dir) *)
  | Remote of string  (** remote endpoint *)

type dal_config = Dal_auto | Dal_disabled | Dal_endpoint of string

(** Extra node specification for baker multi-node support *)
type extra_node_spec =
  | Extra_instance of string  (** Managed node instance *)
  | Extra_endpoint of string  (** Raw RPC endpoint *)

(** Re-export signer_mode from Signer_types for convenience *)
include Signer_types

type baker_request = {
  instance : string;
  node_mode : baker_node_mode;
  base_dir : string option;
  delegates : string list;
  dal_config : dal_config;
  dal_node : string option;  (** DAL node instance name if using local DAL *)
  liquidity_baking_vote : string option;
  signer_mode : signer_mode;  (** Remote signer configuration *)
  extra_args : string list;
  extra_env : (string * string) list;
  service_user : string;
  app_bin_dir : string;
  bin_source : Binary_registry.bin_source option;
  logging_mode : Logging_mode.t;
  auto_enable : bool;
  preserve_data : bool;
  extra_nodes : extra_node_spec list;
      (** Additional nodes for redundancy (octez-baker --extra-node) *)
}

type accuser_request = {
  instance : string;
  node_mode : baker_node_mode;
  base_dir : string option;
  extra_args : string list;
  service_user : string;
  app_bin_dir : string;
  bin_source : Binary_registry.bin_source option;
  logging_mode : Logging_mode.t;
  auto_enable : bool;
  preserve_data : bool;
}

type snapshot_file = {path : string; cleanup : bool}

type snapshot_resolution = {
  download_url : string;
  network_slug : string;
  kind_slug : string;
}

type snapshot_plan =
  | No_snapshot
  | Direct_snapshot of {uri : string}
  | Tzinit_snapshot of snapshot_resolution

type snapshot_metadata = {
  auto : bool;
  uri : string option;
  network_slug : string option;
  kind_slug : string option;
  no_check : bool;
}

(** Signatory operation permissions *)
type signatory_operation =
  | Block  (** Sign new blocks (baking) *)
  | Attestation  (** Sign consensus attestations *)
  | Preattestation  (** Sign pre-attestations *)
  | Attestation_with_dal  (** Sign DAL-enabled attestations *)
  | Generic  (** Sign manager operations (transactions, etc.) *)

(** Authorized key with its permissions *)
type authorized_key = {
  pkh : string;  (** Public key hash (tz1/tz2/tz3/tz4) *)
  permissions : signatory_operation list;  (** Allowed operations *)
}

(** Signatory backend configuration *)
type signatory_backend =
  | File of string  (** Path to keys directory *)
  | YubiHSM of {connector_url : string}
  | Azure_KMS of {vault_name : string; tenant_id : string}
  | AWS_KMS of {region : string}
  | GCP_KMS of {project_id : string; location : string}
  | Vault of {address : string; role : string}

(** Signatory watermark backend *)
type watermark_backend =
  | Memory
  | File_watermark of string  (** Path to watermark file *)
  | AWS_DynamoDB of {table_name : string; region : string}
  | GCP_Firestore of {project_id : string; collection : string}

(** Signatory installation request *)
type signatory_request = {
  instance : string;
  backend : signatory_backend;
  authorized_keys : authorized_key list;
      (** Authorized keys with permissions *)
  address : string;  (** HTTP server address, e.g., "127.0.0.1:6732" *)
  metrics_address : string;  (** Metrics endpoint address *)
  watermark : watermark_backend;
  service_user : string;
  app_bin_dir : string;
  bin_source : Binary_registry.bin_source option;
  logging_mode : Logging_mode.t;
  auto_enable : bool;
  preserve_data : bool;
}

type file_backup = {tmp_path : string; original_path : string}

(** Strategy for importing external services *)
type import_strategy =
  | Takeover  (** Take over the external service (stop and disable it) *)
  | Clone  (** Create a clone, leave original running *)
