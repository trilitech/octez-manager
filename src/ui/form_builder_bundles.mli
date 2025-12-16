(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Pre-built field bundles for common Octez tool configurations.

    These bundles provide reusable field groups that can be composed
    together to build installer forms quickly and consistently. *)

(** {1 Core Service Bundle} *)

(** Generate core service fields common to all Octez tools.

    Includes:
    - Instance name (with uniqueness validation)
    - Service user (with existence validation)
    - App bin dir (with binary validation)
    - Logging mode (Journald vs File)
    - Enable on boot toggle
    - Start now toggle
    - Extra args (with binary help explorer)

    @param get_core Extract core config from model
    @param set_core Update model with new core config
    @param binary Binary name (e.g., "octez-baker", "octez-node")
    @param subcommand Subcommand for help explorer (e.g., ["run"])
    @param binary_validator Optional custom validator for app_bin_dir *)
val core_service_fields :
  get_core:('model -> Form_builder_common.core_service_config) ->
  set_core:(Form_builder_common.core_service_config -> 'model -> 'model) ->
  binary:string ->
  subcommand:string list ->
  ?binary_validator:(string -> bool) ->
  unit ->
  'model Form_builder.field list

(** {1 Client-based Tool Bundle} *)

(** Generate fields for client-based tools (baker, accuser, DAL node, signer).

    Includes:
    - Base directory (client wallet/config directory)
    - Node selection (existing service or custom endpoint)

    The node endpoint is derived from the node selection automatically.

    @param get_client Extract client config from model
    @param set_client Update model with new client config *)
val client_fields :
  get_client:('model -> Form_builder_common.client_config) ->
  set_client:(Form_builder_common.client_config -> 'model -> 'model) ->
  unit ->
  'model Form_builder.field list

(** {1 Node-specific Bundle} *)

(** Generate fields specific to node installation.

    Includes:
    - Network selection (with dynamic fetch from Teztnets)
    - History mode choice (rolling/full/archive)
    - Data directory (with conflict checking)
    - RPC address (with port validation)
    - P2P address (with port validation)

    Note: Snapshot selection is NOT included as it requires special handling
    with the pre_submit_modal feature. Add it separately if needed.

    @param get_node Extract node config from model
    @param set_node Update model with new node config *)
val node_fields :
  get_node:('model -> Form_builder_common.node_config) ->
  set_node:(Form_builder_common.node_config -> 'model -> 'model) ->
  unit ->
  'model Form_builder.field list
