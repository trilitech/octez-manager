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
    - Instance name (with uniqueness validation) - skip with ~skip_instance_name:true
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
    @param binary_validator Optional custom validator for app_bin_dir
    @param skip_instance_name Skip instance name field (for forms with custom logic) *)
val core_service_fields :
  get_core:('model -> Form_builder_common.core_service_config) ->
  set_core:(Form_builder_common.core_service_config -> 'model -> 'model) ->
  binary:string ->
  subcommand:string list ->
  ?baker_mode:('model -> [`Local | `Remote]) ->
  ?binary_validator:(string -> bool) ->
  ?skip_instance_name:bool ->
  unit ->
  'model Form_builder.field list

(** {1 Client-based Tool Bundle} *)

(** Generate fields for client-based tools WITH auto-naming support.

    Same as client_fields but with auto-naming: when selecting a node service,
    automatically sets instance name to "{role}-{node_instance}" if the current
    name is empty or still the default.

    Also inherits app_bin_dir from the selected node if the binary exists there.

    Includes:
    - Node selection (with auto-naming)
    - Base directory

    @param role Service role (e.g., "baker", "accuser")
    @param binary Binary name to check (e.g., "octez-baker")
    @param binary_validator Function to check if binary exists in a directory
    @param get_core Extract core config from model
    @param set_core Update model with new core config
    @param get_client Extract client config from model
    @param set_client Update model with new client config *)
val client_fields_with_autoname :
  role:string ->
  binary:string ->
  binary_validator:(string -> bool) ->
  get_core:('model -> Form_builder_common.core_service_config) ->
  set_core:(Form_builder_common.core_service_config -> 'model -> 'model) ->
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
  ?on_network_selected:(string -> unit) ->
  ?edit_mode:bool ->
  ?editing_instance:string ->
  unit ->
  'model Form_builder.field list
