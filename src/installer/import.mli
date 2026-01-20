(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Import external services as managed instances.

    Converts detected unmanaged services into fully managed octez-manager
    instances while preserving data and configuration. *)

(** {1 Import Strategy} *)

(** Strategy for handling the original external service *)
type import_strategy = Installer_types.import_strategy =
  | Takeover
      (** Stop external service, create managed service, disable original unit.
          Gives octez-manager full control. Original can be restored later. *)
  | Clone
      (** Create managed service, leave original untouched.
          Useful for testing octez-manager without disrupting existing setup.
          Warning: Both services will use the same data directory. *)

(** {1 Field Overrides} *)

(** User-provided values to override or fill in missing detected fields *)
type field_overrides = {
  network : string option;
  data_dir : string option;
  rpc_addr : string option;
  net_addr : string option;
  base_dir : string option;  (** For baker/accuser *)
  delegates : string list option;  (** For baker *)
}

(** Empty overrides (all None) *)
val empty_overrides : field_overrides

(** {1 Import Options} *)

(** Options controlling the import process *)
type import_options = {
  strategy : import_strategy;
  new_instance_name : string option;
      (** Override suggested instance name. If None, uses suggested name from external service. *)
  overrides : field_overrides;  (** Override detected values *)
  dry_run : bool;  (** Preview changes without executing *)
  preserve_data : bool;  (** Always true for import (no data copying) *)
  quiet : bool;  (** Non-interactive mode (fail if missing required fields) *)
}

(** {1 Import Result} *)

(** Result of a successful import *)
type import_result = {
  original_unit : string;  (** Original systemd unit name *)
  new_instance : string;  (** New managed instance name *)
  preserved_paths : string list;  (** Preserved data/base directories *)
  warnings : string list;  (** Non-fatal warnings during import *)
}

(** {1 Validation} *)

(** Validate that an external service can be imported.

    Checks:
    - Service is systemd-managed (not standalone process)
    - Service is not already managed by octez-manager
    - Network is known or can be inferred
    - No name conflicts with existing managed services

    @param external_svc External service to validate
    @return Ok () if importable, Error with reason otherwise *)
val validate_importable : External_service.t -> (unit, Rresult.R.msg) result

(** Get list of required fields that are missing or unknown.

    Required fields by role:
    - Node: network, data_dir
    - Baker: network, base_dir, node_endpoint
    - Accuser: network, base_dir, node_endpoint
    - DAL: network, data_dir, node_endpoint

    @param external_svc External service
    @return List of missing field names *)
val missing_required_fields : External_service.t -> string list

(** {1 Import} *)

(** Import an external service as a managed instance.

    Process:
    1. Validate service is importable
    2. Resolve instance name (use override or suggested)
    3. Check for name conflicts
    4. Resolve all required fields (detected + overrides + prompts)
    5. Stop external service (if Takeover strategy)
    6. Create managed service with preserve_data=true
    7. Disable/remove original systemd unit (if Takeover)
    8. Enable and start managed service
    9. Verify service started successfully
    10. Invalidate external services cache

    If any step fails after stopping the external service, automatic rollback
    is triggered to restore the original service.

    @param on_log Optional callback for progress messages
    @param options Import options
    @param external_svc External service to import
    @return Import result or error
    @raise any exception triggers rollback *)
val import_service :
  ?on_log:(string -> unit) ->
  ?imported_services:(string, string) Hashtbl.t ->
  ?all_external_services:External_service.t list ->
  options:import_options ->
  external_svc:External_service.t ->
  unit ->
  (import_result, Rresult.R.msg) result

(** {1 Cascade Import} *)

(** Import a service and all its dependencies in the correct order.

    This function analyzes the dependency graph, validates it can be imported,
    and imports services in topological order (dependencies first).

    Process:
    1. Build dependency chain (transitive closure)
    2. Validate cascade (check for cycles, external dependents, etc.)
    3. Sort services in dependency order
    4. Import each service in order (dependencies → dependents)
    5. Start services in order after all are imported

    @param on_log Optional callback for progress messages
    @param options Import options (applied to all services in the chain)
    @param external_svc The target service to import (along with its dependencies)
    @param all_services All available external services for dependency resolution
    @return List of import results (one per service) or error
    @raise any exception during import of any service triggers rollback of all *)
val import_cascade :
  ?on_log:(string -> unit) ->
  options:import_options ->
  external_svc:External_service.t ->
  all_services:External_service.t list ->
  unit ->
  (import_result list, Rresult.R.msg) result

(** {1 Rollback} *)

(** Rollback a failed import attempt.

    Called automatically when import_service fails after stopping the
    external service. Can also be called manually.

    Actions:
    1. Re-enable original systemd unit
    2. Start original service
    3. Remove partially created managed service (if any)
    4. Clean up registry entries

    @param original_unit Original systemd unit name
    @param new_instance Partially created managed instance name (if any)
    @return Ok () if rollback successful, Error otherwise *)
val rollback_import :
  original_unit:string ->
  new_instance:string option ->
  (unit, Rresult.R.msg) result
