(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Cascade import for handling dependency chains.

    This module provides dependency graph analysis and cascade import
    functionality for external services that depend on each other. *)

(** {1 Types} *)

(** Node in the dependency graph *)
type graph_node = {
  service : External_service.t;
  dependencies : string list;  (** Unit names this service depends on *)
  dependents : string list;  (** Unit names that depend on this service *)
}

(** Result of dependency analysis *)
type dependency_analysis = {
  nodes : graph_node list;
  import_order : string list;
      (** Topologically sorted order for importing (dependencies first) *)
  cycles : string list list;
      (** Detected dependency cycles (should be empty) *)
  external_dependents : (string * string) list;
      (** Services not in the import set that depend on services being imported.
        Each entry is (dependent_name, dependency_name). *)
}

(** Validation error for cascade import *)
type validation_error =
  | Cycle_detected of string list
      (** Dependency cycle found - list of service names in the cycle *)
  | External_dependents_exist of (string * string) list
      (** External services would be broken by takeover.
        List of (dependent_name, dependency_name) pairs. *)
  | Missing_dependencies of (string * string list) list
      (** Some services have dependencies not in the import set.
        List of (service_name, missing_dependency_names) pairs. *)

(** {1 Dependency Graph Analysis} *)

(** Build dependency graph from a list of services.
    
    @param services All available external services
    @param target_services Services to analyze (subset of services)
    @return Dependency analysis with import order and validation info *)
val analyze_dependencies :
  services:External_service.t list ->
  target_services:External_service.t list ->
  dependency_analysis

(** Get all dependencies of a service (transitive closure).
    
    @param service The service to analyze
    @param all_services All available services for lookup
    @return List of services in dependency order (dependencies first) *)
val get_dependency_chain :
  service:External_service.t ->
  all_services:External_service.t list ->
  External_service.t list

(** Get full cascade: service + all dependencies + all dependents.
    
    This computes the complete transitive closure including both:
    - Dependencies (services this one needs)
    - Dependents (services that need this one or its dependencies)
    
    Used for Takeover strategy to import everything that would be affected.
    
    @param service The service to analyze
    @param all_services All available services for lookup
    @return List of services in dependency order (dependencies first, dependents last) *)
val get_full_cascade :
  service:External_service.t ->
  all_services:External_service.t list ->
  External_service.t list

(** {1 Validation} *)

(** Validate a set of services can be imported together.
    
    Checks for:
    - Dependency cycles
    - External dependents (for takeover strategy)
    - Missing dependencies
    
    @param services All available services
    @param target_services Services to import
    @param strategy Import strategy (Takeover or Clone)
    @return Ok () or Error with validation details *)
val validate_cascade :
  services:External_service.t list ->
  target_services:External_service.t list ->
  strategy:Installer_types.import_strategy ->
  (unit, validation_error) result

(** {1 Pretty Printing} *)

(** Format validation error as human-readable message *)
val pp_validation_error : Format.formatter -> validation_error -> unit

(** Format dependency analysis as human-readable output *)
val pp_analysis : Format.formatter -> dependency_analysis -> unit

(** {1 Testing} *)

(** Exposed for unit testing. Do not use outside tests. *)
module For_tests : sig
  val list_drop : int -> 'a list -> 'a list

  val topological_sort : graph_node list -> string list * string list list

  val build_graph_nodes :
    all_services:External_service.t list ->
    target_services:External_service.t list ->
    graph_node list
end
