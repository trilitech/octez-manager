(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Service lifecycle management (start, stop, restart) *)

(** Get list of stopped dependencies for a service.
    
    @param instance Service instance name
    @return List of stopped dependency services *)
val get_stopped_dependencies :
  instance:string -> unit -> (Service.t list, [`Msg of string]) result

(** Get list of stopped dependents for a service.
    
    @param instance Service instance name
    @return List of stopped dependent services *)
val get_stopped_dependents :
  instance:string -> unit -> (Service.t list, [`Msg of string]) result

(** Start a service by instance name.
    
    Enables the service if not already enabled, then starts it.
    
    @param quiet Suppress command output
    @param instance Service instance name
    @return Unit on success *)
val start_service :
  ?quiet:bool -> instance:string -> unit -> (unit, Rresult.R.msg) result

(** Stop a service and all its dependents recursively.
    
    Stops dependent services first (depth-first), then stops the target service.
    Used internally to ensure proper shutdown order.
    
    @param quiet Suppress command output
    @param instance Service instance name
    @return Unit on success *)
val stop_service_cascade :
  ?quiet:bool -> instance:string -> unit -> (unit, Rresult.R.msg) result

(** Stop a service by instance name.
    
    Alias for stop_service_cascade - stops the service and all its dependents.
    
    @param quiet Suppress command output
    @param instance Service instance name
    @return Unit on success *)
val stop_service :
  ?quiet:bool -> instance:string -> unit -> (unit, Rresult.R.msg) result

(** Restart a service by instance name.
    
    Stops the service and all its dependents, then restarts them in the correct order.
    
    @param quiet Suppress command output
    @param instance Service instance name
    @return Unit on success *)
val restart_service :
  ?quiet:bool -> instance:string -> unit -> (unit, Rresult.R.msg) result

(** Numeric role order for dependency sorting (node=0, baker=1, etc.) *)
val role_order : string -> int

(** Get all services belonging to a group, sorted by dependency order
    (nodes first, then bakers/accusers/dal-nodes/signatories). *)
val group_services :
  group_name:string -> unit -> (Service.t list, Rresult.R.msg) result

(** Start all services in a group, in dependency order (nodes first).
    Stops on first failure.

    @param quiet Suppress command output
    @param group_name Group name
    @return List of started instance names on success *)
val start_group :
  ?quiet:bool ->
  group_name:string ->
  unit ->
  (string list, Rresult.R.msg) result

(** Stop all services in a group, in reverse dependency order (children first).
    Continues on failure (best-effort).

    @param quiet Suppress command output
    @param group_name Group name
    @return List of stopped instance names (may be partial on errors) *)
val stop_group :
  ?quiet:bool ->
  group_name:string ->
  unit ->
  (string list, Rresult.R.msg) result

(** Restart all services in a group: stop all (reverse order),
    then start all (forward order).

    @param quiet Suppress command output
    @param group_name Group name
    @return List of restarted instance names on success *)
val restart_group :
  ?quiet:bool ->
  group_name:string ->
  unit ->
  (string list, Rresult.R.msg) result
