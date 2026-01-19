(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
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
