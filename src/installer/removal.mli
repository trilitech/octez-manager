(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Service removal and cleanup functionality *)

(** Remove a service instance.
    
    Stops dependent services, unregisters from parent, disables systemd unit,
    removes dropin, optionally deletes data directory, and updates registry.
    
    @param quiet Suppress command output
    @param delete_data_dir Whether to delete the service's data directory
    @param instance Service instance name
    @return Unit on success *)
val remove_service :
  ?quiet:bool ->
  delete_data_dir:bool ->
  instance:string ->
  unit ->
  (unit, Rresult.R.msg) result

(** Purge a service instance completely.
    
    Performs full removal including data directory, base directory (for baker/accuser),
    env files, and optionally removes service account if no longer used.
    
    @param quiet Suppress command output
    @param prompt_yes_no Function to prompt user for confirmation
    @param instance Service instance name
    @return Unit on success *)
val purge_service :
  ?quiet:bool ->
  prompt_yes_no:(string -> default:bool -> bool) ->
  instance:string ->
  unit ->
  (unit, Rresult.R.msg) result

(** List all registered services.
    
    @return List of service records *)
val list_services : unit -> (Service.t list, Rresult.R.msg) result

(** Clean up old instance after rename operation.
    
    Removes old service registry entry and systemd dropin while preserving data.
    Updates all dependent services to reference the new instance name.
    
    @param quiet Suppress command output
    @param old_instance Old instance name to clean up
    @param new_instance New instance name
    @return Unit on success *)
val cleanup_renamed_instance :
  ?quiet:bool ->
  old_instance:string ->
  new_instance:string ->
  unit ->
  (unit, Rresult.R.msg) result

(** Clean up stale dependency references.
    
    Scans all services and removes references to non-existent dependent services.
    
    @return Number of stale references cleaned *)
val cleanup_dependencies : unit -> (int, Rresult.R.msg) result

(** Find orphaned data directories and log files.
    
    Discovers directories and files not associated with any registered service.
    
    @return Tuple of (orphan_data_dirs, orphan_log_files) *)
val find_orphan_directories :
  unit -> (string list * string list, Rresult.R.msg) result

(** Clean up orphaned directories and files.
    
    Removes data directories and log files not associated with registered services.
    
    @param dry_run If true, only report what would be removed without actually removing
    @return Tuple of (removed_paths, errors) *)
val cleanup_orphans :
  dry_run:bool -> (string list * (string * string) list, Rresult.R.msg) result
