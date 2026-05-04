(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Export logs and diagnostic information for an instance.

    Creates a tar.gz archive containing:
    - Daily log files for the last 7 days
    - Journald logs for the last 7 days
    - Instance details and configuration
    - Binary version information
    - Related service versions (dependencies and dependents) *)

(** [export_logs ~instance ~svc] exports logs and diagnostic information
    for the given instance. Returns the path to the created archive on success.
    
    @param on_step Optional callback invoked before each major step with a
    description of the current operation. Useful for progress tracking. *)
val export_logs :
  instance:string ->
  svc:Service.t ->
  ?on_step:(string -> unit) ->
  unit ->
  (string, [> `Msg of string]) result

module For_tests : sig
  val get_instance_details : svc:Service.t -> string

  val format_timestamp : float -> string

  val export_filename : instance:string -> timestamp:string -> string
end
