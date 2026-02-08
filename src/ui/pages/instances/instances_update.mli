(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Version update, cascade update, and rollback for managed services.

    Handles updating service binaries to new versions, including cascade
    updates (updating dependent services) and automatic rollback on failure. *)

open Octez_manager_lib

(** Choice presented in the version selection modal *)
type version_choice =
  | ManagedVersion of string
  | RegisteredDir of string * string  (** alias, path *)

(** Get all dependent services transitively.
    If A depends on B and B depends on C, updating C should include both A and B.
    Returns services in dependency order (direct dependents first). *)
val get_dependent_services : string -> Service.t list

(** Show the version update modal for a service.
    Lists available managed versions and registered directories,
    handles cascade confirmation, and performs the update in background. *)
val update_version_modal : Service.t -> unit

(** Functions exposed for testing. *)
module For_tests : sig
  (** Extract version string from binary --version output.
      Parses patterns like "24.0 (hash)", "Octez 24.0", "v24.0.1" *)
  val extract_version_string : string -> string option

  (** Map a service role to its binary name. *)
  val role_to_binary_name : string -> string

  (** BFS to collect all transitive dependents, parameterized on deps lookup.
      Returns the accumulated dependent services (not the root). *)
  val collect_dependents :
    get_deps:(string -> Service.t list) -> string -> Service.t list

  (** Remove duplicate services preserving order. *)
  val dedup_services : Service.t list -> Service.t list
end
