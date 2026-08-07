(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

(** Create a system user account (and group) for running Octez services.
    No-op if the account already exists. *)
val ensure_service_account :
  ?quiet:bool -> name:string -> unit -> (unit, Rresult.R.msg) result

(** Create standard system directories ([/var/lib/octez], [/var/log/octez], etc.)
    with correct ownership for [user:group]. *)
val ensure_system_directories :
  user:string -> group:string -> unit -> (unit, Rresult.R.msg) result

(** Validate that [user] exists and is suitable for running a service
    (e.g. not the root user when running in user mode). *)
val validate_user_for_service : user:string -> (unit, Rresult.R.msg) result

(** Remove a service account previously created by {!ensure_service_account}.
    Deletes the user and its primary group. *)
val remove_service_account :
  ?quiet:bool -> name:string -> unit -> (unit, Rresult.R.msg) result

module For_tests : sig
  (** Reset cached state used by the system user module. *)
  val reset : unit -> unit

  (** Run [f] with overridden implementations for testing.
      Allows injecting stubs for [is_root], [run], [user_exists], and [group_exists]. *)
  val with_overrides :
    ?is_root:(unit -> bool) ->
    ?run:
      (?quiet:bool ->
      ?on_log:(string -> unit) ->
      string list ->
      (unit, Rresult.R.msg) result) ->
    ?user_exists:(string -> bool) ->
    ?group_exists:(string -> bool) ->
    (unit -> 'a) ->
    'a
end
