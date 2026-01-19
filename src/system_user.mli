(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

val ensure_service_account :
  ?quiet:bool -> name:string -> unit -> (unit, Rresult.R.msg) result

val ensure_system_directories :
  user:string -> group:string -> unit -> (unit, Rresult.R.msg) result

val validate_user_for_service : user:string -> (unit, Rresult.R.msg) result

val remove_service_account :
  ?quiet:bool -> name:string -> unit -> (unit, Rresult.R.msg) result

module For_tests : sig
  val reset : unit -> unit

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
