(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

open Octez_manager_lib

module Service_state : sig
  type status = Running | Stopped | Unknown of string

  type t = {
    service : Service.t;
    enabled : bool option;
    active : bool option;
    status : status;
    status_text : string option;
  }

  val status_label : t -> string
end

module Summary : sig
  type t = {total : int; running : int; stopped : int; unknown : int}
end

val load_service_states : ?detail:bool -> unit -> Service_state.t list

val summarize : Service_state.t list -> Summary.t

val diagnostics_lines : Service_state.t list -> string list

val activity_lines : Service_state.t list -> string list

val formatted_timestamp : float -> string

val spotlight_lines : Service_state.t list -> limit:int -> string list

module For_tests : sig
  val parse_enabled_response : string -> bool option

  val classify_unit_state :
    (Octez_manager_lib.Systemd.unit_state, Rresult.R.msg) result ->
    bool option * Service_state.status
end
