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

  (** Human-readable label for a service's status (e.g. ["running"], ["stopped"]). *)
  val status_label : t -> string
end

module Summary : sig
  type t = {total : int; running : int; stopped : int; unknown : int}
end

(** Load service states from systemd, using a cached result when fresh enough.
    When [detail] is [true], fetches extra per-unit information (slower). *)
val load_service_states : ?detail:bool -> unit -> Service_state.t list

(** Invalidate the service-state cache so the next {!load_service_states}
    call re-queries systemd. *)
val force_refresh : unit -> unit

(** Aggregate a list of service states into a summary count. *)
val summarize : Service_state.t list -> Summary.t

(** Format service states as diagnostic lines for the diagnostics page. *)
val diagnostics_lines : Service_state.t list -> string list

(** Format service states as recent-activity lines for the dashboard. *)
val activity_lines : Service_state.t list -> string list

(** Format a Unix timestamp as a human-readable local-time string. *)
val formatted_timestamp : float -> string

(** Render a short "spotlight" summary of the most important services.
    @param limit Maximum number of lines to return. *)
val spotlight_lines : Service_state.t list -> limit:int -> string list

module For_tests : sig
  (** Parse systemd's [is-enabled] output into [Some true/false] or [None]. *)
  val parse_enabled_response : string -> bool option

  (** Classify a systemd unit-state result into [(enabled, status)]. *)
  val classify_unit_state :
    (Octez_manager_lib.Systemd.unit_state, Rresult.R.msg) result ->
    bool option * Service_state.status
end
