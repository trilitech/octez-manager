(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** CLI output formatting utilities *)

open Octez_manager_lib

(** Format a service for list display *)
val pp_service : Format.formatter -> Service.t -> unit

(** Print a list of services *)
val print_services : Service.t list -> unit

(** Format logging mode *)
val pp_logging : Format.formatter -> Logging_mode.t -> unit

(** Print detailed information about a service *)
val print_service_details : Service.t -> unit

(** Print a snapshot entry *)
val print_snapshot_entry : Snapshots.entry -> unit

(** Convert snapshot entry to JSON *)
val snapshot_entry_to_json : Snapshots.entry -> Yojson.Safe.t

(** Get the dropin path for a service *)
val dropin_path_for : role:string -> instance:string -> string

(** Read entire file contents *)
val slurp_file : string -> string

(** Format bytes as human-readable string (GB or MB) *)
val format_bytes : int64 -> string
