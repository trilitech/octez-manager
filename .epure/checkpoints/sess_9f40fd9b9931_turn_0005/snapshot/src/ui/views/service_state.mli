(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Pure service state types shared between view modules and the data layer. *)

(** Service lifecycle status. *)
type status = Running | Stopped | Unknown of string

(** Full service state snapshot. *)
type t = {
  service : Octez_manager_lib.Service.t;
  enabled : bool option;
  active : bool option;
  status : status;
  status_text : string option;
}

(** Human-readable label for a status. *)
val status_label : t -> string
