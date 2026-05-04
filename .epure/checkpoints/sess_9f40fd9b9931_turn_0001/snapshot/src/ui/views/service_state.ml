(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

(** Pure service state types shared between view modules and the data layer.

    Extracted here so that view modules in [octez_manager_ui_views] can
    reference service state without depending on [octez_manager_ui]. *)

open Octez_manager_lib

(** Service lifecycle status. *)
type status = Running | Stopped | Unknown of string

(** Full service state snapshot. All fields are immutable values. *)
type t = {
  service : Service.t;
  enabled : bool option;
  active : bool option;
  status : status;
  status_text : string option;
}

let status_label {status; _} =
  match status with
  | Running -> "running"
  | Stopped -> "stopped"
  | Unknown msg -> Printf.sprintf "unknown (%s)" msg
