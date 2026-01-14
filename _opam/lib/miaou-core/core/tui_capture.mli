(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Append a keystroke event to the capture stream when enabled. *)
val record_keystroke : string -> unit

(** Append a rendered frame snapshot to the capture stream when enabled. [rows]
    and [cols] describe the geometry used for [frame]. *)
val record_frame : rows:int -> cols:int -> string -> unit

(** Close capture writers so tests can run with different environment settings.
    This is primarily intended for the test suite. *)
val reset_for_tests : unit -> unit
