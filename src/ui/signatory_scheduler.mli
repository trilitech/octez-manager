(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Background scheduler for polling Signatory health and metrics *)

(** Start the scheduler in a background domain *)
val start : unit -> unit

(** Request scheduler shutdown *)
val stop : unit -> unit
