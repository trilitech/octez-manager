(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Full-page viewer for payout service details and logs. *)

(** Tab selection for the payout service page. *)
type tab = Details | Logs

(** Set the initial tab to open when navigating to the page. *)
val set_initial_tab : tab -> unit

(** Page name for navigation. *)
val name : string

(** Register the page with the Miaou registry. *)
val register : unit -> unit
