(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Page state wrapper with navigation support.
    Pages work with ['a pstate] instead of raw state, enabling
    framework-managed navigation without boilerplate. *)

type 'a t = {s : 'a; nav : string option}

let make s = {s; nav = None}

let goto page ps = {ps with nav = Some page}

let back ps = {ps with nav = Some "__BACK__"}

let quit ps = {ps with nav = Some "__QUIT__"}

let pending ps = ps.nav

let update f ps = {ps with s = f ps.s}
