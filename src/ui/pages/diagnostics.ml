(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Diagnostics page with charts and metrics recording *)

(* Re-export from diagnostics_page module *)
include Diagnostics_page

let name = Diagnostics_page.name

let page = Diagnostics_page.page

let register = Diagnostics_page.register
