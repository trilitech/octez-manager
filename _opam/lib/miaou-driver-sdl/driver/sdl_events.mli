(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type next_action = Refresh | Quit | Key of string

val poll_event : timeout_ms:int -> on_resize:(unit -> unit) -> next_action
