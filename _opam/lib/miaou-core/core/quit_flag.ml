(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)
let pending : bool ref = ref false

let set_pending () = pending := true

let clear_pending () = pending := false

let is_pending () = !pending
