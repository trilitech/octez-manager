(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type t = [`Handled | `Bubble]

let handled = `Handled

let bubble = `Bubble

let to_bool = function `Handled -> true | `Bubble -> false
