(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(* Lightweight key token module for TUI key bindings. *)
type t =
  | Up
  | Down
  | Left
  | Right
  | Tab
  | ShiftTab
  | Enter
  | Backspace
  | Char of string
  | Control of string
  | PageUp
  | PageDown
  | Home
  | End
  | Escape
  | Delete
  | Function of int

(* Global keys reserved for application-wide functionality *)
type global_key = Settings | Help | Menu | Quit

val of_string : string -> t option

val to_string : t -> string

val equal : t -> t -> bool

val to_label : t -> string

(* Check if a key is reserved as a global key *)
val is_global_key : t -> bool

(* Get the global action for a key string *)
val get_global_action : string -> global_key option

(* Get list of global key bindings for display *)
val show_global_keys : unit -> (string * string) list
