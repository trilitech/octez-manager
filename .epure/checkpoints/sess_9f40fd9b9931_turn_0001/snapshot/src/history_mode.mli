(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

type t = Rolling | Full | Archive

(** The default history mode ([Rolling]). *)
val default : t

(** Convert a history mode to its string representation. *)
val to_string : t -> string

(** Parse a history mode from a string (case-insensitive). *)
val of_string : string -> (t, [> `Msg of string]) result

(** Pretty-printer for history modes. *)
val pp : Format.formatter -> t -> unit
