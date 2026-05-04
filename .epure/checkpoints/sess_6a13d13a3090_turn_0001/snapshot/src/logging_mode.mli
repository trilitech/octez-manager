(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

(** Logging is always via journald - octez binaries handle their own file logging *)
type t = Journald

(** The default logging mode ([Journald]). *)
val default : t

(** Convert a logging mode to its string representation. *)
val to_string : t -> string

(** Serialize a logging mode to JSON. *)
val to_yojson : t -> Yojson.Safe.t

(** Deserialize a logging mode from JSON. *)
val of_yojson : Yojson.Safe.t -> (t, string) result
