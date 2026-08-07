(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Minimal HJSON parser for importing external configuration files.

    Normalizes HJSON to JSON by stripping comments, quoting unquoted
    keys, removing trailing commas, and handling multiline strings,
    then delegates to {!Yojson.Safe} for final parsing. *)

(** Parse an HJSON string into a JSON value.
    @return [Ok json] on success, [Error msg] with line context on failure. *)
val parse : string -> (Yojson.Safe.t, string) result

(** Parse an HJSON file at the given path.
    @return [Ok json] on success, [Error msg] with file/line context. *)
val parse_file : string -> (Yojson.Safe.t, string) result

(**/**)

module Internal_for_tests : sig
  val strip_comments : string -> string

  val normalize_to_json : string -> string
end

(**/**)
