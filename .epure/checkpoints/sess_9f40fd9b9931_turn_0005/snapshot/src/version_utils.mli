(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Shared version string parsing and comparison utilities.

    Used by {!Binary_registry}, {!Version_checker}, and
    {!Self_update_checker} to avoid duplicating version logic. *)

(** Parse a version string into numeric components.
    Strips a leading ['v'] and any suffix after ['-'] (e.g., "-rc1").
    Returns [[]] for unparseable strings.
    @return list of integer components, e.g. "24.1.3" -> [24; 1; 3] *)
val parse_version : string -> int list

(** [is_rc version] returns [true] if [version] contains a ['-'] suffix
    (e.g., "24.0-rc1"). *)
val is_rc : string -> bool

(** Extract the RC number from a version string.
    @return [Some n] for "X.Y-rcN", [None] otherwise *)
val extract_rc_number : string -> int option

(** Compare two version strings numerically.
    Handles RC suffixes: a release is newer than its RC.
    @return negative if [v1 < v2], [0] if equal, positive if [v1 > v2] *)
val compare_versions : string -> string -> int
