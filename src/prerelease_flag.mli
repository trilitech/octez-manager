(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Process-wide opt-in for surfacing prerelease binaries.

    When [true], version listings, "latest" resolvers, and TUI version
    pickers include release candidates and beta/alpha builds for Octez,
    Signatory, and octez-index.

    Defaults to [false]. Set once at CLI/TUI startup from the
    [--unreleased-binaries] flag; not persisted across invocations. *)

val get : unit -> bool

val set : bool -> unit
