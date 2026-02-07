(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

(** Recursively create directories along [path] (like [mkdir -p]).
    Silently succeeds if directories already exist.
    Behavior is unspecified if [path] exists but is not a directory. *)
val ensure_dir : string -> unit

val initialize : ?log:bool -> ?logfile:string -> unit -> unit
