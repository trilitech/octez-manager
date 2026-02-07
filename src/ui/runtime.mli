(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Recursively create directories along [path] (like [mkdir -p]).
    Silently succeeds if directories already exist. *)
val ensure_dir : string -> unit

val initialize : ?log:bool -> ?logfile:string -> unit -> unit
