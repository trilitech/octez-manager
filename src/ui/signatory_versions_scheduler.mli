(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** In-memory cache for available Signatory versions *)

open Octez_manager_lib

(** Get cached versions (returns None if not yet fetched).
    Safe to call from render path. *)
val get_cached : unit -> Signatory_downloader.version_info list option

(** Manually refresh the cache (background fetch).
    Call this from actions, not from view functions. *)
val refresh : unit -> unit

(** Start the background scheduler (called at TUI startup) *)
val start : unit -> unit

(** Shutdown the scheduler *)
val shutdown : unit -> unit

(** Clear the cache (for testing) *)
val clear : unit -> unit
