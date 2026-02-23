(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

(** Keys management page - displays Octez keys from all base directories.
    
    This page shows key aliases and their public key hashes (tz1/tz2/tz3/tz4)
    from:
    - The default Octez client directory (~/.tezos-client)
    - All managed base directories registered in the directory registry
    
    Keys are grouped by base directory and displayed in a navigable list. *)

(** Page name for navigation *)
val name : string

(** Register the page in the global page registry *)
val register : unit -> unit

(** Get all keys from all base directories (default + managed).
    Returns a list of (key_hash, alias, base_dir) tuples.
    Performs I/O - should not be called from render functions. *)
val get_all_keys : unit -> (string * string * string) list
