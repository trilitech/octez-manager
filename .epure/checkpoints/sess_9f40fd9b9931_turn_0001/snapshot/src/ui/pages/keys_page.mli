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

(** Page implementation satisfying the Miaou TUI page signature. *)
module Page : Miaou.Core.Tui_page.PAGE_SIG

(** Register the page in the global page registry *)
val register : unit -> unit

(** Get all keys from all base directories (default + managed).
    Returns a list of (key_hash, alias, base_dir) tuples.
    Performs I/O - should not be called from render functions. *)
val get_all_keys : unit -> (string * string * string) list

(**/**)

module Internal_for_tests : sig
  (** Get the default client base directory (~/.tezos-client) *)
  val default_client_base_dir : unit -> string

  (** Get all base directories to scan for keys (default + managed, deduplicated).
      Exposed for testing deduplication logic. *)
  val get_all_base_dirs : unit -> string list
end

(**/**)
