(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

(** Keys management page — split-panel wallet view.

    Left panel displays keys grouped by base directory with rich metadata
    (key kind icons, truncated PKH). Right panel shows detail for the
    selected item (base directory info or per-key metadata with balances).

    Keys are loaded from:
    - The default Octez client directory (~/.tezos-client)
    - All managed base directories registered in the directory registry *)

(** Page name for navigation *)
val name : string

(** Register the page in the global page registry *)
val register : unit -> unit

(** Get all keys from all base directories (default + managed).
    Returns a list of (key_hash, alias, base_dir) tuples.
    Performs I/O — should not be called from render functions. *)
val get_all_keys : unit -> (string * string * string) list

(**/**)

module Internal_for_tests : sig
  (** Get the default client base directory (~/.tezos-client) *)
  val default_client_base_dir : unit -> string

  (** Get all base directories to scan for keys (default + managed, deduplicated). *)
  val get_all_base_dirs : unit -> string list
end

(**/**)
