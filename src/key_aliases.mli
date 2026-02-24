(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** OM-level key alias overrides.

    Stores user-assigned aliases that override octez-client aliases for
    display purposes. Aliases are scoped per wallet directory (base_dir)
    and keyed by PKH. Persisted to
    [{registry_root}/key_aliases.json]. *)

(** Get the OM alias for a key, if one exists. Fast, reads from memory. *)
val get : base_dir:string -> pkh:string -> string option

(** Set or update the OM alias for a key. Persists to disk. *)
val set : base_dir:string -> pkh:string -> alias:string -> unit

(** Remove the OM alias for a key (reverts to octez-client alias).
    Persists to disk. *)
val remove : base_dir:string -> pkh:string -> unit

(** Load aliases from disk into memory. Call once at startup. *)
val load : unit -> unit
