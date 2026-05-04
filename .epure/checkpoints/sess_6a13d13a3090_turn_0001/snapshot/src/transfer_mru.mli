(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Most-recently-used transfer destinations.

    Persists a capped list of recently used transfer destination addresses
    to [~/.config/octez-manager/transfer_mru.json]. Entries are ordered by
    most recent use. *)

(** A recently used transfer destination. *)
type entry = {
  pkh : string;  (** Destination public key hash *)
  alias : string option;  (** Optional user-assigned or tzkt alias *)
  last_used_at : float;  (** Unix timestamp of last use *)
}

(** Get the MRU list, most recent first. Fast, reads from memory cache. *)
val get : unit -> entry list

(** Record a transfer destination. Moves it to the front of the list.
    Caps the list at 50 entries, evicting the oldest. Persists to disk. *)
val add : pkh:string -> ?alias:string -> unit -> unit

(** Remove a destination from the MRU list. Persists to disk. *)
val remove : pkh:string -> unit

(** Load the MRU list from disk into memory. Call once at startup. *)
val load : unit -> unit
