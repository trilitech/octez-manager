(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Cached status metrics for an octez-index instance. *)

type t = {
  head_level : int option;  (** Last indexed block level. *)
  synced : bool option;  (** [true] when the indexer is fully caught up. *)
  last_check : float;  (** Unix timestamp of the last successful poll. *)
}

(** [get ~instance] returns cached metrics for [instance], or [None] if not
    yet polled. *)
val get : instance:string -> t option

(** [set ~instance v] stores metrics for [instance] in the cache. *)
val set : instance:string -> t -> unit

(** [clear ()] removes all cached metrics. *)
val clear : unit -> unit
