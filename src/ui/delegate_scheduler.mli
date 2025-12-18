(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Background scheduler for polling delegate data. *)

(** Start the background polling loop.
    Only starts once; subsequent calls are no-ops. *)
val start : unit -> unit

(** Perform one tick of polling (all bakers). *)
val tick : unit -> unit

(** Get delegate data for a specific public key hash. *)
val get_delegate_data : pkh:string -> Delegate_data.t option

(** Get all delegate data for a baker instance. *)
val get_baker_delegate_data : instance:string -> Delegate_data.t list

(** Get delegates configured for a baker instance. *)
val get_baker_delegates : instance:string -> string list

(** Clear all state and cache. *)
val clear : unit -> unit
