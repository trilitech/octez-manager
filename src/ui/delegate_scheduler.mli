(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
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

(** Get delegates configured for a baker instance (from cache, never blocks). *)
val get_baker_delegates : instance:string -> string list

(** Check if baker has DAL enabled (from cache, never blocks). *)
val baker_has_dal : instance:string -> bool

(** Refresh cached config for an instance (call after editing baker).
    Immediately reloads from disk so UI shows fresh data. *)
val invalidate_config : instance:string -> unit

(** Clear all state and cache. *)
val clear : unit -> unit
