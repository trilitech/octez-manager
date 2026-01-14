(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)
[@@@warning "-32-34-37-69"]

(** Pure hierarchical key handler stack (no global mutable state).

    This module exposes an immutable stack value ['t]. All operations return
    a new stack instead of mutating hidden refs so the caller (driver) can
    thread it through its event loop state. This satisfies the requirement
    that the key stack is part of the overall application state.

    Future extensions (e.g. actions pushing new frames) can be supported by
    enriching the action type to carry stack transformers. For now actions are
    simple callbacks (typically mutating page/modal state captured in closures).
*)

type action = unit -> unit

type binding = {action : action option; help : string; display_only : bool}

type frame = {
  id : int;
  delegate : bool;  (** if true, unhandled keys bubble to next frame *)
  bindings : (string, binding) Hashtbl.t;  (** key -> binding *)
}

(** opaque stack *)
type t

val empty : t

(** reference id for later pop *)
type handle = int

val push : t -> ?delegate:bool -> (string * binding) list -> t * handle

val pop : t -> handle -> t

val pop_top : t -> t

val clear : t -> t

(** Dispatch a key, returning (consumed, new_stack). New stack is identical for
    now (no structural change) but reserved for future evolving actions. *)
val dispatch : t -> string -> bool * t

(* Introspection *)
val depth : t -> int

val top_keys : t -> string list

(** Return (key, help) pairs for the current top frame, preserving no
  ordering guarantees (hash table fold). *)
val top_bindings : t -> (string * string) list

(** Return flattened list of (key, help) pairs from all frames top-first.
  Duplicates may appear if keys shadow lower frames. *)
val all_bindings : t -> (string * string) list
