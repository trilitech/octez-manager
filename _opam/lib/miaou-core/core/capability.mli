(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)
include module type of Miaou_interfaces.Capability
(*
	SPDX-License-Identifier: MIT
	Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>

	Typed capability registry using type-indexed keys (no unsafe casts).
	Executables inject implementations; core remains backend-agnostic.
*)
(* End of header comment *)

(** Typed capability keys and registry. *)

type 'a key

(** Create a fresh, globally unique key. [name] is for diagnostics. *)
val create : name:string -> 'a key

(** Register a capability implementation for a key. Overwrites existing. *)
val set : 'a key -> 'a -> unit

(** Alias for [set]. *)
val register : 'a key -> 'a -> unit

(** Retrieve a registered implementation, if any. *)
val get : 'a key -> 'a option

(** Retrieve or fail with a clear message (use during init for fail-fast). *)
val require : 'a key -> 'a

(** Whether a key has an implementation. *)
val mem : 'a key -> bool

(** Reset the registry (tests). *)
val clear : unit -> unit

(** List registered keys (name, present?). *)
val list : unit -> (string * bool) list

(** Existential key for validation helpers. *)
type any = Any : 'a key -> any

(** Pack a key as an existential. *)
val any : 'a key -> any

(** Check that a set of capabilities is registered; returns names missing. *)
val check_all : any list -> string list
