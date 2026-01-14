(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)
type page = (module Tui_page.PAGE_SIG)

val register : string -> page -> unit

val register_once : string -> page -> bool

val find : string -> page option

val list : unit -> (string * page) list

val exists : string -> bool

val unregister : string -> unit

val list_names : unit -> string list

(** Register a page via a thunk, created on first use. Useful to avoid heavy deps at load time. *)
val register_lazy : string -> (unit -> page) -> unit

(** Override an existing page registration. *)
val override : string -> page -> unit

(** Check for key conflicts between all registered pages.
    Returns a list of (key_string, page_names) where multiple pages handle the same key. *)
val check_all_conflicts : unit -> (string * string list) list

(** Get a human-readable report of key conflicts, if any exist.
    Returns None if no conflicts, Some report_string if conflicts found. *)
val conflict_report : unit -> string option

(* Application-specific helpers (e.g. storing a last selected instance) were
	intentionally removed from the core registry to keep the core library
	generic. Implement these helpers in application code if needed. *)

(* Small helpers for passing a selected instance name between pages. *)
(* Application-specific selection helpers were removed from the public core
	interface. Applications that need this behavior should use their own
	modules (for example `App_specific.Registry`). *)
