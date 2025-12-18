(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Baker highwatermarks reading.

    Reads the highwatermarks file from the baker's base directory
    to display the last signed block/attestation/preattestation levels. *)

(** Highwatermark entry for a delegate *)
type highwatermark = {round : int; level : int}

(** Per-delegate activity *)
type delegate_activity = {
  delegate : string;
  last_block : highwatermark option;
  last_preattestation : highwatermark option;
  last_attestation : highwatermark option;
}

(** Get cached highwatermarks for a baker instance (never blocks on I/O).
    Returns a list of delegate activities. *)
val get : instance:string -> delegate_activity list

(** Refresh highwatermarks cache for an instance (does file I/O).
    Called by the background scheduler. *)
val refresh : instance:string -> unit

(** Clear cache entry when instance is removed. *)
val clear : instance:string -> unit

(** Deprecated: use [get] instead. *)
val read : instance:string -> delegate_activity list

(** Get the maximum level from a delegate's activity. *)
val max_level : delegate_activity -> int option

(** Format a short summary for display.
    Returns None if no activities. *)
val format_summary : delegate_activity list -> string option
