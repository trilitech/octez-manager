(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Background metrics recorder for diagnostics *)

type duration = OneMin | FiveMin | FifteenMin

type sample = {
  timestamp : float;
  bg_queue_depth : int;
  services_active : int;
  services_total : int;
}

type stats = {
  max_bg_queue : int;
  avg_bg_queue : float;
  current_services_active : int;
  current_services_total : int;
}

(** Check if recorder is running *)
val is_enabled : unit -> bool

(** Get current recording duration setting *)
val get_duration : unit -> duration

(** Set recording duration (resizes buffer, preserves recent samples) *)
val set_duration : duration -> unit

(** Start background recording (samples every 5 seconds) *)
val start : unit -> unit

(** Stop background recording *)
val stop : unit -> unit

(** Clear all recorded samples *)
val clear : unit -> unit

(** Get all recorded samples in chronological order *)
val get_samples : unit -> sample list

(** Get statistics summary *)
val get_stats : unit -> stats option

(** Convert duration to human-readable string *)
val duration_to_string : duration -> string

(** Convert duration to short string *)
val duration_to_short_string : duration -> string

(** All available durations *)
val all_durations : duration list
